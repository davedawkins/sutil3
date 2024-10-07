namespace Sutil

open Browser.Types

type PatchReference =
    | PatchNode of Node
    | BeforeNode of Node
    | AfterNode of Node

/// Type of function returned from effecting functions, that allow
/// the effect to be reversed. So, a subscribe() function returns a
/// function that unsubscribes; listen() function will return  a
/// function that stops listening, etc.
type Unsubscriber = unit -> unit

/// Helper type for general DOM event handlers
type DomEventHandler = Event -> unit

/// Return value from a SutilElement.SideEffect
type SutilEffectResult =
    /// The side-effect created a new node
    | CreatedNode of Node
    /// The side-effect operated on the DOM in some way
    | DomEffect
    with 
        member __.Node = 
            match __ with CreatedNode node -> Some node | _ -> None

type VirtualElementType =
    | NullNode
    | TextNode of string
    | TagNode of string
    | SideEffectNode of SutilSideEffect

and VirtualElement = {
    Type : VirtualElementType
    Children : VirtualElement[]
    Attributes : (string * string) []
    Events : (string * (Browser.Types.Event -> unit)) []
}

/// BuildContext provides context for building SutilElements. 
and BuildContext = 
    {
        /// Return new ID for next DOM element
        NextId : (unit -> int)

        /// The parent for the current SutilElement
        Parent : Node

        /// How to add a new DOM node into the DOM
        AppendNode : Node -> Node -> unit

        /// The current node that this SutilElement is replacing or patching
        Current : Node 

        /// Create DOM element
        ElementCtor: string -> HTMLElement

        /// Notify new Node created
        OnImportedNode : Node -> unit

        /// Observers / mutators for VirtualElement created from SutilElement
        VirtualElementMapper : VirtualElementMapper

        LogElementEnabled : bool
        LogPatchEnabled : bool
    }

    member __.ParentNode = __.Parent
    
    member __.CreateElement( tag : string ) : HTMLElement =
        __.ElementCtor tag

    // member __.WithParentId( id : string ) = 
    //     __.WithParent( Browser.Dom.document.getElementById(id) )

    member __.ParentElement = __.Parent :?> HTMLElement

    member __.WithLogPatchEnabled() = { __ with LogPatchEnabled = true }
    member __.WithLogElementEnabled() = { __ with LogElementEnabled = true }

    member __.WithLogEnabled() = __.WithLogElementEnabled().WithLogPatchEnabled()

    member __.WithParent( node : Node ) = 
        if isNull node then failwith "Parent is null"
        { __ with Parent = node }

    member __.WithAppendNode( append : Node -> Node -> unit  ) = 
                { __ with AppendNode = append }

    member __.WithCurrent( node : Node ) = 
                { __ with Current = node }

    member __.WithVirtualElementMapperPost( p : VirtualElementMapper ) = 
                { __ with VirtualElementMapper = __.VirtualElementMapper>>p }

    member __.WithVirtualElementMapperPre( p : VirtualElementMapper ) = 
                { __ with VirtualElementMapper = __.VirtualElementMapper<<p }

    // member __.WithElementCtor( create : string -> HTMLElement ) = 
    //             { __ with ElementCtor = create }

    member __.WithOnImportedNode( f : Node -> unit ) = 
                { __ with OnImportedNode = fun node -> f node; __.OnImportedNode node }

and VirtualElementMapper = VirtualElement -> VirtualElement

/// Implementation of a SutilElement.SideEffect
and SutilSideEffect = 
    string * (BuildContext -> SutilEffectResult)

and SutilElement =
    /// Text node
    | Text of string
    /// Element node with a given tag, and children. Children are SutilElements
    /// and so will include Elements, Texts, Attributes, Events, Fragments and SideEffects
    | Element of (string * SutilElement[])
    /// Attribute
    | Attribute of (string * string)
    /// Event listener
    | Event of (string * DomEventHandler)
    /// Collection of SutilElements that apply to the parent element
    | Fragment of (SutilElement[])
    /// Custom element that operates on a BuildContext. Bindings are SideEffects, for example
    | SideEffect of SutilSideEffect

type 'T observable = System.IObservable<'T>

///  <exclude />
type IReadOnlyStore<'T> =
    inherit System.IObservable<'T>
    inherit System.IDisposable
    abstract Value : 'T
    abstract OnDispose: (unit -> unit) -> unit

type IStore<'T> =
    inherit IReadOnlyStore<'T>
    abstract Update : f: ('T -> 'T) -> unit
    abstract Name : string with get, set