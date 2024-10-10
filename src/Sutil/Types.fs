namespace Sutil

open Browser.Types

/// Helper type for general DOM event handlers
type DomEventHandler = Event -> unit

type PatchResult =
    | AttrSet
    | AttrRemoved
    | EventAdded
    | EventRemoved
    | TextSet
    | ChildResult of SutilResult
    | EffectResult of SutilResult

and SutilResultType =
    /// The current node was replaced with a new one
    | Replaced

    /// A new node was added (current node did not exist)
    | Appended

    /// A custom effect was applied
    | Effected of string

    /// Current node was removed
    | Removed

    /// The current node was updated (attributes, events, children)
    | Patched of (PatchResult[])

    /// The current node was unchanged
    | Unchanged

and SutilResult =
    | SutilResult of SutilResultType * Node

    static member Of(result, node) = SutilResult(result, node)
    member __.Node = let (SutilResult(_, node)) = __ in node
    member __.Result = let (SutilResult(result, _)) = __ in result

    override __.ToString() : string =
        sprintf "[%A,%s]" (__.Result) (Sutil.Internal.DomHelpers.toStringSummary __.Node)

type VirtualElementType =
    | NullNode
    | TextNode of string
    | TagNode of string
    | SideEffectNode of SutilEffect

and VirtualElement =
    {
        /// Only set when this element has been rendered into the DOM
        Key: string option
        
        Type: VirtualElementType
        Children: VirtualElement[]
        Attributes: (string * obj)[]
        Events: (string * (Browser.Types.Event -> unit) * Internal.CustomEvents.EventOption[])[]
    }

/// BuildContext provides context for building SutilElements.
and BuildContext =
    {
        /// Return new ID for next DOM element
        NextId: (unit -> int)

        /// The parent for the current SutilElement
        Parent: Node

        /// How to add a new DOM node into the DOM
        AppendNode: Node -> Node -> unit

        /// The current node that this SutilElement is replacing or patching
        Current: Node

        /// Create DOM element
        ElementCtor: string -> HTMLElement

        /// Notify new Node created
        OnImportedNode: Node -> unit

        /// Observers / mutators for VirtualElement created from SutilElement
        VirtualElementMapper: VirtualElementMapper

        LogElementEnabled: bool
        LogPatchEnabled: bool
    }

    static member GlobalNextId = Helpers.createIdGenerator ()
    static member DefaultAppendNode = Internal.DomEdit.append

    static member Create(parent: Node) : BuildContext =
        {
            Parent = parent
            NextId = BuildContext.GlobalNextId
            AppendNode = BuildContext.DefaultAppendNode
            Current = null
            VirtualElementMapper = id
            OnImportedNode = ignore
            ElementCtor = Sutil.Internal.DomEdit.element
            LogElementEnabled = false
            LogPatchEnabled = false
        }

    member __.ParentNode = __.Parent

    member __.CreateElement(tag: string) : HTMLElement = __.ElementCtor tag

    // member __.WithParentId( id : string ) =
    //     __.WithParent( Browser.Dom.document.getElementById(id) )

    member __.ParentElement = __.Parent :?> HTMLElement

    member __.WithLogPatchEnabled() =
        { __ with
            LogPatchEnabled = true
        }

    member __.WithLogElementEnabled() =
        { __ with
            LogElementEnabled = true
        }

    member __.WithLogEnabled() =
        __.WithLogElementEnabled().WithLogPatchEnabled()

    member __.WithParent(node: Node) =
        if isNull node then
            failwith "Parent is null"

        { __ with
            Parent = node
        }

    member __.WithAppendNode(append: Node -> Node -> unit) =
        { __ with
            AppendNode = append
        }

    member __.WithCurrent(node: Node) =
        { __ with
            Current = node
        }

    member __.WithVirtualElementMapperPost(p: VirtualElementMapper) =
        { __ with
            VirtualElementMapper = __.VirtualElementMapper >> p
        }

    member __.WithVirtualElementMapperPre(p: VirtualElementMapper) =
        { __ with
            VirtualElementMapper = __.VirtualElementMapper << p
        }

    // member __.WithElementCtor( create : string -> HTMLElement ) =
    //             { __ with ElementCtor = create }

    member __.WithOnImportedNode(f: Node -> unit) =
        { __ with
            OnImportedNode =
                fun node ->
                    f node
                    __.OnImportedNode node
        }

and VirtualElementMapper = VirtualElement -> VirtualElement

/// Implementation of a SutilElement.SideEffect
and SutilEffect = string * (BuildContext -> SutilResult)

and SutilBindEffect = string * (BuildContext -> unit)

and SutilElement =

    /// Text node
    | Text of string

    /// Element node with a given tag, and children. Children are SutilElements
    /// and so will include Elements, Texts, Attributes, Events, Fragments and SideEffects
    | Element of (string * SutilElement[])

    /// Attribute
    | Attribute of (string * obj)

    /// Event listener
    | Event of (string * DomEventHandler * Internal.CustomEvents.EventOption[])

    /// Collection of SutilElements that apply to the parent element
    | Fragment of (SutilElement[])

    /// Custom element that operates on a BuildContext. Bindings are SideEffects, for example
    | SideEffect of SutilEffect

    /// Custom element that will a sub-element at this DOM location.
    /// Eg Bind.el, Html.parse
    | BindElement of SutilBindEffect

type 'T observable = System.IObservable<'T>

///  <exclude />
type IReadOnlyStore<'T> =
    inherit System.IObservable<'T>
    inherit System.IDisposable
    abstract Value: 'T
    abstract OnDispose: (unit -> unit) -> unit

type IStore<'T> =
    inherit IReadOnlyStore<'T>
    abstract Update: f: ('T -> 'T) -> unit
    abstract Name: string with get, set

module Basic =

    let el (tag: string) (children: SutilElement seq) =
        SutilElement.Element(tag, children |> Seq.toArray)

    let fragment (children: SutilElement seq) =
        SutilElement.Fragment(children |> Seq.toArray)

    let text (s: string) = SutilElement.Text(s)

    let attr (name: string) (value: obj) = SutilElement.Attribute(name, value)

    let event (event: string) (handler: Browser.Types.Event -> unit) =
        SutilElement.Event(event, handler, Array.empty)

    let eventWithOptions
        (event: string)
        (handler: Event -> unit)
        (options: Internal.CustomEvents.EventOption[])
        =
        SutilElement.Event(event, handler, options)
