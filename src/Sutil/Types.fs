namespace Sutil

open Browser.Types

/// Helper type for general DOM event handlers
type DomEventHandler = Event -> unit

type Globals =
    static let _globalNextId = Helpers.createIdGenerator ()
    static member NextId : unit -> int = _globalNextId

type NodeRange =
    NodeRange of Browser.Types.Node[]
    with
        static member Of (nodes : Browser.Types.Node[]) = NodeRange nodes
        static member Of (node : Browser.Types.Node) =
            node |> Array.singleton |> NodeRange.Of
        static member Empty = NodeRange [||]
        member __.Nodes = let (NodeRange nodes) = __ in nodes
        member __.Length = __.Nodes.Length
        member __.IsEmpty = __.Length = 0
        member __.IsSingleNode = __.Length = 1
        member __.NodeOrNull = if __.Length = 1 then __.Nodes[0] else null

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
        sprintf "[%A,%s]" (__.Result) (Sutil.Internal.Node.toStringSummary __.Node)

type VirtualElementType =
    | NullNode
    | TextNode of string
    | TagNode of string * string

and VirtualElement =
    {
        /// Only set when this element has been rendered into the DOM
        Key: string
        
        Type: VirtualElementType
        Children: VirtualElement[]
        Attributes: (string * obj)[]
        Events: (string * (Browser.Types.Event -> unit) * Internal.CustomEvents.EventOption[])[]
        Mapper : (BuildContext -> BuildContext) option
    }

/// BuildContext provides context for building SutilElements.
and BuildContext =
    {
        Id : int

        /// Return new ID for next DOM element
        NextId: (unit -> int)

        /// The parent for the current SutilElement
        Parent: Node

        /// How to add a new DOM node into the DOM
        AppendNode: Node -> Node -> unit

        /// The current node that this SutilElement is replacing or patching
        Current: NodeRange

        /// Create DOM element
        ElementCtor: string * string -> Element

        /// Notify new Node created
        OnImportedNode: (Node -> unit) option

        // /// Observers / mutators for VirtualElement created from SutilElement
        // VirtualElementMapper: VirtualElementMapper

        LogElementEnabled: bool
        LogPatchEnabled: bool
    }

    static member DefaultAppendNode = Internal.DomEdit.append
    static member Create(parent: Node) : BuildContext =
        {
            Id = Globals.NextId()
            Parent = parent
            NextId = Globals.NextId
            AppendNode = BuildContext.DefaultAppendNode
            Current = NodeRange.Empty
            OnImportedNode = None
            ElementCtor = Sutil.Internal.DomEdit.elementNS
            LogElementEnabled = false
            LogPatchEnabled = false
        }

    member __.ParentNode = __.Parent

    member __.CreateElement(ns: string, tag: string) : Element = __.ElementCtor (ns,tag)

    member __.ParentElement = __.Parent :?> HTMLElement

    member __.WithLogPatchEnabled() =
        { __ with
            LogPatchEnabled = true
            Id = Globals.NextId()
        }

    member __.WithLogElementEnabled() =
        { __ with
            LogElementEnabled = true
            Id = Globals.NextId()
        }

    member __.WithLogEnabled() =
        __.WithLogElementEnabled().WithLogPatchEnabled()

    member __.WithParent(node: Node) =
        if isNull node then
            failwith "Parent is null"

        { __ with
            Parent = node
            Id = Globals.NextId()
        }

    member __.WithAppendNode(append: Node -> Node -> unit) =
        { __ with
            AppendNode = append
            Id = Globals.NextId()
        }

    member __.WithCurrent(nodes: NodeRange) =
        { __ with
            Current = nodes
            Id = Globals.NextId()
        }

    member __.WithCurrent(nodes: Node[]) =
        { __ with
            Current = NodeRange.Of nodes
            Id = Globals.NextId()
        }

    member __.WithCurrent(node: Node) =
        { __ with
            Current = NodeRange.Of node
            Id = Globals.NextId()
        }

    member __.WithOnImportedNode(f: Node -> unit) =
        { __ with
            Id = Globals.NextId()
            OnImportedNode =
                __.OnImportedNode 
                |> Option.map (fun f0 ->
                    fun node ->
                        f node
                        f0 node)
                |> Option.orElse (Some f)
        }

    member __.NotifyNodeImported (node : Node) =
        __.OnImportedNode |> Option.iter (fun f -> f node)

and VirtualElementMapper = VirtualElement -> VirtualElement

and SutilBindEffect = string * SutilElement * (BuildContext -> unit)

and SutilElement =

    /// Text node
    | Text of string

    /// Element node with a given tag, and children. Children are SutilElements
    /// and so will include Elements, Texts, Attributes, Events, Fragments and SideEffects
    | Element of (string * string * SutilElement[])

    /// Attribute
    | Attribute of (string * obj)

    /// Event listener
    | Event of (string * DomEventHandler * Internal.CustomEvents.EventOption[])

    /// Collection of SutilElements that apply to the parent element
    | Fragment of (SutilElement[])

    /// Custom element that will manage a sub-element at this DOM location.
    /// An initial element is created and the effect is called when the initial element 
    /// is mounted
    /// Eg Bind.el, Html.parse
    | BindElement of SutilBindEffect

    | MappingElement of (string * (BuildContext -> BuildContext) * SutilElement)

    with
        override __.ToString() =
            match __ with
            | Text s -> "Text '" + s + "'"
            | Element (ns, tag, children) -> "Element '" + tag + "' [" + (children |> Array.map _.ToString() |> String.concat ", ") + "]"
            | Attribute (name,value) -> "Attr '" + name + "'='" + (string value) + "'"
            | Event (name,_,_) -> "Event '" + name + "'"
            | Fragment (children) -> "Fragment [" + (children |> Array.map _.ToString() |> String.concat ", ") + "]"
            | BindElement (name,_,_) -> "Bind '" + name + "'"
            | MappingElement (name, _, child) -> "Map '" + name + "' [" + child.ToString() + "]"

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
        SutilElement.Element("", tag, children |> Seq.toArray)

    let elns (ns : string) (tag: string) (children: SutilElement seq) =
        SutilElement.Element(ns, tag, children |> Seq.toArray)

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
