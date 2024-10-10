module Sutil.VirtualDom

open Sutil
open Sutil.Internal

type EventHandler = (Browser.Types.Event -> unit)

[<Literal>]
let private CLASS = "class"

let private _log = Log.create ("VirtualDom")

_log.enabled <- false

type VirtualElement with

    static member Empty =
        {   Key = None
            Type = NullNode
            Children = Array.empty
            Attributes = Array.empty
            Events = Array.empty
        }

    static member TextNode(s: string) =
        { VirtualElement.Empty with
            Type = TextNode s
        }

    static member ElementNode(tag: string) =
        { VirtualElement.Empty with
            Type = TagNode tag
        }

    static member SideEffectNode(effect: SutilEffect) =
        { VirtualElement.Empty with
            Type = SideEffectNode(effect)
        }

    member __.GetKey() =
        match __.Key with
        | Some k -> k
        | None -> failwith "Key is not set"

    member __.IsTextNode =
        match __.Type with
        | TextNode _ -> true
        | _ -> false

    member __.IsElementNode =
        match __.Type with
        | TagNode _ -> true
        | _ -> false

    member __.IsEffectNode =
        match __.Type with
        | SideEffectNode _ -> true
        | _ -> false

    member __.IsDomNode = __.IsTextNode || __.IsElementNode

    member __.AsEffect() =
        match __.Type with
        | SideEffectNode(namedEffect) -> namedEffect
        | _ -> failwith "Not an effect"

    member __.DomChildren = __.Children |> Array.filter _.IsDomNode
    member __.EffectChildren = __.Children |> Array.filter _.IsEffectNode

    member __.ChildrenWithDomIndex =
        let mutable i = -1

        [|
            for child in __.Children do
                if child.IsDomNode then
                    i <- i + 1
                    yield (i, child)
                else
                    yield (-1, child)
        |]

    member __.AddChild(ch: VirtualElement) =
        { __ with
            Children = Array.singleton ch |> Array.append __.Children
        }

    member __.AddAttr(name, value: obj) =
        { __ with
            Attributes = Array.singleton (name, value) |> Array.append __.Attributes
        }

    member __.AddEvent(name, handler, ?options: Internal.CustomEvents.EventOption[]) =
        { __ with
            Events =
                Array.singleton (name, handler, options |> Option.defaultValue Array.empty)
                |> Array.append __.Events
        }

    member __.AddEffect(effect: SutilEffect) =
        __.AddChild(VirtualElement.SideEffectNode(effect))

    member __.RemoveAttr(name: string) =
        { __ with
            Attributes = __.Attributes |> Array.filter (fun (aname, _) -> aname <> name)
        }

    member __.ClassList =
        __.Attributes
        |> Array.tryFind (fun (name, _) -> name = CLASS)
        |> Option.map (snd >> string)
        |> Option.defaultValue ""
        |> ClassHelpers.splitBySpace

    member __.SetClassList(classes: string[]) =
        __.RemoveAttr(CLASS).AddAttr(CLASS, classes |> String.concat " ")

    member __.AddClass(cls: string) =
        cls
        |> ClassHelpers.splitBySpace
        |> Array.append __.ClassList
        |> Array.distinct
        |> __.SetClassList

    member __.Tag =
        match __.Type with
        | NullNode -> "#null#"
        | TextNode _ -> "#text#"
        | TagNode tag -> tag
        | SideEffectNode(name, _) -> sprintf "#%s#" name

    member __.InnerText =
        let rec inner (e: VirtualElement) =
            match e.Type with
            | TextNode s -> s
            | _ -> e.Children |> Array.map inner |> String.concat ""

        inner __

    member __.AsString(children) =
        let attrs =
            if __.Attributes.Length = 0 then
                ""
            else
                " "
                + (__.Attributes
                   |> Array.map (fun (n, v) -> sprintf "%s=\"%A\"" n v)
                   |> String.concat " ")

        match __.Type with
        | NullNode -> "<null/>"
        | TextNode s -> s
        | TagNode tag -> "<" + tag + attrs + ">" + children + "</" + tag + ">"
        | SideEffectNode(tag, _) ->
            if children = "" then
                sprintf "<%s%s/>" tag attrs
            else
                sprintf "<%s%s>%s</%s>" tag attrs children tag

    member __.AsString() =
        __.AsString(__.Children |> Array.map _.AsString() |> String.concat "")

    member __.ToTagWithInnerText() = __.AsString(__.InnerText)

let private addAttr name value (e: VirtualElement) = e.AddAttr(name, value)
let private addClass cls (e: VirtualElement) = addAttr "class" cls e
let private addStyle style (e: VirtualElement) = addAttr "style" style e

let private emptyDiv () = VirtualElement.ElementNode("div")

let private invisibleDiv () =
    // Use something other than 'div' for a fragment, because of this case:
    //     if flag then fragment[] else Html.div [ ...; unsubscribeOnUnmount (cb) ]
    // If fragment is a div then it can be patched between the two cases, and the
    // unsubscribe/dispose/mount/unmount handlers are never called
    //
    // Yeah but Dave, when the node is patched, that should be treated like a disposal
    // and then a reconstruction, so the handlers should all be called then
    VirtualElement.ElementNode("sutil-fragment").AddAttr("style", "display:none")

/// Apply the SutilElement to the parent VirtualDom element
let rec addSutilElement (parent: VirtualElement) (se: SutilElement) : VirtualElement =
    match se with
    | Text text -> parent.AddChild(VirtualElement.TextNode text)

    | Element(tag, children) ->
        children
        |> Array.fold addSutilElement (VirtualElement.ElementNode tag)
        |> parent.AddChild

    | Fragment(children) -> children |> Array.fold addSutilElement parent

    | Attribute(name, value) ->
        if name = "class" then
            parent.AddClass(string value)
        else
            parent.AddAttr(name, value)

    | Event(name, handler, options) -> parent.AddEvent(name, handler, options)

    | BindElement(name, handler) ->
        (VirtualElement.ElementNode "div")

        |> fun ve ->
            (if _log.enabled then
                 ve.AddChild(VirtualElement.TextNode("binding:" + name))
             else
                 ve.AddAttr("style", "display:none;"))

        |> _.AddAttr("data-binding", name)

        |> _.AddEvent(
            CustomEvents.MOUNT,
            fun e ->
                let el = e.target.asElement

                let ctx: BuildContext = JsMap.getKey el "__sutil_ctx"

                if isNull (ctx :> obj) then
                    Log.Console.error (
                        "Key '__sutil_ctx' not set on ",
                        el |> DomHelpers.toStringSummary
                    )
                else
                    if _log.enabled then
                        _log.trace (
                            "Mount: init '",
                            name,
                            "'",
                            e.target.asElement |> Internal.DomHelpers.toString
                        )

                    handler (ctx.WithCurrent(el))
        )

        |> parent.AddChild

    | SideEffect effect -> parent.AddEffect(effect)

/// Create a VirtualDom element from a SutilElement.
/// This function will also be able to hoist fragment children up into the parent element
and fromSutil (se: SutilElement) : VirtualElement =
    let root = addSutilElement (emptyDiv ()) se

    let noAttributes = root.Attributes.Length = 0 && root.Events.Length = 0

    let el =

        // A regular single element, and no sibling attributes. The most common situation: an element
        // Eg   div [ ... ]
        // Note also that fragment [ div [ ... ] ] would bring us here too
        if root.Children.Length = 1 && noAttributes then
            root.Children[0]

        // No elements found, and no attributes (and events etc) given
        // Eg fragment []
        elif root.Children.Length = 0 && noAttributes then
            invisibleDiv ()

        // Every other case. We have a non-empty fragment
        // Eg
        // fragment [ div [] div [] ... ]
        // fragment [ div [] Attr.xx  ... ]
        // fragment [ Attr.xx ... ]
        //
        else
            root |> addClass "fragment" |> addAttr "style" "display:contents;"

    if not (el.IsElementNode) && not (el.IsTextNode) && not (el.IsEffectNode) then
        failwith "Not an element or a text node"

    el

/// Create a DOM element from a VirtualDom element
let rec toDom (context: BuildContext) (ve: VirtualElement) : Browser.Types.Node =

    match ve.Type with
    | NullNode -> failwith "Cannot create DOM node from null node"

    | TextNode s ->
        if _log.enabled then
            _log.trace (
                "toDom: TextNode",
                s,
                context.ParentNode |> Internal.DomHelpers.toStringSummary
            )

        DomEdit.text s

    | TagNode tag ->
        if _log.enabled then
            _log.trace (
                "toDom: TagNode",
                "<" + tag + "> parented to",
                context.ParentNode |> Internal.DomHelpers.toStringSummary
            )

        let el = context.CreateElement tag

        let _id = context.NextId()

        if _log.enabled then
            _log.trace ("toDom: -- setSutilId", _id)

        Id.setId el (_id |> string)

        if (ve.Attributes |> Array.exists (fun (name, value) -> name = "data-binding")) then
            if _log.enabled then
                _log.trace ("toDom: -- set __sutil_ctx ", el |> DomHelpers.toStringSummary)

            JsMap.setKey el "__sutil_ctx" context

        ve.Attributes
        |> Array.iter (fun (name, value) ->
            if _log.enabled then
                _log.trace ("toDom: -- setAttribute", "'" + name + "'", value)

            DomEdit.setAttribute el name value
        )

        ve.Events
        |> Array.iter (fun (name, handler, options) ->
            if _log.enabled then
                _log.trace ("toDom: -- addEventListener", "'" + name + "'")

            if
                options
                |> Array.exists (
                    function
                    | Internal.CustomEvents.Once -> true
                )
            then
                EventListeners.once name el handler |> ignore
            else
                EventListeners.add el name handler |> ignore
        )

        ve.Children
        |> Array.iter (fun child ->
            let childEl: Browser.Types.Node =
                if _log.enabled then
                    _log.trace (
                        "toDom: -- addChild to ",
                        el |> Internal.DomHelpers.toStringSummary
                    )

                toDom (context.WithParent(el).WithAppendNode(DomEdit.append)) child

            // Maybe toDom should return an enum to be very specific about what
            // happened?
            if not (childEl.isSameNode (el)) then
                DomEdit.append el childEl
        )

        el

    | SideEffectNode(name, effect) ->
        if _log.enabled then
            _log.trace (
                "toDom: Effect",
                name,
                context.ParentNode |> Internal.DomHelpers.toStringSummary
            )

        (effect context).Node //|> Option.defaultValue context.Parent
