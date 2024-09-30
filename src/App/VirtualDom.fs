
module VirtualDom 

open CoreTypes

type EventHandler = (Browser.Types.Event -> unit)
let [<Literal>] TextTag = "[text]"

type Element = {
    Tag : string
    Text : string
    Children : Element[]
    Attributes : (string * string) []
    Events : (string * EventHandler) []
    SideEffects : (BuildContext -> unit) []
}
with
    static member Empty = { Tag = ""; Text = ""; Children = [||]; Attributes = [||]; Events = [||]; SideEffects = [||] }
    static member TextNode( s : string ) = { Element.Empty with Tag = TextTag; Text = s }
    static member ElementNode( tag : string ) = { Element.Empty with Tag = tag; }
    member __.AddChild( ch : Element ) = { __ with Children = Array.singleton ch |> Array.append __.Children }
    member __.AddAttr( name, value ) = { __ with Attributes = Array.singleton (name,value) |> Array.append __.Attributes }
    member __.AddEvent( name, handler ) = { __ with Events = Array.singleton (name,handler) |> Array.append __.Events }
    member __.AddEffect( effect ) = { __ with SideEffects = Array.singleton (effect) |> Array.append __.SideEffects }
    member __.IsTextNode = __.Tag = TextTag
    member __.IsElementNode = __.Tag <> TextTag && __.Tag <> ""

let rec addSutilElement (parent : Element) ( se : SutilElement ) : Element =
    match se with
    | Text text -> 
        parent.AddChild (Element.TextNode text)
    | Element  (tag, children) ->
        children |> Array.fold addSutilElement (Element.ElementNode tag) |> parent.AddChild
    | Attribute (name, value) ->
        parent.AddAttr(name,value)
    | Event (name, handler) ->
        parent.AddEvent(name,handler)
    | SideEffect f -> 
        parent.AddEffect f

let fromSutil (se : SutilElement) : Element =
    let root = addSutilElement (Element.Empty) se

    if (root.Attributes.Length > 0 || root.Events.Length > 0 || root.SideEffects.Length > 0) then
        failwith ("Cannot set attribute/event/side-effect with no parent")
    
    if (root.Children.Length > 1) then
        failwith "Fragment not supported"

    if (root.Children.Length = 0) then
        failwith "No node or element"

    let el = root.Children[0]

    if not (el.IsElementNode) && not (el.IsTextNode) then
        failwith "Not an element or a text node"

    el

let rec toDom (ve : Element) =
    match ve.Tag with
    | _ when ve.IsTextNode ->
        DomHelpers.text ve.Text
    | "" -> failwith "Invalid tag"
    | tag ->
        let el = DomHelpers.element tag
        ve.Attributes |> Array.iter (fun (name,value) -> el.setAttribute(name,value) |> ignore )
        ve.Events |> Array.iter (fun (name,h) -> el.addEventListener(name,h))
        ve.Children |> Array.iter (fun child -> DomHelpers.append el (toDom child))
        ve.SideEffects |> Array.iter (fun effect -> effect( BuildContext.Create().WithParent(el) ))
        el
