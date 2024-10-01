
module VirtualDom 

open CoreTypes

type EventHandler = (Browser.Types.Event -> unit)

let [<Literal>] TextTag = "[text]"

/// VirtualDom element. Cheap and nasty.
type Element = {
    Tag : string
    Text : string
    Children : Element[]
    Attributes : (string * string) []
    Events : (string * EventHandler) []
    SideEffects : (BuildContext -> unit) []
    Mappers : (Browser.Types.Node -> Browser.Types.Node) []
}
with
    static member Empty = { Tag = ""; Text = ""; Children = [||]; Attributes = [||]; Events = [||]; SideEffects = [||]; Mappers = [||] }
    static member TextNode( s : string ) = { Element.Empty with Tag = TextTag; Text = s }
    static member ElementNode( tag : string ) = { Element.Empty with Tag = tag; }
    member __.AddChild( ch : Element ) = { __ with Children = Array.singleton ch |> Array.append __.Children }
    member __.AddAttr( name, value ) = { __ with Attributes = Array.singleton (name,value) |> Array.append __.Attributes }
    member __.AddEvent( name, handler ) = { __ with Events = Array.singleton (name,handler) |> Array.append __.Events }
    member __.AddEffect( effect ) = { __ with SideEffects = Array.singleton (effect) |> Array.append __.SideEffects }
    member __.AddMapper( m ) = { __ with Mappers = Array.singleton (m) |> Array.append __.Mappers }
    member __.IsTextNode = __.Tag = TextTag
    member __.IsElementNode = __.Tag <> TextTag && __.Tag <> ""
    override __.ToString() = 
        if __.IsTextNode then "<text>" + __.Text + "</text>"
        else 
            "<" + __.Tag + ">" + 
                "..." + 
                "</" + __.Tag + ">"


let private addAttr name value (e : Element) = e.AddAttr(name, value)
let private addClass cls (e : Element) = addAttr "class" cls e
let private addStyle  style (e : Element)= addAttr "style" style e

let private emptyDiv() = 
    Element.ElementNode("div")

let private invisibleDiv() = 
    emptyDiv().AddAttr("style", "display:none")

/// Apply the SutilElement to the parent VirtualDom element
let rec addSutilElement (parent : Element) ( se : SutilElement ) : Element =
    match se with
    | Text text -> 
        parent.AddChild (Element.TextNode text)
    | Element  (tag, children) ->
        children |> Array.fold addSutilElement (Element.ElementNode tag) |> parent.AddChild
    | Fragment (children) ->
        children |> Array.fold addSutilElement parent
    | Attribute (name, value) ->
        parent.AddAttr(name,value)
    | Event (name, handler) ->
        parent.AddEvent(name,handler)
    | SideEffect f -> 
        parent.AddEffect f
    | MapElement (map, _se) -> 
        _se |> fromSutil |> _.AddMapper(map) |> parent.AddChild

/// Create a VirtualDom element from a SutilElement. 
/// This function will also be able to hoist fragment children up into the parent element
and fromSutil (se : SutilElement) : Element =
    let root = addSutilElement (emptyDiv()) se

    let noAttributes = root.Attributes.Length = 0 && root.Events.Length = 0 && root.SideEffects.Length = 0

    let el = 

        // A regular single element, and no sibling attributes. The most common situation: an element
        // Eg   div [ ... ]
        // Note also that fragment [ div [ ... ] ] would bring us here too
        if root.Children.Length = 1 && noAttributes then   
            root.Children[0]

        // No elements found, and no attributes (and events etc) given
        // Eg fragment []
        elif root.Children.Length = 0 && noAttributes then 
            invisibleDiv()

        // Every other case. We have a non-empty fragment 
        // Eg 
        // fragment [ div [] div [] ... ]
        // fragment [ div [] Attr.xx  ... ]
        // fragment [ Attr.xx ... ]
        //
        else  
            root |> addClass "fragment"

    if not (el.IsElementNode) && not (el.IsTextNode) then
        failwith "Not an element or a text node"

    el

/// Create a DOM element from a VirtualDom element
let rec toDom (context : BuildContext) (ve : Element) =
    match ve.Tag with
    | _ when ve.IsTextNode ->
        DomHelpers.text ve.Text
    | "" -> failwith "Invalid tag"
    | tag ->
        let el = DomHelpers.element tag

        DomHelpers.Id.setId el (context.MakeId() |> string)

        ve.Attributes |> Array.iter (fun (name,value) -> el.setAttribute(name,value) |> ignore )
        ve.Events |> Array.iter (fun (name,h) -> DomHelpers.EventListeners.add el name h)
        ve.Children |> Array.iter (fun child -> DomHelpers.append el (toDom context child))
        ve.SideEffects |> Array.iter (fun effect -> effect( context.WithParent(el) ))
        ve.Mappers |> Array.fold (fun el map -> map el) el