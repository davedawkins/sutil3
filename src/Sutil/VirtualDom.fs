module Sutil.VirtualDom 

open CoreTypes

type EventHandler = (Browser.Types.Event -> unit)

type NodeType =
    | NullNode

    | TextNode of string
    | TagNode of string

    /// Hooks for things like bindings, which will usually subscribe to some event
    /// and then use the BuildContext to make some change to the DOM.
    /// They may also just make a one-time change and exit. 
    | SideEffectNode of SideEffect

type Element = {
    Type : NodeType
    Children : Element[]
    Attributes : (string * string) []
    Events : (string * EventHandler) []
}
with
    static member Empty = { Type = NullNode; Children = Array.empty; Attributes = Array.empty; Events = Array.empty }

    static member TextNode( s : string ) = { Element.Empty with Type = TextNode s }
    static member ElementNode( tag : string ) = { Element.Empty with Type = TagNode tag; }
    static member SideEffectNode( effect : SideEffect ) = { Element.Empty with Type = SideEffectNode (effect); }
    // static member MapperNode( name, mapper ) = { Element.Empty with Type = MapperNode (name,mapper); }

    member __.IsTextNode = match __.Type with TextNode _ -> true | _ -> false
    member __.IsElementNode = match __.Type with TagNode _ -> true | _ -> false
    member __.IsEffectNode = match __.Type with SideEffectNode _ -> true | _ -> false

    member __.AddChild( ch : Element ) = { __ with Children = Array.singleton ch |> Array.append __.Children }
    member __.AddAttr( name, value ) = { __ with Attributes = Array.singleton (name,value) |> Array.append __.Attributes }
    member __.AddEvent( name, handler ) = { __ with Events = Array.singleton (name,handler) |> Array.append __.Events }
    member __.AddEffect( effect : SideEffect ) = __.AddChild( Element.SideEffectNode(effect) )

    member __.Tag =
        match __.Type with
        | NullNode -> "#null#"
        | TextNode _ -> "#text#"
        | TagNode tag -> tag
        | SideEffectNode (name, _) -> sprintf "#%s#" name

    member __.InnerText =
        let rec inner (e : Element) =
            match e.Type with
            | TextNode s -> s
            | _ -> e.Children |> Array.map inner |> String.concat ""
        inner __

    member __.AsString( children ) = 
        let attrs =
            if __.Attributes.Length = 0 
                then "" 
                else " " + (__.Attributes |> Array.map (fun (n,v) -> sprintf "%s=\"%s\"" n v) |> String.concat " ")

        match __.Type with
        | NullNode -> "<null/>"
        | TextNode s ->  s
        | TagNode tag -> 
            "<" + tag + attrs + ">" + children + "</" + tag + ">"
        | SideEffectNode (tag,_) ->
            if children = "" then
                sprintf "<%s%s/>" tag attrs
            else
                sprintf "<%s%s>%s</%s>" tag attrs children tag

    override __.ToString() = 
        __.AsString( __.Children |> Array.map _.ToString() |> String.concat "" )

    member __.ToTagWithInnerText() = 
        __.AsString( __.InnerText )
    
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
    | SideEffect effect -> 
        parent.AddEffect( effect )

/// Create a VirtualDom element from a SutilElement. 
/// This function will also be able to hoist fragment children up into the parent element
and fromSutil (se : SutilElement) : Element =
    let root = addSutilElement (emptyDiv()) se

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
            invisibleDiv()

        // Every other case. We have a non-empty fragment 
        // Eg 
        // fragment [ div [] div [] ... ]
        // fragment [ div [] Attr.xx  ... ]
        // fragment [ Attr.xx ... ]
        //
        else  
            root |> addClass "fragment"

    if not (el.IsElementNode) && not (el.IsTextNode) && not (el.IsEffectNode) then
        failwith "Not an element or a text node"

    el

/// Create a DOM element from a VirtualDom element
let rec toDom (context : BuildContext) (ve : Element) : Browser.Types.Node  =
    match ve.Type with
    | NullNode -> failwith "Cannot create DOM node from null node"

    | TextNode s -> DomHelpers.text s

    | TagNode tag ->
        let el = DomHelpers.element tag

        DomHelpers.Id.setId el (context.MakeId() |> string)

        ve.Attributes |> Array.iter (fun (name,value) -> el.setAttribute(name,value) |> ignore )
        ve.Events |> Array.iter (fun (name,h) -> DomHelpers.EventListeners.add el name h)

        ve.Children 
        |> Array.iter (fun child -> 
            let childEl : Browser.Types.Node = toDom (context.WithParent(el)) child

            // Maybe toDom should return an enum to be very specific about what
            // happened?
            if not (childEl.isSameNode(el)) then
                DomHelpers.append el  childEl
        )
        el

    | SideEffectNode (name, effect) ->
        (effect context).Node
