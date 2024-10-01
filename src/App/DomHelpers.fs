
module DomHelpers

open Browser.Types
open Browser.Dom
open Browser.CssExtensions


module SutilKeys =

    open Fable.Core.JsInterop

    let [<Literal>] Id = "__sutil_id"

    let hasKey (key : string) (node : Node) =
        jsIn key node

    let setKey (key : string)  (node : Node) (value : obj) : unit =
        Fable.Core.JS.console.log("setKey: ", key, " = ", value )
        node?(key) <- value
        Fable.Core.JS.console.log("setKey: ", node )

    let getKey (key : string)  (node : Node) : obj = 
        node?(key)

    let setId (node : Node) (x: string) = 
        setKey Id node x
    
    let getId (node : Node) = 
        if hasKey Id node then
            getKey Id node |> string
        else    
            ""
    
[<Literal>]
let internal ElementNodeType = 1.0

[<Literal>]
let internal TextNodeType = 3.0

let dispose (n : Node) = 
    ()

/// Return true if n is a Text node (nodeType = 3)
let isTextNode (n: Node) = n <> null && n.nodeType = TextNodeType

/// Return true if n is an Element node (nodeType = 1)
let isElementNode (n: Node) = n <> null && n.nodeType = ElementNodeType

let asEl<'T when 'T :> HTMLElement> (n : Node) : 'T = n :?> 'T
let asElement (n : Node) : HTMLElement = n :?> HTMLElement

let elementTag (n : Node) = if isElementNode n then (asElement n).tagName else ""

let findElement (doc : Document) selector = doc.querySelector(selector)

let children (node: Node) =
    let rec visit (child: Node) =
        seq {
            if not (isNull child) then
                yield child
                yield! visit child.nextSibling
        }

    if isNull node then Seq.empty else visit node.firstChild

let remove (node : Node) = 
    dispose node
    node.parentNode.removeChild(node) |> ignore

let append (parent : Node) (node : Node) = 
    parent.appendChild(node) |> ignore

let appendNodes (parent : Node) (nodes : Node[]) = 
    nodes |> Array.iter (append parent)

let replace (parent : Node) (current : Node) (node : Node) =
    parent.insertBefore( node, current ) |> ignore
    remove current

let insertBefore (parent: Node) (child: Node) (refNode: Node) =
    parent.insertBefore (child, refNode) |> ignore

let insertAfter (parent: Node) (newChild: Node) (refChild: Node) =
    let beforeChild =
        if isNull refChild then
            parent.firstChild
        else
            refChild.nextSibling

    insertBefore parent newChild beforeChild

let replaceNodes (parent : Node) (existing : Node[]) (nodes : Node[]) =
    if existing |> Array.isEmpty then
        appendNodes parent nodes
    else
        let last = existing |> Array.last
        nodes |> Array.iter (fun node -> last.parentNode.insertBefore(node, last.nextSibling) |> ignore)
        existing |> Array.iter remove

let clear (e : HTMLElement) =
    e |> children |> Seq.toArray |> Array.iter remove

let text s : Node = document.createTextNode s

let element tag = document.createElement tag

let invisibleElement tag =
    let e = element tag
    e.style.display <- "none"
    e
