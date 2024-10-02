
module Sutil.DomHelpers

open Browser.Types
open Browser.Dom
open Browser.CssExtensions

let private _log (s : string) = ()

[<Literal>]
let internal ElementNodeType = 1.0

[<Literal>]
let internal TextNodeType = 3.0

module JsMap =
    open Fable.Core.JsInterop
    open Fable.Core

    let inline getKey (node : obj) (name : string) = node?(name)

    [<Emit("delete $0[$1]")>]
    let deleteKey (node : obj) (key : string) = jsNative
        
    let hasKey (node : obj) (key : string) =
        jsIn key node

    let setKey  (node : obj)(key : string)  (value : obj) : unit =
        node?(key) <- value

    let tryGetKey  (node : obj)(key : string)  : 't option = 
        if hasKey node key then
            node?(key) |> Some
        else
            None

    let getKeyWith  (node : obj)(key : string)  (defaultValue : 't) : 't = 
        tryGetKey node key |> Option.defaultValue defaultValue

module NodeKey =

    open Fable.Core.JsInterop
    open Fable.Core

    [<Emit("delete $0[$1]")>]
    let deleteKey (key : string) (node : Node) = jsNative
        
    let hasKey (key : string) (node : Node) =
        jsIn key node

    let setKey (key : string)  (node : Node) (value : obj) : unit =
        node?(key) <- value

    let tryGetKey (key : string)  (node : Node) : 't option = 
        if hasKey key node then
            node?(key) |> Some
        else
            None

    let getKeyWith (key : string)  (node : Node) (defaultValue : 't) : 't = 
        tryGetKey key node |> Option.defaultValue defaultValue

module Id =
    open NodeKey

    let [<Literal>] Id = "__sutil_id"

    let setId (node : Node) (x: string) = 
        setKey Id node x
    
    let getId (node : Node) =
        getKeyWith Id node "" 
    

    let nodeStrShort (node: Node) =
        let svId node = getId node
        
        if isNull node then
            "null"
        else
            let mutable tc = node.textContent

            if tc.Length > 16 then
                tc <- tc.Substring(0, 16) + "..."

            match node.nodeType with
            | ElementNodeType ->
                let e = node :?> HTMLElement
                $"<{e.tagName.ToLower()}> #{svId node}"
            | TextNodeType -> $"text:\"{tc}\" #{svId node}"
            | _ -> $"?'{tc}'#{svId node}"

module CustomEvents =
    open Fable.Core

    module CustomEvent =
        let [<Literal>] Mount = "sutil-mount"
        let [<Literal>] Unmount = "sutil-unmount"
        let [<Literal>] Show = "sutil-show"
        let [<Literal>] Hide = "sutil-hide"
        let [<Literal>] Updated = "sutil-updated"
        let [<Literal>] Connected = "sutil-connected"

    [<Emit("new CustomEvent($0, $1)")>]
    let customEvent name data = jsNative

    let private dispatch (target: EventTarget) name (data: obj) =
        if not (isNull target) then
            target.dispatchEvent (customEvent name data)
            |> ignore

    let internal dispatchSimple (target: EventTarget) name =
        dispatch target name {|  |}

    let private dispatchCustom<'T> (target: EventTarget) (name: string) (init: CustomEventInit<'T>) =
        if not (isNull target) then
            target.dispatchEvent (customEvent name init)
            |> ignore

    /// <summary>
    /// Custom events
    /// </summary>
    type CustomDispatch<'T> =
        | Detail of 'T
        | Bubbles of bool
        | Composed of bool
        static member toCustomEvent<'T>(props: CustomDispatch<'T> list) =
            let mutable data: obj = upcast {|  |}

            for p in props do
                match p with
                | Detail d ->JsMap.setKey data "detail" d
                | Bubbles b ->JsMap.setKey data "bubbles" b
                | Composed c ->JsMap.setKey data "composed" c

            data :?> CustomEventInit<'T>

        static member dispatch(target: EventTarget, name: string) =
            dispatchCustom<unit> target name (CustomDispatch.toCustomEvent<unit> ([]))

        static member dispatch(e: Event, name: string) =
            dispatchCustom<unit> (e.target) name (CustomDispatch.toCustomEvent<unit> ([]))

        static member dispatch<'T>(target: EventTarget, name: string, props: CustomDispatch<'T> list) =
            dispatchCustom<'T> target name (CustomDispatch.toCustomEvent<'T> props)

        static member dispatch<'T>(e: Event, name: string, props: CustomDispatch<'T> list) =
            dispatchCustom<'T> (e.target) name (CustomDispatch.toCustomEvent<'T> props)

        static member dispatch (target: EventTarget, name, data: 'T) =
            dispatchCustom<'T> (target) name (CustomDispatch.toCustomEvent<unit> ([ Detail data ]))


let children (node: Node) =
    let rec visit (child: Node) =
        seq {
            if not (isNull child) then
                yield child
                yield! visit child.nextSibling
        }

    if isNull node then Seq.empty else visit node.firstChild

let rec descendants (node: Node) =
    seq {
        for child in children node do
            yield child
            yield! descendants child
    }

let rec internal descendantsDepthFirst (node: Node) =
    seq {
        for child in children node do
            yield! descendants child
            yield child
    }

module Dispose =
    open NodeKey
    let [<Literal>] Disposables = "__sutil_ds"

    let makeDisposable (f : unit -> unit) =
        { new System.IDisposable with member _.Dispose() = f() }

    let private hasDisposables (node: Node) : bool = 
        hasKey (Disposables) node

    let private getDisposables (node: Node) : System.IDisposable[] =
        getKeyWith Disposables node Array.empty

    let private clearDisposables (node: Node) : unit = 
        deleteKey Disposables node 

    let setDisposables (node : Node) (ds : System.IDisposable[]) =
        setKey (Disposables) node ds

    let addDisposable (node : Node) (d : System.IDisposable) =
        Array.singleton d
        |> Array.append (getDisposables node)
        |> setDisposables node

    let addSimple( node : Node) (f : unit -> unit) =
        f |> makeDisposable |> addDisposable node

    let rec internal dispose (node : Node) = 

        descendantsDepthFirst node
        |> Array.ofSeq
        |> Array.iter dispose

        CustomEvents.dispatchSimple node CustomEvents.CustomEvent.Unmount

        let safeDispose (d : System.IDisposable) = 
            try d.Dispose() with x -> _log (sprintf "Error while disposing: %s" x.Message)

        getDisposables node |> Array.iter safeDispose
        clearDisposables node

let internal isConnected (node: Node) : bool = JsMap.getKey node "isConnected"

/// Return true if n is a Text node (nodeType = 3)
let isTextNode (n: Node) = n <> null && n.nodeType = TextNodeType

/// Return true if n is an Element node (nodeType = 1)
let isElementNode (n: Node) = n <> null && n.nodeType = ElementNodeType

let asEl<'T when 'T :> HTMLElement> (n : Node) : 'T = n :?> 'T
let asElement (n : Node) : HTMLElement = n :?> HTMLElement

let elementTag (n : Node) = if isElementNode n then (asElement n).tagName else ""

let findElement (doc : Document) selector = doc.querySelector(selector)

let remove (node : Node) = 
    Dispose.dispose node
    if not (isNull node.parentNode) then
        node.parentNode.removeChild(node) |> ignore

let append (parent : Node) (node : Node) = 
    // Log.Console.log("append: parent=", Id.getId parent, " child=", Id.getId node )
    // Log.Console.log(parent, node )
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

/// Remove all children of this node, cleaning up Sutil resources and dispatching "unmount" events
let clear (e : Node) =
    e |> children |> Seq.toArray |> Array.iter remove

let text s : Node = document.createTextNode s

let element tag = document.createElement tag

let invisibleElement tag =
    let e = element tag
    e.style.display <- "none"
    e

module EventListeners =

    let [<Literal>] Listeners = "__sutil_ev"

    let empty : (string * (Event -> unit)) [] = Array.empty

    let add (node : Node) (event : string) (f : Event -> unit)  =
        Array.singleton (event,f) |>
        Array.append (NodeKey.getKeyWith Listeners node empty) |>
        NodeKey.setKey Listeners node
        node.addEventListener(event, f)

    let clear (node : Node) =
        NodeKey.getKeyWith Listeners node empty
        |> Array.iter (fun (name, f) -> node.removeEventListener(name,f))
        NodeKey.deleteKey Listeners node

