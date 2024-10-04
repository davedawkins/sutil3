
namespace Sutil.Dom

module Types =
    open Fable.Core

    [<Erase>]
    type Unsubscribable =
        Unsubscribe of (unit -> unit)
        with 
            static member Of(f) = Unsubscribe f
            member __.Value = let (Unsubscribe f) = __ in f
            member __.Invoke() = __.Value ()
        
module TypeHelpers =

    open Browser.Types
    open Fable.Core

    [<Literal>]
    let internal ElementNodeType = 1.0

    [<Literal>]
    let internal TextNodeType = 3.0

    /// Return true if n is a Text node (nodeType = 3)
    let isTextNode (n: Node) = n <> null && n.nodeType = TextNodeType

    /// Return true if n is an Element node (nodeType = 1)
    let isElementNode (n: Node) = n <> null && n.nodeType = ElementNodeType

    let asEl<'T when 'T :> HTMLElement> (n : Node) : 'T = n :?> 'T
    let asElement (n : Node) : HTMLElement = n :?> HTMLElement

    let tryAsElement (n : Node) = if isElementNode n then Some (asElement n) else None


module Logging = 
    let error (s) = Sutil.Log.Console.log("Error: " + s)
    let  log (s : string) = Sutil.Log.Console.log("Error: " + s)

module JsHelpers =
    open Fable.Core

    [<Emit("$0 === $1")>]
    let eq3 a b : bool = jsNative

    [<Emit("$0 == $1")>]
    let eq2 a b : bool = jsNative

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

    let arrayAppendKey (node : obj) (key : string) (value : 't) : unit =
        Array.singleton value |>
            Array.append (getKeyWith node key Array.empty) |>
            setKey node key       

    let arrayRemoveKey (node : obj) (key : string) (pred : 't -> bool) : unit =
        getKeyWith node key (Array.empty : 't[])
        |> Array.filter (pred>>not)
        |> setKey node key      

    let getCreate<'T> (node: obj) key (cons: unit -> 'T) : 'T =
        match tryGetKey node key with
        | Some v -> v
        | None ->
            let newVal = cons ()
            setKey node key newVal
            newVal


module NodeKey =

    open Fable.Core.JsInterop
    open Fable.Core
    open Browser.Types

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


module EventListeners =
    open Browser.Types
    open Types

    let [<Literal>] Listeners = "__sutil_ev"

    type NamedHandler =  (string * (Event -> unit))

    let empty : NamedHandler [] = Array.empty

    let add (node : EventTarget) (event : string) (f : Event -> unit) : Unsubscribable =
        JsMap.arrayAppendKey node Listeners (Array.singleton (event,f))

        node.addEventListener(event, f)

        (fun () ->
            node.removeEventListener( event, f )
            JsMap.arrayRemoveKey 
                node 
                Listeners 
                (fun (name,fn) -> JsHelpers.eq3 fn f )) |> Unsubscribable.Of

    let clear (node : Node) =
        NodeKey.getKeyWith Listeners node empty
            |> Array.iter (fun (name, f) -> node.removeEventListener(name,f))
        NodeKey.deleteKey Listeners node

    /// Listen for the given event, and remove the listener after the first occurrence of the evening firing.
    let once (event: string) (target: EventTarget) (fn: Event -> Unit) : unit =
        let mutable remove : Unsubscribable = Unchecked.defaultof<_>

        let rec inner e =
            remove.Invoke()
            fn (e)

        remove <- add (target :?> Node) event inner


module Id =
    open NodeKey
    open Browser.Types
    open TypeHelpers

    let [<Literal>]private  Id = "__sutil_id"
    let [<Literal>]private  NodeMap = "__sutil_nodemap"

    let internal getNodeMap (doc: Document) : obj =
        JsMap.getCreate doc.body NodeMap (fun () -> upcast {|  |})

    let setId (node : Node) (x: string) = 
        setKey Id node x

        let map = getNodeMap node.ownerDocument
        JsMap.setKey map (string id) node
    
    let getId (node : Node) =
        getKeyWith Id node "" 
    
    let internal findNodeWithId (doc: Document) id : Node option =
        let map = getNodeMap doc
        let key = string id

        match JsMap.hasKey map key with
        | true -> Some(JsMap.getKey map key)
        | _ -> None

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
    open Browser.Types

    let [<Literal>] ElementReady = "sutil-element-ready"
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

module DomHelpers =
    open TypeHelpers
    open Browser.Types
    open Browser.Dom

    let outerHTML (n : Node) = 
        n |> tryAsElement 
        |> Option.map _.outerHTML 
        |> Option.defaultValue (if isNull n then "<null node>" else n.textContent)

    let elementTag (n : Node) = if isElementNode n then (asElement n).tagName else ""

    let private ifNotNull (f : 't -> unit) (n : 't) =
        if not (isNull n) then f n

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

    let internal isConnected (node: Node) : bool = JsMap.getKey node "isConnected"

    let findElement (doc : Document) selector = doc.querySelector(selector)

    let internal nullToEmpty s = if isNull s then "" else s

    /// Wrapper for Window.requestAnimationFrame
    let raf (f: float -> unit) =
        window.requestAnimationFrame (fun t ->
            try
                f t
            with
            | x -> Logging.error $"raf: {x.Message}")

    /// Wrapper for Window.requestAnimationFrame, ignoring the timestamp.
    let rafu (f: unit -> unit) =
        window.requestAnimationFrame (fun _ ->
            try
                f ()
            with
            | x -> Logging.error $"rafu: {x.Message}")
        |> ignore

module Dispose =
    open NodeKey
    open Browser.Types
    open Sutil.Log
    open Types
    open DomHelpers
    
    let [<Literal>] Disposables = "__sutil_ds"

    let makeDisposable (f : Unsubscribable) =
        { new System.IDisposable with member _.Dispose() = f.Invoke() }

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

    let addUnsubscribe( node : Node) (f : Unsubscribable) =
        f |> makeDisposable |> addDisposable node

    let internal disposeNode (node : Node) = 
        
        Sutil.Log.Console.log( "Posting Unmount for ", node |>  outerHTML)
        CustomEvents.dispatchSimple node CustomEvents.Unmount

        let safeDispose (d : System.IDisposable) = 
            try d.Dispose() with x -> log (sprintf "Error while disposing: %s" x.Message)

        EventListeners.clear node

        getDisposables node |> Array.iter safeDispose
        clearDisposables node

    let rec internal disposeTree (node : Node) = 

        descendantsDepthFirst node
        |> Array.ofSeq
        |> Array.iter disposeNode

        disposeNode node

    let internal dispose (node : Node) = disposeTree node

module DomEdit =

    open Browser.Types
    open Browser.Dom
    open Browser.CssExtensions
    open DomHelpers

    let remove (node : Node) = 
        Dispose.dispose node
        if not (isNull node.parentNode) then
            node.parentNode.removeChild(node) |> ignore

    let append (parent : Node) (node : Node) = 
        parent.appendChild(node) |> ignore

    let appendLabel label (parent : Node) (node : Node) = 
        append parent node

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

    /// Remove all children of this node, cleaning up Sutil resources and dispatching "unmount" events
    let clear (e : Node) =
        e |> children |> Seq.toArray |> Array.iter remove

    let text s : Node = document.createTextNode s

    let element tag = document.createElement tag

    let invisibleElement tag =
        let e = element tag
        e.style.display <- "none"
        e

    let private booleanAttributes = 
        [   "hidden"
            "disabled"
            "readonly"
            "required"
            "checked" ] |> Set

    let private isBooleanAttribute (name : string) = booleanAttributes.Contains (name.ToLower())

    let private toBool (v : obj) = 
            if v :? bool then
                v :?> bool
            else
                string v <> "false"

    let internal setAttribute (el: HTMLElement) (name: string) (value: obj) =

        let svalue = string value

        if isBooleanAttribute (name) then
            // we'd call el.toggleAttribute( name, bValue) if it was available
            if toBool value then
                el.setAttribute (name, "")
            else
                el.removeAttribute name
        else
            el.setAttribute(name, svalue)

    let removeAttribute (parent : HTMLElement) name =
        parent.removeAttribute(name)

module ClassHelpers =
    open System
    open Browser.Types

    let private  splitBySpace (s: string) =
        s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    let setClass (className: string) (e: HTMLElement) = e.className <- className

    let toggleClass (className: string) (e: HTMLElement) =
        e.classList.toggle (className) |> ignore

    let addToClasslist classes (e: HTMLElement) =
        e.classList.add (classes |> splitBySpace)

    let removeFromClasslist classes (e: HTMLElement) =
        e.classList.remove (classes |> splitBySpace)


[<AutoOpen>]
module Extensions =
    open Browser.Types

    type EventTarget with
        member __.asElement : HTMLElement =
            __ :?> HTMLElement

        member __.asEl<'T when 'T :> HTMLElement>() =
            (__ :?> 'T)