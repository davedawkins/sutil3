namespace Sutil.Internal

// /(Patch:|toDom|Mount)/

/// Type of function returned from effecting functions, that allow
/// the effect to be reversed. So, a subscribe() function returns a
/// function that unsubscribes; listen() function will return  a
/// function that stops listening, etc.
type Unsubscriber = unit -> unit

open Sutil

[<AutoOpen>]
module private Locals =
    let log = Sutil.Log.create "Dom"
    log.enabled <- false

/// JS interop. This doesn't belong in DomHelpers
module JsHelpers =
    open Fable.Core

    [<Emit("$0 === $1")>]
    let eq3 a b : bool = jsNative

    [<Emit("$0 == $1")>]
    let eq2 a b : bool = jsNative

/// JS interop specifically for keyed JS objects
[<RequireQualifiedAccess>]
module JsMap =
    open Fable.Core.JsInterop
    open Fable.Core

    let inline getKey (node: obj) (name: string) = node?(name)

    [<Emit("delete $0[$1]")>]
    let deleteKey (node: obj) (key: string) = jsNative

    let hasKey (node: obj) (key: string) = jsIn key node

    let setKey (node: obj) (key: string) (value: obj) : unit = node?(key) <- value

    let tryGetKey (node: obj) (key: string) : 't option =
        if hasKey node key then
            node?(key) |> Some
        else
            None

    let getKeyWith (node: obj) (key: string) (defaultValue: 't) : 't =
        tryGetKey node key |> Option.defaultValue defaultValue

    let arrayAppendKey (node: obj) (key: string) (value: 't) : unit =
        Array.singleton value
        |> Array.append (getKeyWith node key Array.empty)
        |> setKey node key

    let arrayRemoveKey (node: obj) (key: string) (pred: 't -> bool) : unit =
        getKeyWith node key (Array.empty: 't[])
        |> Array.filter (pred >> not)
        |> setKey node key

    let getCreate<'T> (node: obj) key (cons: unit -> 'T) : 'T =
        match tryGetKey node key with
        | Some v -> v
        | None ->
            let newVal = cons ()
            setKey node key newVal
            newVal

// Support for managing Sutil identifiers
[<RequireQualifiedAccess>]
module Id =
    open Browser.Types
    //open TypeHelpers

    [<Literal>]
    let private SUTIL_ID = "__sutil_id"

    [<Literal>]
    let private NodeMap = "__sutil_nodemap"

    let internal getNodeMap (doc: Document) : obj =
        JsMap.getCreate doc.body NodeMap (fun () -> upcast {| |})

    let setId (node: Node) (x: string) =
        JsMap.setKey node SUTIL_ID  x

        let map = getNodeMap node.ownerDocument
        JsMap.setKey map (string id) node

    let getId (node: Node) = JsMap.getKeyWith node SUTIL_ID ""

    let internal findNodeWithId (doc: Document) id : Node option =
        let map = getNodeMap doc
        let key = string id

        match JsMap.hasKey map key with
        | true -> Some(JsMap.getKey map key)
        | _ -> None

/// Support for custom events in general and the custom events that Sutil uses
[<RequireQualifiedAccess>]
module CustomEvents =
    open Fable.Core
    open Browser.Types

    //let [<Literal>] ELEMENT_READY = "sutil-element-ready"
    [<Literal>]
    let MOUNT = "sutil-mount"

    [<Literal>]
    let ELEMENT_READY = MOUNT

    [<Literal>]
    let UNMOUNT = "sutil-unmount"

    [<Literal>]
    let SHOW = "sutil-show"

    [<Literal>]
    let HIDE = "sutil-hide"

    [<Literal>]
    let UPDATED = "sutil-updated"

    [<Literal>]
    let CONNECTED = "sutil-connected"

    [<Emit("new CustomEvent($0, $1)")>]
    let customEvent name data = jsNative

    let private dispatch (target: EventTarget) name (data: obj) =
        if not (isNull target) then
            target.dispatchEvent (customEvent name data) |> ignore

    let internal dispatchSimple (target: EventTarget) name = dispatch target name {| |}

    let private dispatchCustom<'T>
        (target: EventTarget)
        (name: string)
        (init: CustomEventInit<'T>)
        =
        if not (isNull target) then
            target.dispatchEvent (customEvent name init) |> ignore

    let notifySutilUpdated (doc: EventTarget) = dispatchSimple doc UPDATED

    type EventOption = Once

    /// <summary>
    /// Custom events
    /// </summary>
    type CustomDispatch<'T> =
        | Detail of 'T
        | Bubbles of bool
        | Composed of bool

        static member toCustomEvent<'T>(props: CustomDispatch<'T> list) =
            let mutable data: obj = upcast {| |}

            for p in props do
                match p with
                | Detail d -> JsMap.setKey data "detail" d
                | Bubbles b -> JsMap.setKey data "bubbles" b
                | Composed c -> JsMap.setKey data "composed" c

            data :?> CustomEventInit<'T>

        static member dispatch(target: EventTarget, name: string) =
            dispatchCustom<unit> target name (CustomDispatch.toCustomEvent<unit> ([]))

        static member dispatch(e: Event, name: string) =
            dispatchCustom<unit> (e.target) name (CustomDispatch.toCustomEvent<unit> ([]))

        static member dispatch<'T>
            (target: EventTarget, name: string, props: CustomDispatch<'T> list)
            =
            dispatchCustom<'T> target name (CustomDispatch.toCustomEvent<'T> props)

        static member dispatch<'T>(e: Event, name: string, props: CustomDispatch<'T> list) =
            dispatchCustom<'T> (e.target) name (CustomDispatch.toCustomEvent<'T> props)

        static member dispatch(target: EventTarget, name, data: 'T) =
            dispatchCustom<'T>
                (target)
                name
                (CustomDispatch.toCustomEvent<unit> (
                    [
                        Detail data
                    ]
                ))

[<RequireQualifiedAccess>]
module NamedNodeMap =
    open Browser.Types

    let toArray (n: NamedNodeMap) =
        [|
            for i in 0 .. (n.length - 1) do
                let a = n.item (i)
                a.name, a.value
        |]

[<RequireQualifiedAccess>]
module Node =
    open Browser.Types

    [<Literal>]
    let internal ElementNodeType = 1.0

    [<Literal>]
    let internal TextNodeType = 3.0

    /// Return true if n is a Text node (nodeType = 3)
    let isTextNode (n: Node) = n <> null && n.nodeType = TextNodeType

    /// Return true if n is an Element node (nodeType = 1)
    let isElementNode (n: Node) =
        n <> null && n.nodeType = ElementNodeType

    let asEl<'T when 'T :> HTMLElement> (n: Node) : 'T = n :?> 'T
    let asElement (n: Node) : HTMLElement = n :?> HTMLElement

    let tryAsElement (n: Node) =
        if isElementNode n then
            Some(asElement n)
        else
            None

    let tryAsTextNode (node: Node) =
        if isTextNode node then
            Some(node :?> Browser.Types.Text)
        else
            None

    let outerHTML (n: Browser.Types.Node) =
        n
        |> tryAsElement
        |> Option.map _.outerHTML
        |> Option.defaultValue (
            if isNull n then
                "<null node>"
            else
                n.textContent
        )

    let elementTag (n: Browser.Types.Node) =
        if isElementNode n then
            (asElement n).tagName
        else
            ""

    let toStringSummary (node: Browser.Types.Node) =
        let _id node = Id.getId node

        if isNull node then
            "null"
        else
            let mutable tc = node.textContent

            if tc.Length > 16 then
                tc <- tc.Substring(0, 16) + "..."

            let _s s f =
                if s = "" then
                    ""
                else
                    sprintf f s

            match node.nodeType with
            | ElementNodeType ->
                let e = node :?> Browser.Types.HTMLElement
                let tn = (e.tagName.ToLower())

                let cs =
                    [
                        for i in 0 .. (e.classList.length - 1) do
                            e.classList[i]
                    ]
                    |> String.concat " "

                sprintf
                    "<%s%s%s>%s</%s>"
                    tn
                    (_s (cs) " class='%s'")
                    (_s (_id node) " sutil-id='%s'")
                    tc
                    tn

            | TextNodeType -> sprintf "<text sutil-id='%A'>%s</text>" (_id node) tc
            | _ -> $"?'{tc}'#{_id node}"

    let children (node: Browser.Types.Node) =
        let rec visit (child: Browser.Types.Node) =
            seq {
                if not (isNull child) then
                    yield child
                    yield! visit child.nextSibling
            }

        if isNull node then
            Seq.empty
        else
            visit node.firstChild

    let rec toString (node: Browser.Types.Node) =
        let _id node = Id.getId node

        if isNull node then
            "null"
        else

            //if tc.Length > 16 then
            //    tc <- tc.Substring(0, 16) + "..."

            let _s s f =
                if s = "" then
                    ""
                else
                    sprintf f s

            match node.nodeType with
            | ElementNodeType ->
                let mutable tc = children node |> Seq.map toString |> String.concat ""
                let e = node |> asEl
                let tn = (e.tagName.ToLower())

                let cs =
                    [
                        for i in 0 .. (e.classList.length - 1) do
                            e.classList[i]
                    ]
                    |> String.concat " "

                sprintf
                    "<%s%s%s%s>%s</%s>"
                    tn
                    (_s (cs) " class='%s'")
                    (_s (e.getAttribute("data-sutil-key")) " key='%s'")
                    (_s (_id node) " sutil-id='%s'")
                    tc
                    tn

            | TextNodeType -> node.textContent
            | _ -> $"?'{node.textContent}'#{_id node}"

    let rec toStringOutline (node: Browser.Types.Node) =
        let _id node = Id.getId node

        if isNull node then
            "null"
        else

            //if tc.Length > 16 then
            //    tc <- tc.Substring(0, 16) + "..."

            let _s s f =
                if s = "" then
                    ""
                else
                    sprintf f s

            match node.nodeType with
            | ElementNodeType ->
                let mutable tc = children node |> Seq.map toStringOutline |> String.concat ""
                let e = node |> asEl
                let tn = (e.tagName.ToLower())

                let cs =
                    [
                        for i in 0 .. (e.classList.length - 1) do
                            e.classList[i]
                    ]
                    |> String.concat " "

                sprintf "<%s%s%s>%s</%s>" tn (_s (cs) " class='%s'") (_s (_id node) " '%s'") tc tn

            | TextNodeType ->
                if node.textContent <> "" then
                    ".."
                else
                    ""
            | _ -> $"?'{node.textContent}'#{_id node}"

    let rec descendants (node: Browser.Types.Node) =
        seq {
            for child in children node do
                yield child
                yield! descendants child
        }

    let rec internal descendantsDepthFirst (node: Browser.Types.Node) =
        seq {
            for child in children node do
                yield! descendants child
                yield child
        }

    let rec internal descendantsDepthFirstReverse (node: Browser.Types.Node) =
        seq {
            for child in Seq.rev (children node) do
                yield! descendants child
                yield child
        }

    let isConnected (node: Browser.Types.Node) : bool = JsMap.getKey node "isConnected"

    let findElement (doc: Browser.Types.Document) selector = doc.querySelector (selector)

    let internal nullToEmpty s =
        if isNull s then
            ""
        else
            s

module Timers =

    /// Wrapper for Window.requestAnimationFrame
    let raf (f: float -> unit) =
        Browser.Dom.window.requestAnimationFrame (fun t ->
            try
                f t
            with x ->
                log.error $"raf: {x.Message}"
        )

    /// Wrapper for Window.requestAnimationFrame, ignoring the timestamp.
    let rafu (f: unit -> unit) =
        Browser.Dom.window.requestAnimationFrame (fun _ ->
            try
                f ()
            with x ->
                log.error $"rafu: {x.Message}"
        )
        |> ignore

    /// Call handler every delayMs. Return value is a function that will cancel the timer.
    let interval handler (delayMs: int) =
        let id = Fable.Core.JS.setInterval handler delayMs

        fun () -> Fable.Core.JS.clearInterval id

    /// Call handler after delayMs. Return value is a function that will cancel the timeout (if it hasn't occurred yet)
    let timeout handler (delayMs: int) =
        let id = Fable.Core.JS.setTimeout handler delayMs

        fun () -> Fable.Core.JS.clearTimeout id

module Promise =
    open Fable.Core

    let [<Literal>] private PROMISE_KEY = "__sutil_promise"

    ///<summary>
    /// Serialize tasks through an element. If the task already has a running task
    /// wait for it to complete before starting the new task. Otherwise, run the
    /// new task immediately
    ///</summary>

    let wait (el: Browser.Types.HTMLElement) (andThen: unit -> JS.Promise<unit>) =
        let key = PROMISE_KEY
        let run () = andThen () |> JsMap.setKey el key

        if JsMap.hasKey el key then
            let p : JS.Promise<unit> = JsMap.getKey el key
            JsMap.deleteKey el key
            p.``then`` run |> ignore
        else
            run ()

    
[<RequireQualifiedAccess>]
module HTMLElement =

    let attributes (el : Browser.Types.HTMLElement) = el.attributes |> NamedNodeMap.toArray

                
/// Support for disposing of Node-related resources (subscriptions etc)
module Dispose =
    open Browser.Types

    [<Literal>]
    let DISPOSABLES = "__sutil_ds"

    let makeDisposable (f: Unsubscriber) =
        { new System.IDisposable with
            member _.Dispose() = f ()
        }

    let private hasDisposables (node: Node) : bool = JsMap.hasKey node DISPOSABLES

    let private getDisposables (node: Node) : System.IDisposable[] =
        JsMap.getKeyWith node DISPOSABLES Array.empty

    let private clearDisposables (node: Node) : unit =
        JsMap.deleteKey node DISPOSABLES

        if (hasDisposables node) then
            failwith "Internal error"

    let setDisposables (node: Node) (ds: System.IDisposable[]) = 
        log.info("Setting disposables on ", node )
        JsMap.setKey node (DISPOSABLES) ds

    let addDisposable (node: Node) (name: string) (d: System.IDisposable) =
        log.info (
            "disposeNode: adding disposable '" + name + "' to ",
            node |> Node.toStringOutline
        )

        Array.singleton d |> Array.append (getDisposables node) |> setDisposables node

    let addUnsubscribe (node: Node) (name: string) (f: Unsubscriber) =
        f |> makeDisposable |> addDisposable node name

    let mutable _disposeId = 0

    let internal disposeNode (node: Node) =
        let _id = _disposeId
        _disposeId <- _disposeId + 1
        let text = node |> Node.toStringOutline
        log.info ("disposeNode: ", _id, text)

        let disposables = getDisposables node |> Array.copy
        clearDisposables node

        CustomEvents.dispatchSimple node CustomEvents.UNMOUNT

        let safeDispose (d: System.IDisposable) =
            try
                d.Dispose()
            with x ->
                log.error (sprintf "Error while disposing: %s" x.Message)

        // EventListeners.clear node

        disposables |> Array.iter safeDispose
        log.info ("EXIT: disposeNode: ", _id, text)

    let rec internal disposeTree (node: Node) =

        Node.descendantsDepthFirstReverse node |> Array.ofSeq |> Array.iter disposeNode

        disposeNode node

    let internal dispose (node: Node) = disposeTree node

    let composeUU (u1 : Unsubscriber) (u2 : Unsubscriber) =
        (u1>>u2) |> makeDisposable
            
    let composeDD (d1 : System.IDisposable)  (d2 : System.IDisposable)=
        (fun () ->
            d1.Dispose()
            d2.Dispose()) |> makeDisposable

    let composeDU (d : System.IDisposable) (u : Unsubscriber) =
        (fun () ->
            d.Dispose()
            u()) |> makeDisposable
            
    let composeUD (u : Unsubscriber)  (d : System.IDisposable)=
        (fun () ->
            d.Dispose()
            u()) |> makeDisposable

            
/// Support for editing classes
module ClassHelpers =
    open System
    open Browser.Types

    let splitBySpace (s: string) =
        s.Split(
            [|
                ' '
            |],
            StringSplitOptions.RemoveEmptyEntries
        )

    let setClass (className: string) (e: HTMLElement) = 
        e.setAttributeNS(null, "class", className)
//        e.className <- className // Doesn't work for SVG nodes

    let toggleClass (className: string) (e: HTMLElement) =
        e.classList.toggle (className) |> ignore

    let setClassList (element: HTMLElement) (classes: string[]) =
        setClass (classes |> String.concat " ") element

    let addToClasslist classes (e: HTMLElement) =
        e.classList.add (classes |> splitBySpace)

    let removeFromClasslist classes (e: HTMLElement) =
        e.classList.remove (classes |> splitBySpace)

    let toSeq (classes: DOMTokenList) : seq<string> =
        seq {
            for i in 0 .. (classes.length - 1) do
                yield classes[i]
        }

    let toArray (classes: DOMTokenList) : string[] = classes |> toSeq |> Seq.toArray

/// Support for edits to the DOM: creating nodes, setting attributes etc
module DomEdit =

    open Browser.CssExtensions

    let remove (node: Browser.Types.Node) =
        Dispose.dispose node

        if not (isNull node.parentNode) then
            node.parentNode.removeChild (node) |> ignore

    let append (parent: Browser.Types.Node) (node: Browser.Types.Node) = parent.appendChild (node) |> ignore

    let replace (parent: Browser.Types.Node) (current: Browser.Types.Node) (node: Browser.Types.Node) =
        if isNull (current) then
            failwith "Attempt to replace null node"

        try
            parent.insertBefore (node, current) |> ignore
            remove current
        with x ->
            Log.Console.error ("replace: ", x.Message)
            Log.Console.info ("current: ", current |> Node.toStringOutline)
            Log.Console.info ("node   : ", node |> Node.toStringOutline)
            Log.Console.info ("parent : ", parent |> Node.toStringOutline)

    let insertBefore (parent: Browser.Types.Node) (child: Browser.Types.Node) (refNode: Browser.Types.Node) =
        parent.insertBefore (child, refNode) |> ignore

    let insertAfter (parent: Browser.Types.Node) (newChild: Browser.Types.Node) (refChild: Browser.Types.Node) =
        let beforeChild =
            if isNull refChild then
                parent.firstChild
            else
                refChild.nextSibling

        insertBefore parent newChild beforeChild

    /// Remove all children of this node, cleaning up Sutil resources and dispatching "unmount" events
    let clear (e: Browser.Types.Node) =
        e |> Node.children |> Seq.toArray |> Array.iter remove

    let text s : Browser.Types.Node = Browser.Dom.document.createTextNode s

    let element tag = Browser.Dom.document.createElement tag

    let elementNS(ns : string, tag : string) : Browser.Types.Element =
        if ns = "" then 
            Browser.Dom.document.createElement tag
        else
            Browser.Dom.document.createElementNS(ns,tag)

    let invisibleElement tag =
        let e = element tag
        e.style.display <- "none"
        e

    let private booleanAttributes =
        [
            "hidden"
            "disabled"
            "readonly"
            "required"
            "checked"
        ]
        |> Set

    let private isBooleanAttribute (name: string) =
        booleanAttributes.Contains(name.ToLower())

    let private toBool (v: obj) =
        if v :? bool then
            v :?> bool
        else
            string v <> "false"

    let internal setAttribute (el: Browser.Types.Element) (name: string) (value: obj) =

        let svalue = string value

        if isBooleanAttribute (name) then
            // we'd call el.toggleAttribute( name, bValue) if it was available
            if toBool value then
                el.setAttribute (name, "")
            else
                el.removeAttribute name

        // The  (SutilElement -> VirtualElement) fn will collate all the classes into
        // into a single class attribute, so that we don't need to addToClassList here

        // elif (name.ToLower() = "class") then
        //     ClassHelpers.addToClasslist svalue el
        elif name = "value" then
            JsMap.setKey el "__value" value
            el.setAttribute (name, svalue)
        else
            el.setAttribute (name, svalue)

    let removeAttribute (parent: Browser.Types.HTMLElement) name = parent.removeAttribute (name)

    let setHeadStylesheet (doc: Browser.Types.Document) (url: string) =
        let head = Node.findElement doc "head"
        let styleEl = doc.createElement ("link")
        head.appendChild (styleEl) |> ignore
        styleEl.setAttribute ("rel", "stylesheet")
        styleEl.setAttribute ("href", url) |> ignore

    let setHeadScript (doc: Browser.Types.Document) (url: string) =
        let head = Node.findElement doc "head"
        let el = doc.createElement ("script")
        head.appendChild (el) |> ignore
        el.setAttribute ("src", url) |> ignore

    let setHeadEmbedScript (doc: Browser.Types.Document) (source: string) =
        let head = Node.findElement doc "head"
        let el = doc.createElement ("script")
        head.appendChild (el) |> ignore
        el.appendChild (doc.createTextNode (source)) |> ignore

    let setHeadTitle (doc: Browser.Types.Document) (title: string) =
        let head = Node.findElement doc "head"
        let existingTitle = Node.findElement doc "head>title"

        if not (isNull existingTitle) then
            head.removeChild (existingTitle) |> ignore

        let titleEl = doc.createElement ("title")
        titleEl.appendChild (doc.createTextNode (title)) |> ignore
        head.appendChild (titleEl) |> ignore

/// Support for managing event listeners.
module EventListeners =
    open Browser.Types

    let add (event: string) (node: EventTarget) (handler: Event -> unit) : Unsubscriber =
        // Bug in Feliz.Engine that sends us "dragStart" instead of "dragstart"
        node.addEventListener (event.ToLower(), handler)

        let remove = (fun () -> node.removeEventListener (event, handler))

        remove |> Dispose.addUnsubscribe (node :?> Node) event

        remove

    let listen (event: string) (node: EventTarget) (handler: Event -> unit) : Unsubscriber =
        add event node handler

    /// Listen for the given event, and remove the listener after the first occurrence of the evening firing.
    let once (event: string) (target: EventTarget) (fn: Event -> Unit) : Unsubscriber =
        let mutable remove: Unsubscriber = Unchecked.defaultof<_>

        let rec inner e =
            remove ()
            fn (e)

        remove <- add event (target :?> Node) inner
        remove

///
[<AutoOpen>]
module Extensions =
    open Browser.Types

    type Browser.Types.NamedNodeMap with
        member __.ToArray() = NamedNodeMap.toArray __

    type NodeList with
        member __.toSeq() =
            seq {
                for i in 0 .. (__.length - 1) do
                    yield __[i]
            }

    type Node with
        member __.asTextNode = Node.tryAsTextNode __
        member __.asElement = Node.tryAsElement __

    type EventTarget with
        member __.asElement: HTMLElement = __ :?> HTMLElement

        member __.asEl<'T when 'T :> HTMLElement>() = (__ :?> 'T)
