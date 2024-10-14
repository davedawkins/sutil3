///<exclude/>
module internal Sutil.Bindings

open Core
open Sutil.Internal
open DomHelpers
open Browser.Types
open System
open Fable.Core
open Sutil

/// <exclude/>
type internal ICollectionWrapper<'T> =
    abstract member ToList: unit -> List<'T>
    abstract member ToArray: unit -> 'T array
    abstract member Length: int
    abstract member Mapi: (int -> 'T -> 'R) -> ICollectionWrapper<'R>
    abstract member Map: ('T -> 'R) -> ICollectionWrapper<'R>
    abstract member Exists: ('T -> bool) -> bool
    abstract member TryFind: ('T -> bool) -> 'T option
    inherit System.Collections.Generic.IEnumerable<'T>

///  <exclude />
[<AutoOpen>]
module internal CollectionWrapperExt =

    type private ListW<'T>(list: 'T list) =
        interface ICollectionWrapper<'T> with
            member _.ToList() = list
            member _.ToArray() = list |> Array.ofList
            member _.Length = list.Length
            member _.Mapi(f: (int -> 'T -> 'R)) = upcast ListW((list |> List.mapi f))
            member _.Map(f: ('T -> 'R)) = upcast ListW(list |> List.map f)
            member _.Exists(p: 'T -> bool) = list |> List.exists p
            member _.TryFind(p: 'T -> bool) = list |> List.tryFind p

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                upcast (list |> Seq.ofList).GetEnumerator()

        interface System.Collections.Generic.IEnumerable<'T> with
            member _.GetEnumerator() =
                upcast (list |> Seq.ofList).GetEnumerator()

    type private ArrayW<'T>(a: 'T array) =
        interface ICollectionWrapper<'T> with
            member _.ToList() = a |> List.ofArray
            member _.ToArray() = a
            member _.Length = a.Length
            member _.Mapi(f: (int -> 'T -> 'R)) = upcast ArrayW((a |> Array.mapi f))
            member _.Map(f: ('T -> 'R)) = upcast ArrayW(a |> Array.map f)
            member _.Exists(p: 'T -> bool) = a |> Array.exists p
            member _.TryFind(p: 'T -> bool) = a |> Array.tryFind p

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                upcast (a |> Seq.ofArray).GetEnumerator()

        interface System.Collections.Generic.IEnumerable<'T> with
            member _.GetEnumerator() =
                upcast (a |> Seq.ofArray).GetEnumerator()

    type List<'T> with
        member internal __.ToCollectionWrapper() : ICollectionWrapper<'T> = upcast ListW(__)

    type 'T ``[]`` with
        member internal __.ToCollectionWrapper() : ICollectionWrapper<'T> = upcast ArrayW(__)

///  <exclude />
module internal CollectionWrapper =
    let length (c: ICollectionWrapper<'T>) = c.Length
    let mapi (f: (int -> 'T -> 'R)) (c: ICollectionWrapper<'T>) = c.Mapi f
    let map (f: ('T -> 'R)) (c: ICollectionWrapper<'T>) = c.Map f
    let exists f (c: ICollectionWrapper<'T>) = c.Exists(f)
    let tryFind f (c: ICollectionWrapper<'T>) = c.TryFind(f)

let private logEnabled () = false
let private log s = Log.Console.log (s)
let logEachEnabled key = false //Logging.isEnabled key

// Binding helper
// let bindSubCtx<'T> (source: IObservable<'T>) (handler: BuildContext -> 'T -> unit) =
//     SutilElement.Define(
//         "bindSub",
//         fun ctx ->
//             let unsub = source.Subscribe(handler ctx)
//             SutilEffect.RegisterDisposable(ctx.Parent, "bindSub", unsub)
//     )


open Sutil2

let elementFromException (x: exn) =
    el "div" [
        attr ("style", "color: #FF8888;")
        attr ("title", "See console for details")
        text ("sutil: exception in bind: " + x.Message)
    ]


(*
    span
        bind <

*)
let private bindLog = Log.create ("Bind")
bindLog.enabled <- true

open VirtualDom

let bindElementWithName<'T>
    (name: string)
    (source: IObservable<'T>)
    (view: 'T -> SutilElement)
    (compare: 'T -> 'T -> bool)
    =
    let _log = Log.create("Bind:" + name)
    _log.enabled <- true

    let mapOptions (options : BuildOptions) : BuildOptions =
        options
            .WithPostBuildVirtualElement(
                fun (ve : VirtualElement) -> 
                    ve.WithKey("host-for-" + name)
            )

    SutilElement.DefineBinding(
        name,
        fun _context ->

            let mutable currentNode: Node = _context.Current

            let context = _context.WithCurrent(null)

            if _log.enabled then

                _log.trace (
                    "parent=",
                    _context.ParentNode |> DomHelpers.toString
                )
                _log.trace ("current=", currentNode |> DomHelpers.toString)

            source
            |> Observable.distinctUntilChangedCompare compare
            |> Store.subscribe (fun value ->
                try
                    if _log.enabled then
                        _log.trace ( sprintf "rebuilding with value %A" (Fable.Core.JS.JSON.stringify value) )

                    currentNode <- 
                        value   
                        |> view
                        |> mountWith mapOptions (context.WithCurrent(currentNode))
                        |> _.Node

                    if _log.enabled then
                        _log.trace ( "next parent=", currentNode.parentNode |> DomHelpers.toString )
                        _log.trace ( "next current=", currentNode |> DomHelpers.toString )
                        _log.trace ( "next ctxprnt=", _context.ParentNode |> DomHelpers.toString )

                with x ->
                    JS.console.error (x)
                    currentNode <- (elementFromException x) |> mount (context.WithCurrent(null)) |> _.Node
            )
    )

let bindElement source view compare =
    bindElementWithName "bindElement" source view compare

/// Backwards compatibility
let bindFragment = bindElement

let bindElement2<'A, 'B>
    (a: IObservable<'A>)
    (b: IObservable<'B>)
    (view: ('A * 'B) -> SutilElement)
    =
    SutilElement.DefineBinding(
        "bindElement2",
        fun ctx ->
            let mutable currentNode: Node = ctx.Current

            Store.subscribe2
                a
                b
                (fun value ->
                    try
                        currentNode <- value |> view |> mount (ctx.WithCurrent(currentNode)) |> _.Node
                    with x ->
                        Logging.error $"Exception in bind: {x.Message}"
                )
    )

let bindElementK<'T, 'K when 'K: equality>
    (name: string)
    (store: IObservable<'T>)
    (element: 'T -> SutilElement)
    (key: 'T -> 'K)
    : SutilElement
    =
    let compare a b = key a = key b
    bindElementWithName name store element compare

let bindPromiseStore<'T>
    (p: ObservablePromise<'T>)
    (waiting: SutilElement)
    (result: 'T -> SutilElement)
    (fail: Exception -> SutilElement)
    : SutilElement
    =
    bindElement
        p
        (function
        | PromiseState.Waiting -> waiting
        | PromiseState.Result r -> result r
        | PromiseState.Error x -> fail x)
        (fun _ _ -> false)

let bindPromise<'T>
    (p: JS.Promise<'T>)
    (waiting: SutilElement)
    (result: 'T -> SutilElement)
    (fail: Exception -> SutilElement)
    : SutilElement
    =
    let x = new ObservablePromise<'T>(p) // This is disposable. TODO: Wire it into something...
    bindPromiseStore x waiting result fail

type BindFn<'T> = IObservable<'T> -> ('T -> SutilElement) -> SutilElement
let private getInputChecked el = Interop.get el "checked"
let private setInputChecked (el: Node) (v: obj) = Interop.set el "checked" v
let private getInputValue el : string = Interop.get el "value"
let private setInputValue el (v: string) = Interop.set el "value" v

let bindSelected<'T when 'T: equality>
    (selection: IObservable<List<'T>>)
    (dispatch: List<'T> -> unit)
    =
    CoreElements.bindDisposable(
        //"bindSelected",
        fun (selectElement : HTMLSelectElement) ->

            //let selectElement = parent :?> HTMLSelectElement
            let selOps = selectElement.selectedOptions
            let op (coll: HTMLCollection) i = coll.[i] :?> HTMLOptionElement
            let opValue op : 'T = Interop.get op "__value"

            let getValueList () =
                [
                    0 .. selOps.length - 1
                ]
                |> List.map (fun i -> opValue (op selOps i))

            let updateSelected (v: List<'T>) =
                let ops = selectElement.options

                for i in
                    [
                        0 .. ops.length - 1
                    ] do
                    let o = op ops i
                    o.selected <- v |> List.contains (opValue o)

            let unsubInput =
                listen "input" selectElement <| fun _ -> getValueList () |> dispatch

            // We need to finalize checked status after all attrs have been processed for input,
            // in case 'value' hasn't been set yet
            EventListeners.once
                CustomEvents.ELEMENT_READY
                selectElement
                (fun _ ->
                    let unsub = selection |> Store.subscribe (updateSelected)
                    SutilEffect.RegisterDisposable(selectElement, "bindSelected:unsub", unsub)
                )
            |> ignore

            unsubInput |> Dispose.makeDisposable
//            SutilEffect.RegisterUnsubscribe(ctx.Parent, "bindSelected:unsubInput", unsubInput)

    )

let bindSelectMultiple<'T when 'T: equality> (store: IStore<List<'T>>) =
    bindSelected store (fun sln -> store <~ sln)

let bindSelectSingle<'T when 'T: equality> (store: IStore<'T>) : SutilElement =
    bindSelected (store .> List.singleton) (fun sln -> sln |> List.exactlyOne |> Store.set store)

let bindSelectOptional<'T when 'T: equality> (store: IStore<'T option>) : SutilElement =
    let toList topt =
        match topt with
        | None -> []
        | Some t -> List.singleton t

    let fromList list =
        match list with
        | [] -> None
        | x :: _ -> Some x

    bindSelected (store .> toList) (fun sln -> sln |> fromList |> Store.set store)

let private isNullString (obj: obj) =
    isNull obj || System.String.IsNullOrEmpty(downcast obj)

let private getId (s: IStore<'T>) = s.GetHashCode()

let bindGroup<'T> (store: IStore<List<string>>) : SutilElement =
    CoreElements.bindDisposable(
        //"bindGroup",
        fun parent ->
            let name =
                match Interop.get parent "name" with
                | s when isNullString s -> $"store-{getId store}"
                | s -> s

            // Group this input with all other inputs that reference the same store
            Interop.set parent "name" name

            let getValueList () =
                let inputs = (documentOf parent).querySelectorAll (@$"input[name=""{name}""]")

                [
                    0 .. (inputs.length - 1)
                ]
                |> List.map (fun i -> inputs.[i])
                |> List.filter getInputChecked
                |> List.map getInputValue

            let updateChecked (v: List<string>) =
                setInputChecked parent (v |> List.contains (getInputValue parent))

            // Update the store when the radio box is clicked on
            let unsubInput =
                listen "input" parent <| fun _ -> getValueList () |> Store.set store

            // We need to finalize checked status after all attrs have been processed for input,
            // in case 'value' hasn't been set yet
            once CustomEvents.ELEMENT_READY parent (fun _ -> store |> Store.get |> updateChecked)
            |> ignore

            // When store changes make sure check status is synced
            store 
            |> Store.subscribe (updateChecked)
            |> Dispose.composeUD unsubInput 
            // SutilEffect.RegisterDisposable(ctx.Parent, "bindGroup:unsub", unsub)
            // SutilEffect.RegisterUnsubscribe(ctx.Parent, "bindGroup:unsubInput", unsubInput)
    )

// T can realistically only be numeric or a string. We're relying (I think!) on JS's ability
// to turn a string into an int automatically in the Store.set call (maybe it's Fable doing that)
//
let bindRadioGroup<'T> (store: IStore<'T>) : SutilElement =
    CoreElements.bindDisposable(
        //"bindRadioGroup",
        fun parent ->

            let name =
                match Interop.get parent "name" with
                | s when isNullString s -> $"store-{getId store}"
                | s -> s
            // Group this input with all other inputs that reference the same store
            Interop.set parent "name" name

            let updateChecked (v: obj) =
                setInputChecked parent ((string v) = getInputValue parent)

            // We need to finalize checked status after all attrs have been processed for input,
            // in case 'value' hasn't been set yet
            once CustomEvents.ELEMENT_READY parent (fun _ -> store |> Store.get |> updateChecked)
            |> ignore

            // Update the store when the radio box is clicked on
            listen 
                "input" 
                parent 
                (fun _ -> Interop.get parent "value" |> Store.set store)
            |> Dispose.composeDU 
                (store |> Store.subscribe updateChecked)
    )

let bindClassToggle
    (toggle: IObservable<bool>)
    (classesWhenTrue: string)
    (classesWhenFalse: string)
    =
    CoreElements.bindSubscribe toggle
    <| fun el active ->
        if active then
            el |> ClassHelpers.removeFromClasslist classesWhenFalse
            el |> ClassHelpers.addToClasslist classesWhenTrue
        else
            el |> ClassHelpers.removeFromClasslist classesWhenTrue
            el |> ClassHelpers.addToClasslist classesWhenFalse

let bindBoolAttr (toggle: IObservable<bool>) (boolAttr: string) =
    CoreElements.bindSubscribe toggle
    <| fun el active ->
        match active with
        | true -> DomEdit.setAttribute el boolAttr boolAttr
        | false -> el.removeAttribute (boolAttr)

// Deprecated
let bindClass (toggle: IObservable<bool>) (classes: string) = 
    bindClassToggle toggle classes ""

let bindClassNames (classNames: IObservable<#seq<string>>) =
    CoreElements.bindSubscribe 
        classNames <|
            fun el current ->
                el.className <- ""
                el.classList.add (current |> Array.ofSeq)

let bindClassName (classNames: IObservable<string>) =
    CoreElements.bindSubscribe classNames (fun el current -> el.className <- current)

/// Bind a store value to an element attribute. Updates to the element are unhandled
let bindAttrIn<'T> (attrName: string) (store: IObservable<'T>) : SutilElement =
    CoreElements.bindDisposable(
        //"bindAttrIn",
        fun parent ->
            if attrName = "class" then
                store |> Store.subscribe (fun cls -> parent.className <- (string cls))
            else
                store |> Store.subscribe (DomEdit.setAttribute parent attrName)
    )

let bindAttrOut<'T> (attrName: string) (onchange: 'T -> unit) : SutilElement =
    CoreElements.bindDisposable(
        //"bindAttrOut",
        fun parent ->
            listen "input" parent (fun _ -> Interop.get parent attrName |> onchange)
            |> Dispose.makeDisposable
    )

// Bind a scalar value to an element attribute. Listen for onchange events and dispatch the
// attribute's current value to the given function. This form is useful for view templates
// where v is invariant (for example, an each that already filters on the value of v, like Todo.Done)
let attrNotify<'T> (attrName: string) (value: 'T) (onchange: 'T -> unit) : SutilElement =
    CoreElements.bindDisposable(
        // "attrNotify",
        fun parent ->
            listen 
                "input" 
                parent 
                (fun _ -> Interop.get parent attrName |> onchange)

            |> fun stop ->
                Interop.set parent attrName value
                stop

            |> Dispose.makeDisposable
    )

// Bind an observable value to an element attribute. Listen for onchange events and dispatch the
// attribute's current value to the given function
let bindAttrBoth<'T>
    (attrName: string)
    (value: IObservable<'T>)
    (onchange: 'T -> unit)
    : SutilElement
    =
    fragment [
        bindAttrIn attrName value
        bindAttrOut attrName onchange
    ]

let bindListen<'T>
    (attrName: string)
    (store: IObservable<'T>)
    (event: string)
    (handler: Event -> unit)
    : SutilElement
    =
    CoreElements.bindDisposable(
        //"bindListen",
        fun parent ->
            listen event parent handler
            |> Dispose.composeDU 
                (store |> Store.subscribe (Interop.set parent attrName))
    )

        
// Bind a store value to an element attribute. Listen for onchange events write the converted
// value back to the store
let private bindAttrConvert<'T>
    (attrName: string)
    (store: IStore<'T>)
    (convert: obj -> 'T)
    : SutilElement
    =
    CoreElements.bindDisposable(
        //"bindAttrConvert",
        fun parent ->
            //let attrName' = if attrName = "value" then "__value" else attrName
            listen 
                "input" 
                parent
                (fun _ -> Interop.get parent attrName |> convert |> Store.set store)
            |> Dispose.composeDU (store |> Store.subscribe (Interop.set parent attrName))
    )

// Unsure how to safely convert Element.getAttribute():string to 'T
let private convertObj<'T> (v: obj) : 'T = v :?> 'T

// Bind a store to an attribute in both directions
let bindAttrStoreBoth<'T> (attrName: string) (store: IStore<'T>) =
    bindAttrConvert attrName store convertObj<'T>

let bindAttrStoreOut<'T> (attrName: string) (store: IStore<'T>) : SutilElement =
    CoreElements.bindDisposable(
//        "bindAttrStoreOut",
        fun parent ->
            listen 
                "input" 
                parent
                (fun _ -> Interop.get parent attrName |> convertObj<'T> |> Store.set store)
            |> Dispose.makeDisposable
    )

let private attrIsSizeRelated (attrName: string) =
    let upr = attrName.ToUpper()
    upr.IndexOf("WIDTH") >= 0 || upr.IndexOf("HEIGHT") >= 0

let listenToProp<'T> (attrName: string) (dispatch: 'T -> unit) : SutilElement =
    CoreElements.bindDisposable(
        //sprintf "listenToProp %s" attrName,
        fun parent ->
            //let parent = ctx.ParentNode

            let notify () =
                Interop.get parent attrName |> convertObj<'T> |> dispatch

            once
                CustomEvents.ELEMENT_READY
                parent
                (fun _ ->
                    if attrIsSizeRelated attrName then
                        SutilEffect.RegisterDisposable(
                            parent,
                            "listenToProp",
                            (ResizeObserver.getResizer parent).Subscribe(notify)
                        )
                    else
                        SutilEffect.RegisterUnsubscribe(
                            parent,
                            "listenToProp",
                            listen "input" parent (fun _ -> notify ())
                        )

                    rafu notify
                )
            |> ignore

            ignore |> Dispose.makeDisposable
    )

let bindPropOut<'T> (attrName: string) (store: IStore<'T>) : SutilElement =
    listenToProp attrName (Store.set store)

type KeyedStoreItem<'T, 'K> =
    {
        Key: 'K
        Node: Node //SutilEffect
        SvId: int
        Position: IStore<int>
        Value: IStore<'T>
    }

let private findCurrentNode doc (current: Node) (id: int) =
    if (isNull current || isNull current.parentNode) then
        if logEnabled () then
            log ($"each: Find node with id {id}")

        match Id.findNodeWithId doc id with
        | None ->
            if logEnabled () then
                log ("each: Disaster: cannot find node")

            null
        | Some n ->
            if logEnabled () then
                log ($"each: Found it: {n}")

            n
    else
        //log($"Cannot find node with id {id}")
        current

let private findCurrentElement doc (current: Node) (id: int) =
    let node = findCurrentNode doc current id

    match node with
    | null -> null
    | n when TypeHelpers.isElementNode n -> n :?> HTMLElement
    | x ->
        if logEnabled () then
            log $"each: Disaster: found node but it's not an HTMLElement"

        null

let private genEachId = Helpers.createIdGenerator ()

type EachItemRenderer<'T> =
    | Static of ('T -> SutilElement)
    | StaticIndexed of (int * 'T -> SutilElement)
// | LiveStore of (IReadOnlyStore<'T> -> SutilElement)
// | Live of (IObservable<'T> -> SutilElement)
// | LiveIndexed of (IObservable<int> * IObservable<'T> -> SutilElement)

let private eachItemRender (renderer: EachItemRenderer<'T>) (index: int) (item: 'T) : SutilElement =
    match renderer with
    | Static v -> v (item)
    | StaticIndexed v -> v (index, item)
// | Live v -> v item
// | LiveStore v -> v item
// | LiveIndexed v -> v (index,item)

let eachiko_wrapper
    (source: IObservable<ICollectionWrapper<'T>>)
    (view: EachItemRenderer<'T>)
    (key: int * 'T -> 'K)
    : SutilElement
    =
    //    Bind.el( source, (Array.map view >>  SutilElement.Fragment) )
    bindElement
        source
        (fun (items: ICollectionWrapper<'T>) ->
            SutilElement.Fragment(items.Mapi(eachItemRender view).ToArray())
        )
        (fun _ _ -> false)

let private duc = Observable.distinctUntilChanged

let eachiko = eachiko_wrapper

let each (items: IObservable<ICollectionWrapper<'T>>) (view: 'T -> SutilElement) =
    //eachiko_wrapper items (fun (_,item) -> bindElement (duc item) view) (fun (i,v) -> i,v.GetHashCode()) trans
    eachiko_wrapper items (Static view) (fun (i, v) -> i, v.GetHashCode())

let eachi
    (items: IObservable<ICollectionWrapper<'T>>)
    (view: (int * 'T) -> SutilElement)
    : SutilElement
    =
    //eachiko items (fun (index,item) -> bindElement2 (duc index) (duc item) view) fst trans
    eachiko items (StaticIndexed view) fst

// let eachio (items:IObservable<ICollectionWrapper<'T>>) (view : (IObservable<int>*IObservable<'T>) -> SutilElement)   =
//     //eachiko items view fst trans
//     eachiko items (LiveIndexed view) fst trans

let eachk (items: IObservable<ICollectionWrapper<'T>>) (view: 'T -> SutilElement) (key: 'T -> 'K) =
    eachiko
        items
        //(fun (_,item) -> bindElement (duc item) view)
        (Static view)
        (snd >> key)

open Browser.CssExtensions

let bindStyle<'T> (value: IObservable<'T>) (f: CSSStyleDeclaration -> 'T -> unit) =
    CoreElements.bindSubscribe 
        value
        (fun el value -> f (el.style) value)

    // SutilElement.Define(
    //     "bindStyle",
    //     fun ctx ->
    //         let style = ctx.ParentElement.style
    //         let unsub = value.Subscribe(f style)
    //         SutilEffect.RegisterDisposable(ctx.Parent, "bindStyle", unsub)
    // )

let bindElementEffect<'T, 'Element when 'Element :> HTMLElement>
    (value: IObservable<'T>)
    (f: 'Element -> 'T -> unit)
    =
    CoreElements.bindSubscribe value f
    // SutilElement.DefineBinding(
    //     "bindElementEffect",
    //     fun ctx ->
    //         let el = ctx.ParentElement :?> 'E
    //         value.Subscribe(f el)
    // )

let bindWidthHeight (wh: IObservable<float * float>) =
    bindStyle
        wh
        (fun style (w, h) ->
            if w <> 0.0 && h <> 0.0 then
                style.width <- w.ToString() + "px"
                style.height <- h.ToString() + "px"
        )

let bindLeftTop (xy: IObservable<float * float>) =
    bindStyle
        xy
        (fun style (x, y) ->
            if x <> 0.0 && y <> 0.0 then
                style.left <- x.ToString() + "px"
                style.top <- y.ToString() + "px"
        )

let (|=>) store element = bindElement store element

let cssAttrsToString (cssAttrs) =
    cssAttrs |> Seq.map (fun (n, v) -> $"{n}: {v};") |> String.concat ""

let listWrap (list: 'T list) = list.ToCollectionWrapper()
let listWrapO (list: IObservable<'T list>) = list |> Store.map listWrap

let arrayWrap (arr: 'T array) = arr.ToCollectionWrapper()
let arrayWrapO (arr: IObservable<'T array>) = arr |> Store.map arrayWrap
