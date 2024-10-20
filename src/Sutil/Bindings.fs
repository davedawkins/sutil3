///<exclude/>
module internal Sutil.Bindings

open Core
open Sutil.Internal
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

let logEachEnabled key = false //Logging.isEnabled key

open Sutil2

let elementFromException (x: exn) =
    el "div" [
        attr ("style", "color: #FF8888;")
        attr ("title", "See console for details")
        text ("sutil: exception in bind: " + x.Message)
    ]

let private bindLog = Log.create ("Bind")
bindLog.enabled <- false

open VirtualDom

let bindElementWithName<'T>
    (name: string)
    (source: IObservable<'T>)
    (view: 'T -> SutilElement)
    (compare: 'T -> 'T -> bool)
    =
    let _log = Log.create("Bind:" + name)
    _log.enabled <- false

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
                    _context.ParentNode |> Node.toString
                )
                _log.trace ("current=", currentNode |> Node.toString)

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
                        _log.trace ( "next parent=", currentNode.parentNode |> Node.toString )
                        _log.trace ( "next current=", currentNode |> Node.toString )
                        _log.trace ( "next ctxprnt=", _context.ParentNode |> Node.toString )

                with x ->
                    JS.console.error (x)
                    currentNode <- 
                        (elementFromException x) 
                        |> mountWith mapOptions (context.WithCurrent(currentNode))
                        |> _.Node
            )
    )

let bindElement source view compare =
    bindElementWithName "bindElement" source view compare

let bindElement2<'A, 'B>
    (a: IObservable<'A>)
    (b: IObservable<'B>)
    (view: ('A * 'B) -> SutilElement)
    =
    bindElement (Store.zip a b) view (fun _ _ -> false)


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
// let private setInputValue el (v: string) = Interop.set el "value" v

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
        fun parent ->
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

                    Timers.rafu notify
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

type EachItemRenderer<'T> =
    | Static of ('T -> SutilElement)
    | StaticIndexed of (int * 'T -> SutilElement)

let private eachItemRender (renderer: EachItemRenderer<'T>) (index: int) (item: 'T) : SutilElement =
    match renderer with
    | Static v -> v (item)
    | StaticIndexed v -> v (index, item)

let eachiko_wrapper
    (source: IObservable<ICollectionWrapper<'T>>)
    (view: EachItemRenderer<'T>)
    (key: int * 'T -> 'K)
    : SutilElement
    =
    bindElement
        source
        (fun (items: ICollectionWrapper<'T>) ->
            SutilElement.Fragment(items.Mapi(eachItemRender view).ToArray())
        )
        (fun _ _ -> false)

let private duc = Observable.distinctUntilChanged

let eachiko = eachiko_wrapper

let each (items: IObservable<ICollectionWrapper<'T>>) (view: 'T -> SutilElement) =
    eachiko_wrapper items (Static view) (fun (i, v) -> i, v.GetHashCode())

let eachi
    (items: IObservable<ICollectionWrapper<'T>>)
    (view: (int * 'T) -> SutilElement)
    : SutilElement
    =
    eachiko items (StaticIndexed view) fst


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

let bindElementEffect<'T, 'Element when 'Element :> HTMLElement>
    (value: IObservable<'T>)
    (f: 'Element -> 'T -> unit)
    =
    CoreElements.bindSubscribe value f

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
