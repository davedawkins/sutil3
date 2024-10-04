///<exclude/>
module internal Sutil.Bindings

open Core
open Sutil.Dom
open DomHelpers
open Browser.Types
open System
open Fable.Core
open CoreTypes
open Store

let private logEnabled() = false
let private log s = Log.Console.log(s)
let logEachEnabled key = false //Logging.isEnabled key

// Binding helper
let bindSub<'T> (source : IObservable<'T>) (handler : BuildContext -> 'T -> unit) =
    SutilElement.Define( "bindSub",
    fun ctx ->
    let unsub = source.Subscribe( handler ctx )
    SutilEffect.RegisterDisposable(ctx.Parent,unsub)
    EffectedNode (ctx.ParentElement)
    )

open Sutil2

let elementFromException (x : exn) =
    el "div" [
        attr ("style","color: #FF8888;")
        attr ("title", "See console for details")
        text ("sutil: exception in bind: " + x.Message)
    ]

let bindElementC<'T>  (source : IObservable<'T>) (view: 'T -> SutilElement) (compare : 'T -> 'T -> bool)=
        SutilElement.SideEffect (
            "bindElementC",
            fun context -> 

                let context = context.WithMount BuildContext.DefaultMount

                let mutable currentNode : Node =
                    SutilElement.HiddenDiv
                    |> mount context null

                source|> Observable.distinctUntilChangedCompare compare |> Store.subscribe( fun value ->
                    try
                        currentNode <-
                            value
                            |> view 
                            |> mount context currentNode
                    with
                    | x ->
                        JS.console.error(x)
                        currentNode <- 
                            (elementFromException x) 
                            |> mount context currentNode

                ) |> Dispose.addDisposable (context.ParentElement) // FIXME: Put this in the disposals list

                EffectedNode (context.ParentElement)
        )   

let bindElementCO<'T>  (source : IObservable<'T>) (view: IObservable<'T> -> SutilElement) (compare : 'T -> 'T -> bool)=
        SutilElement.SideEffect (
            "bindElementCO",
            fun context -> 

                let context = context.WithMount BuildContext.DefaultMount

                let mutable currentNode : Node =
                    SutilElement.HiddenDiv
                    |> mount context null

                source|> Observable.distinctUntilChangedCompare compare |> Store.subscribe( fun value ->
                    try
                        currentNode <-
                            source
                            |> view 
                            |> mount context currentNode
                    with
                    | x ->
                        JS.console.error(x)
                        currentNode <- 
                            (elementFromException x) 
                            |> mount context currentNode
                ) |> Dispose.addDisposable (context.ParentElement) // FIXME: Put this in the disposals list

                EffectedNode (context.ParentElement)
        )   

// let __bindElementCO<'T>  (store : IObservable<'T>) (element: IObservable<'T> -> SutilElement) (compare : 'T -> 'T -> bool)=
//     SutilElement.Define( "bindElementCO",
//     fun ctx ->
//         let mutable node = SideEffect

//         let run() =
//             let bindCtx = ctx // { ctx with Parent = bindNode }
//             let disposable = store |> Observable.distinctUntilChangedCompare compare |> Store.subscribe (fun next ->
//                 try
//                     node <- build (element(store)) (bindCtx |> ContextHelpers.withReplace (node,group.NextDomNode))
//                 with
//                 | x ->
//                     JS.console.error("sutil.bindElementCO:parentNode: ", ctx.ParentNode, "exception:", x)
//                     node <- build (elementFromException x) (bindCtx |> ContextHelpers.withReplace (node,group.NextDomNode))
//             )
//             // group.RegisterUnsubscribe ( fun () ->
//             //     if logEnabled() then log($"dispose: Bind.el: {group}")
//             //     node.Dispose()
//             //     disposable.Dispose())


//         run()

        
//         )

let bindElement<'T>  (store : IObservable<'T>)  (element: 'T -> SutilElement) : SutilElement=
    bindElementCO store (Store.current >> element) (fun _ _-> false)

/// Backwards compatibility
let bindFragment = bindElement

let bindElement2<'A,'B> (a : IObservable<'A>) (b : IObservable<'B>)  (view: ('A*'B) -> SutilElement) =
    SutilElement.Define(
        "bindElement2",
        fun ctx ->
            let mutable currentNode : Node = Unchecked.defaultof<_>

            Store.subscribe2 a b (fun value ->
                try
                    currentNode <-
                        value
                        |> view 
                        |> mount ctx currentNode
                with
                | x -> Logging.error $"Exception in bind: {x.Message}"
            ) |> Dispose.addDisposable ctx.ParentElement

            EffectedNode (ctx.ParentElement)
    )

let bindElementKO<'T,'K when 'K : equality> (store : IObservable<'T>) (element: IObservable<'T> -> SutilElement) (key : 'T -> 'K) : SutilElement =
    let compare a b = key a = key b
    bindElementCO store element compare

let bindElementK<'T,'K when 'K : equality> (store : IObservable<'T>) (element: 'T -> SutilElement) (key : 'T -> 'K) : SutilElement =
    let compare a b = key a = key b
    bindElementC store element compare

let bindPromiseStore<'T>  (p : ObservablePromise<'T>)
        (waiting : SutilElement)
        (result: 'T -> SutilElement)
        (fail : Exception -> SutilElement)
        : SutilElement =
    bindElement p <| (function
        | PromiseState.Waiting -> waiting
        | PromiseState.Result r -> result r
        | PromiseState.Error x -> fail x)

let bindPromise<'T>  (p : JS.Promise<'T>)
        (waiting : SutilElement)
        (result: 'T -> SutilElement)
        (fail : Exception -> SutilElement)
        : SutilElement =
    let x = ObservablePromise<'T>(p)
    //x.Run p
    bindPromiseStore x waiting result fail

type BindFn<'T> = IObservable<'T> -> ('T -> SutilElement) -> SutilElement
let private getInputChecked el = Interop.get el "checked"
let private setInputChecked (el : Node) (v:obj) = Interop.set el "checked" v
let private getInputValue el : string = Interop.get el "value"
let private setInputValue el (v:string) = Interop.set el "value" v

let bindSelected<'T when 'T : equality> (selection:IObservable<List<'T>>) (dispatch : List<'T> -> unit) =
    SutilElement.Define(
        "bindSelected",
        fun ctx ->

            let selectElement = ctx.ParentElement :?> HTMLSelectElement
            let selOps = selectElement.selectedOptions
            let op (coll:HTMLCollection) i = coll.[i] :?> HTMLOptionElement
            let opValue op : 'T = Interop.get op "__value"

            let getValueList() =
                [0..selOps.length-1] |> List.map (fun i -> opValue (op selOps i))

            let updateSelected (v : List<'T>) =
                let ops = selectElement.options
                for i in [0..ops.length-1] do
                    let o = op ops i
                    o.selected <- v |> List.contains (opValue o)

            let unsubInput = listen "input" selectElement <| fun _ ->
                getValueList() |> dispatch

            // We need to finalize checked status after all attrs have been processed for input,
            // in case 'value' hasn't been set yet
            EventListeners.once CustomEvents.ElementReady selectElement <| fun _ ->
                let unsub = selection |> Store.subscribe (updateSelected)
                SutilEffect.RegisterDisposable(ctx.Parent, unsub)

            SutilEffect.RegisterUnsubscribe(ctx.Parent,unsubInput)
            EffectedNode (ctx.ParentElement)

    )

let bindSelectMultiple<'T when 'T : equality> (store:IStore<List<'T>>)  =
    bindSelected store (fun sln -> store <~ sln)

let bindSelectSingle<'T when 'T : equality> (store:IStore<'T>) : SutilElement =
    bindSelected (store .> List.singleton) (fun sln -> sln |> List.exactlyOne |> Store.set store)

let bindSelectOptional<'T when 'T : equality> (store:IStore<'T option>) : SutilElement =
    let toList topt = match topt with |None -> []|Some t -> List.singleton t
    let fromList list = match list with |[] -> None |x::_ -> Some x
    bindSelected (store .> toList) (fun sln -> sln |> fromList |> Store.set store)

let private isNullString (obj:obj) =
    isNull obj || System.String.IsNullOrEmpty(downcast obj)

let private getId (s : IStore<'T>) = s.GetHashCode()

let bindGroup<'T> (store:Store<List<string>>) : SutilElement =
    SutilElement.Define( 
        "bindGroup",
        fun ctx ->
            let parent = ctx.ParentNode
            let name = match Interop.get parent "name" with
                        | s when isNullString s -> $"store-{getId store}"
                        | s -> s

            // Group this input with all other inputs that reference the same store
            Interop.set parent "name" name

            let getValueList() =
                let inputs = (documentOf parent).querySelectorAll(@$"input[name=""{name}""]")
                [0..(inputs.length-1)] |> List.map (fun i -> inputs.[i]) |> List.filter getInputChecked |> List.map getInputValue

            let updateChecked (v : List<string>) =
                setInputChecked parent ( v |> List.contains (getInputValue parent) )

            // Update the store when the radio box is clicked on
            let unsubInput = listen "input" parent <| fun _ ->
                getValueList() |> Store.set store

            // We need to finalize checked status after all attrs have been processed for input,
            // in case 'value' hasn't been set yet
            once CustomEvents.ElementReady parent <| fun _ ->
                store |> Store.get |> updateChecked

            // When store changes make sure check status is synced
            let unsub = store |> Store.subscribe (updateChecked)

            SutilEffect.RegisterDisposable(ctx.Parent,unsub)
            SutilEffect.RegisterUnsubscribe(ctx.Parent,unsubInput)
            EffectedNode (ctx.ParentElement)
    )

// T can realistically only be numeric or a string. We're relying (I think!) on JS's ability
// to turn a string into an int automatically in the Store.set call (maybe it's Fable doing that)
//
let bindRadioGroup<'T> (store:Store<'T>) : SutilElement =
    SutilElement.Define( "bindRadioGroup",
    fun ctx ->
    let parent = ctx.ParentNode
    let name = match Interop.get parent "name" with
                | s when isNullString s -> $"store-{getId store}"
                | s -> s
    // Group this input with all other inputs that reference the same store
    Interop.set parent "name" name

    let updateChecked (v : obj) =
        setInputChecked parent ( (string v) = getInputValue parent )

    // Update the store when the radio box is clicked on
    let inputUnsub = listen "input" parent <| fun _ ->
        Interop.get parent "value" |> Store.set store

    // We need to finalize checked status after all attrs have been processed for input,
    // in case 'value' hasn't been set yet
    once CustomEvents.ElementReady parent <| fun _ ->
        store |> Store.get |> updateChecked

    // When store changes make sure check status is synced
    let unsub = store |> Store.subscribe updateChecked

    SutilEffect.RegisterDisposable(ctx.Parent,unsub)
    SutilEffect.RegisterUnsubscribe(ctx.Parent,inputUnsub)

    EffectedNode (ctx.ParentElement) )

let bindClassToggle (toggle:IObservable<bool>) (classesWhenTrue:string) (classesWhenFalse:string) =
    bindSub toggle <| fun ctx active ->
        if active then
            ctx.ParentElement |> ClassHelpers.removeFromClasslist classesWhenFalse
            ctx.ParentElement |> ClassHelpers.addToClasslist classesWhenTrue
        else
            ctx.ParentElement |> ClassHelpers.removeFromClasslist classesWhenTrue
            ctx.ParentElement |> ClassHelpers.addToClasslist classesWhenFalse

let bindBoolAttr (toggle:IObservable<bool>) (boolAttr : string) =
    bindSub toggle <| fun ctx active ->
        match active with
        | true -> DomEdit.setAttribute ctx.ParentElement boolAttr boolAttr
        | false -> ctx.ParentElement.removeAttribute(boolAttr)

// Deprecated
let bindClass (toggle:IObservable<bool>) (classes:string) = bindClassToggle toggle classes ""

let bindClassNames (classNames:IObservable<#seq<string>>)  =
    bindSub classNames <| fun ctx current ->
        ctx.ParentElement.className <- ""
        ctx.ParentElement.classList.add( current |> Array.ofSeq )

let bindClassName (classNames:IObservable<string>)  =
    bindSub classNames <| fun ctx current ->
        ctx.ParentElement.className <- current

/// Bind a store value to an element attribute. Updates to the element are unhandled
let bindAttrIn<'T> (attrName:string) (store : IObservable<'T>) : SutilElement =
    SutilElement.Define("bindAttrIn",
    fun ctx ->
    let unsub =
        if attrName = "class" then
            store |> Store.subscribe (fun cls -> ctx.ParentElement.className <- (string cls))
        else
            store |> Store.subscribe (DomEdit.setAttribute ctx.ParentElement attrName)
    SutilEffect.RegisterDisposable(ctx.Parent,unsub)
    EffectedNode (ctx.ParentElement))

let bindAttrOut<'T> (attrName:string) (onchange : 'T -> unit) : SutilElement =
    SutilElement.Define( "bindAttrOut",
    fun ctx ->
    let parent = ctx.ParentNode
    let unsubInput = listen "input" parent <| fun _ ->
        Interop.get parent attrName |> onchange
    SutilEffect.RegisterUnsubscribe(ctx.Parent,unsubInput)
    EffectedNode (ctx.ParentElement) )

// Bind a scalar value to an element attribute. Listen for onchange events and dispatch the
// attribute's current value to the given function. This form is useful for view templates
// where v is invariant (for example, an each that already filters on the value of v, like Todo.Done)
let attrNotify<'T> (attrName:string) (value :'T) (onchange : 'T -> unit) : SutilElement =
    SutilElement.Define( "attrNotify",
    fun ctx ->
    let parent = ctx.ParentNode
    let unsubInput = listen "input" parent  <| fun _ ->
        Interop.get parent attrName |> onchange
    Interop.set parent attrName value
    SutilEffect.RegisterUnsubscribe(ctx.Parent, unsubInput)
    EffectedNode (ctx.ParentElement) )

// Bind an observable value to an element attribute. Listen for onchange events and dispatch the
// attribute's current value to the given function
let bindAttrBoth<'T> (attrName:string) (value : IObservable<'T>) (onchange : 'T -> unit) : SutilElement =
    fragment [
        bindAttrIn attrName value
        bindAttrOut attrName onchange
    ]

let bindListen<'T> (attrName:string) (store : IObservable<'T>) (event:string) (handler : Event -> unit) : SutilElement =
    SutilElement.Define( "bindListen",
    fun ctx ->
    let parent = ctx.ParentNode
    let unsubA = listen event parent handler
    let unsubB = store |> Store.subscribe ( Interop.set parent attrName )
    SutilEffect.RegisterUnsubscribe(ctx.Parent,unsubA)
    SutilEffect.RegisterDisposable(ctx.Parent,unsubB)
    EffectedNode (ctx.ParentElement) )

// Bind a store value to an element attribute. Listen for onchange events write the converted
// value back to the store
let private bindAttrConvert<'T> (attrName:string) (store : IStore<'T>) (convert : obj -> 'T) : SutilElement =
    SutilElement.Define( "bindAttrConvert",
    fun ctx ->
    let parent = ctx.ParentNode
    //let attrName' = if attrName = "value" then "__value" else attrName
    let unsubInput = listen "input" parent <| fun _ ->
        Interop.get parent attrName |> convert |> Store.set store
    let unsub = store |> Store.subscribe ( Interop.set parent attrName )
    SutilEffect.RegisterUnsubscribe(parent,unsubInput)
    SutilEffect.RegisterDisposable(parent,unsub)
    EffectedNode (ctx.ParentElement) )

// Unsure how to safely convert Element.getAttribute():string to 'T
let private convertObj<'T> (v:obj) : 'T  =
    v :?> 'T

// Bind a store to an attribute in both directions
let bindAttrStoreBoth<'T> (attrName:string) (store : IStore<'T>) =
    bindAttrConvert attrName store convertObj<'T>

let bindAttrStoreOut<'T> (attrName:string) (store : Store<'T>) : SutilElement =
    SutilElement.Define( "bindAttrStoreOut",
    fun ctx ->
    let parent = ctx.ParentNode
    let unsubInput = listen "input" parent <| fun _ ->
        Interop.get parent attrName |> convertObj<'T> |> Store.set store
    //(asEl parent).addEventListener("input", (fun _ -> Interop.get parent attrName |> convertObj<'T> |> Store.set store ))
    SutilEffect.RegisterUnsubscribe(ctx.Parent,unsubInput)
    EffectedNode (ctx.ParentElement)
    )

let private attrIsSizeRelated  (attrName:string) =
    let upr = attrName.ToUpper()
    upr.IndexOf("WIDTH") >= 0 || upr.IndexOf("HEIGHT") >= 0

let listenToProp<'T> (attrName:string) (dispatch: 'T -> unit) : SutilElement =
    SutilElement.Define( sprintf "listenToProp %s" attrName,
    fun ctx ->
    let parent = ctx.ParentNode
    let notify() = Interop.get parent attrName |> convertObj<'T> |> dispatch

    once CustomEvents.ElementReady parent <| fun _ ->
        if attrIsSizeRelated attrName then
            SutilEffect.RegisterDisposable(parent,(ResizeObserver.getResizer (downcast parent)).Subscribe( notify ))
        else
            SutilEffect.RegisterUnsubscribe(parent, listen "input" parent (fun _ -> notify()))

        rafu notify
    EffectedNode (ctx.ParentElement))

let bindPropOut<'T> (attrName:string) (store : Store<'T>) : SutilElement =
    listenToProp attrName (Store.set store)

type KeyedStoreItem<'T,'K> = {
    Key : 'K
    Node : Node //SutilEffect
    SvId : int
    Position : IStore<int>
    Value: IStore<'T>
}

let private findCurrentNode doc (current:Node) (id:int) =
    if (isNull current || isNull current.parentNode) then
        if logEnabled() then log($"each: Find node with id {id}")
        match Id.findNodeWithId doc id with
        | None ->
            if logEnabled() then log("each: Disaster: cannot find node")
            null
        | Some n ->
            if logEnabled() then log($"each: Found it: {n}")
            n
    else
        //log($"Cannot find node with id {id}")
        current

let private findCurrentElement doc (current:Node) (id:int) =
    let node = findCurrentNode doc current id
    match node with
    | null -> null
    | n when TypeHelpers.isElementNode n -> n :?> HTMLElement
    | x ->  if logEnabled() then log $"each: Disaster: found node but it's not an HTMLElement"
            null

let private genEachId = Helpers.createIdGenerator()

type EachItemRenderer<'T> =
    | Static of ('T -> SutilElement)
    | StaticIndexed of (int * 'T -> SutilElement)
    // | LiveStore of (IReadOnlyStore<'T> -> SutilElement)
    // | Live of (IObservable<'T> -> SutilElement)
    // | LiveIndexed of (IObservable<int> * IObservable<'T> -> SutilElement)

let private eachItemRender (renderer : EachItemRenderer<'T>) (index : int) (item : 'T) : SutilElement =
    match renderer with 
    | Static v -> v (item)
    | StaticIndexed v -> v (index, item)
    // | Live v -> v item
    // | LiveStore v -> v item
    // | LiveIndexed v -> v (index,item)


let eachiko_wrapper (source:IObservable<ICollectionWrapper<'T>>) (view : EachItemRenderer<'T>) (key:int*'T->'K)  : SutilElement =
//    Bind.el( source, (Array.map view >>  SutilElement.Fragment) )
    bindElementC 
        source 
        (fun (items : ICollectionWrapper<'T>) -> 
            SutilElement.Fragment( items.Mapi( eachItemRender view ).ToArray() )
            )
        (fun _ _ -> false)

let private duc = Observable.distinctUntilChanged

let eachiko = eachiko_wrapper

let each (items:IObservable<ICollectionWrapper<'T>>) (view : 'T -> SutilElement)  =
    //eachiko_wrapper items (fun (_,item) -> bindElement (duc item) view) (fun (i,v) -> i,v.GetHashCode()) trans
    eachiko_wrapper items (Static view) (fun (i,v) -> i,v.GetHashCode()) 

let eachi (items:IObservable<ICollectionWrapper<'T>>) (view : (int*'T) -> SutilElement)   : SutilElement =
    //eachiko items (fun (index,item) -> bindElement2 (duc index) (duc item) view) fst trans
    eachiko items (StaticIndexed view) fst 

// let eachio (items:IObservable<ICollectionWrapper<'T>>) (view : (IObservable<int>*IObservable<'T>) -> SutilElement)   =
//     //eachiko items view fst trans
//     eachiko items (LiveIndexed view) fst trans

let eachk (items:IObservable<ICollectionWrapper<'T>>) (view : 'T -> SutilElement)  (key:'T -> 'K)  =
    eachiko
        items
        //(fun (_,item) -> bindElement (duc item) view)
        (Static view)
        (snd>>key)

open Browser.CssExtensions

let bindStyle<'T> (value : IObservable<'T>) (f : CSSStyleDeclaration -> 'T -> unit) =
    SutilElement.Define( "bindStyle",
    fun ctx ->
    let style = ctx.ParentElement.style
    let unsub = value.Subscribe(f style)
    SutilEffect.RegisterDisposable( ctx.Parent, unsub )
    EffectedNode (ctx.ParentElement) )

let bindElementEffect<'T, 'E when 'E :> HTMLElement> (value : IObservable<'T>) (f : 'E -> 'T -> unit) =
    SutilElement.Define( "bindElementEffect",
    fun ctx ->
    let el = ctx.ParentElement :?> 'E
    let unsub = value.Subscribe(f el)
    SutilEffect.RegisterDisposable( ctx.Parent, unsub )
    EffectedNode (ctx.ParentElement) )

let bindWidthHeight (wh: IObservable<float*float>) =
    bindStyle wh (fun style (w,h) ->
        if w <> 0.0 && h <> 0.0 then
            style.width <- w.ToString() + "px"
            style.height <- h.ToString() + "px"
    )

let bindLeftTop (xy : IObservable<float*float>) =
    bindStyle xy (fun style (x,y) ->
        if x <> 0.0 && y <> 0.0 then
            style.left <- x.ToString() + "px"
            style.top <- y.ToString() + "px"
    )

let (|=>) store element = bindElement store element

let cssAttrsToString (cssAttrs) =
    cssAttrs |> Seq.map (fun (n,v) -> $"{n}: {v};") |> String.concat ""

let listWrap( list : 'T list ) = list.ToCollectionWrapper()
let listWrapO (list : IObservable<'T list>) = list |> Store.map listWrap

let arrayWrap( arr : 'T array ) = arr.ToCollectionWrapper()
let arrayWrapO (arr : IObservable<'T array>) = arr |> Store.map arrayWrap
