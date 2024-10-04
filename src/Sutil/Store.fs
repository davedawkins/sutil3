module Sutil.Store 

type 'T observable = System.IObservable<'T>

open System
open Browser.Dom
open Microsoft.FSharp.Core
open System.Collections.Generic
open Sutil.Dom
open Sutil.Dom.Types

///  <exclude />
type IReadOnlyStore<'T> =
    inherit IObservable<'T>
    inherit IDisposable
    abstract Value : 'T
    abstract OnDispose: (unit -> unit) -> unit

type IStore<'T> =
    inherit IReadOnlyStore<'T>
    abstract Update : f: ('T -> 'T) -> unit
    abstract Name : string with get, set
    
///  <exclude />
type Update<'Model> = ('Model -> 'Model) -> unit // A store updater. Store updates by being passed a model updater

let private logEnabled() = false //Logging.isEnabled "store"
let private log s = () // Logging.log "store" s

type StoreCons<'Model, 'Store> = 
    (unit -> 'Model) -> ('Model -> unit) -> 'Store * Update<'Model>

#if FABLE_COMPILER
open Fable.Core

[<Emit("$0 === $1")>]
let private fastEquals (x: 'T) (y: 'T): bool = jsNative
#else
let private fastEquals (x: 'T) (y: 'T): bool = Unchecked.equals x y
#endif

let private nextStoreId() = Helpers.createIdGenerator()

/// <summary>
/// Helper functions for <c>IObservables</c>
/// </summary>
[<RequireQualifiedAccess>]
module Observable =

    [<AbstractClass>]
    type BasicObserver<'T>() =

        let mutable stopped = false
        abstract Next : value : 'T -> unit
        abstract Error : error : exn -> unit
        abstract Completed : unit -> unit

        interface IObserver<'T> with
            member x.OnNext value =
                if not stopped then x.Next value
            member x.OnError e =
                if not stopped then stopped <- true; x.Error e
            member x.OnCompleted () =
                if not stopped then stopped <- true; x.Completed ()

    let map2<'A, 'B, 'Res> (f: 'A -> 'B -> 'Res) (a: IObservable<'A>) (b: IObservable<'B>) : IObservable<'Res> =
        { new IObservable<'Res> with
            member _.Subscribe(h: IObserver<'Res>) =
                let mutable valueA, valueB = None, None

                let notify() =
                     if valueA.IsSome && valueB.IsSome then h.OnNext (f valueA.Value valueB.Value)

                let disposeA = a.Subscribe(fun v -> valueA <- Some v; notify())
                let disposeB = b.Subscribe(fun v -> valueB <- Some v; notify())

                Dispose.makeDisposable(Unsubscribable.Of (fun _ -> disposeA.Dispose(); disposeB.Dispose()))
        }

    let zip<'A,'B> (a:IObservable<'A>) (b:IObservable<'B>) : IObservable<'A*'B> =
        map2<'A, 'B, 'A * 'B> (fun a b -> a, b) a b

    let distinctUntilChangedCompare<'T> (eq:'T -> 'T -> bool) (source:IObservable<'T>) : IObservable<'T> =
        { new System.IObservable<'T> with
            member _.Subscribe( h : IObserver<'T> ) =
                let mutable value = Unchecked.defaultof<'T>
                let mutable init = false

                // For Fable: isNull(unbox(None)) = true
                // Can't use Unchecked.defaultof<'T> as meaning "init = false"
                let safeEq next = init && eq value next

                let disposeA = source.Subscribe( fun next ->
                    if not (safeEq next) then
                        h.OnNext next
                        value <- next
                        init <- true
                )

                Dispose.makeDisposable ( 
                    Unsubscribe (fun _ -> disposeA.Dispose()  )
                )
        }

    let distinctUntilChanged<'T when 'T : equality> (source:IObservable<'T>) : IObservable<'T> =
        source |> distinctUntilChangedCompare (=)

    /// Provide the initial value for a sequence so that new subscribers will receive an 
    /// immediate update of the current value
    let init (v : 'T) (source: IObservable<'T>) =
        let mutable current = v
        { new System.IObservable<'T> with
            member _.Subscribe( h : IObserver<'T> ) =
            
                let notify() =
                    try h.OnNext (current)
                    with ex -> h.OnError ex

                let disposeA = source.Subscribe( fun x ->
                    current <- x
                    notify()
                )

                notify()

                Dispose.makeDisposable ( (fun _ -> disposeA.Dispose()) |> Unsubscribe )
        }


    /// Determines whether an observable sequence contains a specified value
    /// which satisfies the given predicate
    let exists predicate (source: IObservable<'T>) =
        { new System.IObservable<'T> with
            member _.Subscribe( h : IObserver<'T> ) =
                let disposeA = source.Subscribe( fun x ->
                    try h.OnNext (predicate x)
                    with ex -> h.OnError ex
                )
                Dispose.makeDisposable ((fun _ -> disposeA.Dispose()) |> Unsubscribe )
        }

    /// Filters the observable elements of a sequence based on a predicate
    let filter predicate (source: IObservable<'T>) =
        { new System.IObservable<'T> with
            member _.Subscribe( h : IObserver<'T> ) =
                let disposeA = source.Subscribe( fun x ->
                    try if predicate x then h.OnNext x
                    with ex -> h.OnError ex
                )
                Dispose.makeDisposable ((fun _ -> disposeA.Dispose()) |> Unsubscribe )
        }

    //let choose (f : 'T option -> 'R option) (source:IObservable<'T option>) : IObservable<'R> =
    //    { new System.IObservable<_> with
    //        member _.Subscribe( h : IObserver<_> ) =
    //            let disposeA = source.Subscribe( fun x ->
    //                (try f x with ex -> h.OnError ex;None) |> Option.iter h.OnNext
    //            )
    //            Dispose.makeDisposable (fun _ -> disposeA.Dispose() )
    //    }

// Allow stores that can handle mutable 'Model types (eg, <input>.FileList). In this
// case we can pass (fun _ _ -> true)
type Store<'Model>(init: unit -> 'Model, dispose: 'Model -> unit) =
    let mutable uid = 0
    let storeId = nextStoreId()
    let mutable name = "store-" + (string storeId)
    let mutable _modelInitialized = false
    let mutable _model = Unchecked.defaultof<_>
    let mutable disposeListeners : (unit -> unit) list = []

    let onDispose f = disposeListeners <- f :: disposeListeners

    let notifyDispose() =
        disposeListeners |> List.iter (fun f -> f())
        disposeListeners <- []

    let model() =
        if not _modelInitialized then
            _model <- init()
            _modelInitialized <- true
        _model
    let subscribers =
        Collections.Generic.Dictionary<_, IObserver<'Model>>()

    override _.ToString() = $"#{storeId}={_model}"

    member _.Value = model()

    member this.Update(f: 'Model -> 'Model) =
        let newModel = f (model())

        // Send every update. Use 'distinctUntilChanged' with fastEquals to get previous behaviour
        //Fable.Core.JS.console.log($"Update {_model} -> {newModel}")
        if not (fastEquals _model newModel) then
            _model <- newModel

            if subscribers.Count > 0 then
                subscribers.Values
                    |> Seq.iter (fun s -> s.OnNext(_model))

    member this.NumSubscribers = subscribers.Count

    member this.Subscribe(observer: IObserver<'Model>): IDisposable =
        let id = uid
        uid <- uid + 1

        if logEnabled() then log $"subscribe {id}"

        subscribers.Add(id, observer)

        // TODO: Is this the right way to report the model to the subscriber immediately?
        //Fable.Core.JS.setTimeout (fun _ -> observer.OnNext(model)) 0 |> ignore

        // Sutil depends on an immediate callback
        observer.OnNext(model())

        Dispose.makeDisposable( Unsubscribe ( fun () ->
            if logEnabled() then log $"unsubscribe {id}"
            subscribers.Remove(id) |> ignore
        ))

    member this.Name with get() = name and set (v) = name <- v

    member this.OnDispose( f : unit -> unit ) =
        onDispose f

    member this.Dispose() =
        subscribers.Values |> Seq.iter (fun x -> x.OnCompleted())
        subscribers.Clear()
        dispose (model())
        _model <- Unchecked.defaultof<_>
        //Registry.notifyDisposeStore this
        notifyDispose()

    interface IStore<'Model> with
        member this.Subscribe(observer: IObserver<'Model>) = this.Subscribe(observer)
        member this.Update(f) = this.Update(f)
        member this.Value = this.Value
        member this.OnDispose( f : unit -> unit ) = this.OnDispose(f)
        member this.Name with get() = this.Name and set (v:string) = this.Name <- v
        // member this.Debugger = {
        //         new IStoreDebugger with
        //             member _.Value = upcast this.Value
        //             member _.NumSubscribers = subscribers.Count }

    interface IDisposable with
        member this.Dispose() = this.Dispose()

let countSubscribers (store : IStore<'T>) = (store :?> Store<'T>).NumSubscribers 

let makeStore<'Model> (init:unit->'Model) (dispose:'Model->unit) =
    new Store<'Model>(init,dispose)

/// <summary>
/// Create a new store
/// </summary>
/// <example>
/// <code lang="fsharp">
///     let intStore: IStore&lt;int&gt; = Store.make 1
///
///     let anonymousStore:
///         IStore&lt;{| prop1: int;
///                   prop2: string option |}&gt;
///         = Store.make {| prop1 = 10; prop2 = None |}
///     (* After using the store *)
///     intStore.Dispose()
///     anonymousStore.Dispose()
/// </code>
/// </example>
let make (modelInit: 'T) : IStore<'T> =
    let init () = modelInit
    let s = makeStore init ignore
    upcast s

/// <summary>
/// Obtains the current value of the store
/// </summary>
/// <example><code>
///     let value = Store.get initStore
///     value = 1 // true
///     let value2 = Store.get anonymousStore
///     Option.isNone value2.prop2 // true
/// </code></example>
let get (store: IStore<'T>) : 'T = store.Value

/// <summary>
/// Replaces the current value of the store
/// </summary>
/// <example><code>
///     Store.set 2 intStore
///     let value = Store.get intStore
///     value = 1 // false
/// </code></example>
let set (store: IStore<'T>) newValue : unit = store.Update(fun _ -> newValue)

/// <summary>
/// Provides a subscription that invokes a callback
/// every time the store value is updated
/// </summary>
/// <example>
/// <code>
///     let subscription =
///         Store.subscribe (fun value -> printfn $"{value}") intStore
///
///     (* after you are done with the subscription *)
///
///     subscription.Dispose()
/// </code>
/// </example>
let subscribe (callback: 'T -> unit) (store: IObservable<'T>) = store.Subscribe(callback)

/// <summary>
/// Returns an observable that will resolve to the result of said callback
/// </summary>
/// <example>
/// <code>
///     let subscription: IObservable&lt;string&gt; =
///         Store.map (fun value -> $"{value}") intStore
///
///     (* after you are done with the subscription *)
///
///     subscription.Dispose()
/// </code>
/// </example>
let map<'A, 'B> (callback: 'A -> 'B) (store: IObservable<'A>) = store |> Observable.map callback

/// <summary>
/// Returns an observable that will resolve to the result of said callback applied to two observables
/// </summary>
/// <example>
/// <code>
///     let subscription: IObservable&lt;int&gt; =
///         Store.map2 (fun value1 value2 -> value1 * value2) intStore1 intStore2
///
///     (* after you are done with the subscription *)
///
///     subscription.Dispose()
/// </code>
/// </example>
let map2<'A, 'B, 'Res> (callback: 'A -> 'B -> 'Res) (storeA: IObservable<'A>) (storeB: IObservable<'B>) = Observable.map2<'A, 'B, 'Res> callback storeA storeB

/// <summary>
/// Applies a predicate function to obtain an observable of the elements that evaluated to true
/// </summary>
/// <example>
/// <code>
///     let usersOver18: IObservable&lt;string&gt; =
///         Store.filter (fun user -> user.age > 18) usersStore
///
///     (* after you are done with the subscription *)
///
///     over18.Dispose()
/// </code>
/// </example>
let filter<'A> (predicate: 'A -> bool) (store: IObservable<'A>) = store |> Observable.filter predicate

/// <summary>
/// Provides an observable that will emit a value only when the updated store value is different from the previous one
/// </summary>
/// <example>
/// <code>
///     let store = Store.make 0
///     let whenDistinct = Store.distinct store
///     let sub1 = store.subscribe(printfn "number: %i")
///     let sub2 = whenDistinct.subscribe(printfn "number: %i")
///     Store.set 0 store // store emits
///     Store.set 0 store // when distinct doesn't emit
///     Store.set 1 store // both store and distinct emit
///     Store.set 1 store // when distinct doesn't emit
/// </code>
/// </example>
let distinct<'T when 'T: equality> (source: IObservable<'T>) = Observable.distinctUntilChanged source

/// <summary>
/// Helper function for commonly used form
/// <code>
///     store |> Store.map (fun s -> s.Thing) |> Observable.distinctUntilChanged
/// </code>
/// </summary>
let mapDistinct (callback: 'A -> 'B) (store: IObservable<'A>) =
    store |> map callback |> distinct

/// <summary>
/// Merges two stores into a single tupled observable
/// </summary>
/// <example>
/// <code>
///     let tableInfo =
///     Observable.zip
///         (Strore.map(fun model -> model.rows) model)
///         (Strore.map(fun model -> model.columns) model)
///
///     (* once done with tableInfo *)
///
///     tableInfo.Dispose()
/// </code>
/// </example>
let zip source1 source2 = Observable.zip source1 source2

let current (store: IObservable<'T>) =
    let mutable value = Unchecked.defaultof<'T>
    store.Subscribe(fun v -> value <- v).Dispose() // Works only when root observable is a store, or root responds immediately upon subscription
    value

/// <summary>
/// Takes a store and applies a mapping function then returns the value from the evaluated function
/// </summary>
/// <remarks>
/// This might be called foldMap
/// </remarks>
/// <example>
/// <code>
///     let store: IStore&lt;{| name: string; budget: decimal |}> =
///     Store.make {| name = "Frank"; budget = 547863.26M |}
///
///     let formattedBudget: string =
///         Store.getMap
///             (fun model -> sprintf $"$ %0.00M{model.budget}")
///             store
///     printf %"Budget available: {formattedBudget}
/// </code>
/// </example>
let getMap (mapper : 'T -> 'U) (store : IStore<'T>) = store |> get |> mapper

/// <summary>
/// Invoke callback for each observed value from source
/// </summary>
/// <example>
/// <code>
///    source |> Store.iter (fun v -> printfn $"New value: {v}")
/// </code>
/// </example>
let iter<'A> (callback: 'A -> unit) (source: IObservable<'A>) =
    subscribe callback source

[<Obsolete("Use Store.iter instead")>]
let write<'A> (callback: 'A -> unit) (store: IObservable<'A>) = iter callback store |> ignore

/// <summary>Modify the store by mapping its current value with a callback</summary>
/// <example>
/// <code>
///     let store: IStore&lt;int> = Store.make 2
///
///     let squareMe() =
///         Store.modify (fun model -> model * model) store
///
///     Html.div [
///         bindFragment store &lt;| fun model -> text $"The value is {model}"
///         Html.button [
///             onClick (fun _ -> squareMe()) []
///             text "Square me"
///         ]
///     ]
/// </code>
/// </example>


let modify (callback: ('T -> 'T)) (store: IStore<'T>) = store |> getMap callback |> set store


/// <summary>
/// Takes two observables and subscribes to both with a single callback,
/// both values will be cached individually and
/// on every notify they will be updated and emitted,
/// every notification can come from any of the observables
/// </summary>
/// <example>
/// <code>
///     let player1Score = Store.make 0
///     let player2Score = Store.make 0
///
///     let printPlayerScores (score1: int * score2: int) =
///         printfn $"Player 1: {score1}\nPlayer2: {score2}"
///
///     let scores =
///         Store.subscribe2
///             player1Score
///             player2Score
///             printPlayerScore
///     (* Game Finished, dispose the observables *)
///     scores.Dispose()
/// </code>
/// </example>
let subscribe2<'A, 'B>
    (source1: IObservable<'A>)
    (source2: IObservable<'B>)
    (callback: ('A * 'B) -> unit)
    : System.IDisposable =
    // Requires that subscribe makes an initializing first callback. Otherwise, there will
    // be a missing value for A (say) when B (say) sends an update.
    let mutable initState = 0

    let mutable cachea : 'A = Unchecked.defaultof<'A>
    let mutable cacheb : 'B = Unchecked.defaultof<'B>

    let notify () =
        if initState = 2 then
            callback (cachea, cacheb)

    let unsuba =
        source1
        |> subscribe
            (fun v ->
                if initState = 0 then initState <- 1
                cachea <- v
                notify ())

    let unsubb =
        source2
        |> subscribe
            (fun v ->
                if initState = 1 then initState <- 2
                cacheb <- v
                notify ())

    if (initState <> 2) then
        console.log ("Error: subscribe didn't initialize us")
        failwith "Subscribe didn't initialize us"

    (fun () ->
        unsuba.Dispose()
        unsubb.Dispose()) |> Unsubscribe |> Dispose.makeDisposable

/// <summary>
/// Operators for store functions
/// </summary>
[<AutoOpen>]
module StoreOperators =

    /// <summary>
    /// Alias for <c>Store.getMap</c>, takes a store and applies a mapping function then returns the value from the evaluated function
    /// </summary>
    /// <remarks>
    /// This might be called foldMap
    /// </remarks>
    /// <example>
    /// <code>
    ///     let store: IStore&lt;{| name: string; budget: decimal |}> =
    ///     Store.make {| name = "Frank"; budget = 547863.26M |}
    ///
    ///     let formattedBudget: string =
    ///         store |-> (fun model -> sprintf $"$ %0.00M{model.budget}")
    ///     printf %"Budget available: {formattedBudget}
    ///  </code>
    ///  </example>
    //let (|%>) s f = Store.map f s
    let (|->) s f = getMap f s

    /// <summary>
    /// Alias for <c>Store.map</c>, returns an observable that will resolve to the result of said callback
    /// </summary>
    /// <example>
    /// <code>
    ///     let subscription: IObservable&lt;string&gt; =
    ///         intStore .> (fun value -> $"{value}")
    ///
    ///     (* after you are done with the subscription *)
    ///
    ///     subscription.Dispose()
    /// </code>
    /// </example>
    let (.>) s f = map f s

    /// <summary>
    /// Alias for <c>Store.mapDistinct</c>
    /// </summary>
    let (.>>) s f = mapDistinct f s

    /// <summary>
    /// Alias for <c>Store.set</c>,  replaces the current value of the store
    /// </summary>
    /// <example>
    /// <code>
    ///     intStore &lt;~ 2
    ///     let value = Store.get intStore
    ///     value = 1 // false
    /// </code>
    /// </example>
    let (<~) s v = set s v

    /// <summary>
    /// Alias for <c>Store.set</c>, replaces the current value of the store
    /// </summary>
    /// <example>
    /// <code>
    ///     intStore &lt;~- 2
    ///     let value = Store.get intStore
    ///     value = 1 // false
    /// </code>
    /// </example>
    let (<~-) s v = set s v

    /// <summary>
    /// Alias for <c>Store.set</c>,  replaces the current value of the store
    /// </summary>
    /// <example>
    /// <code>
    ///     2 -~> intStore
    ///     let value = Store.get intStore
    ///     value = 1 // false
    /// </code>
    /// </example>
    let (-~>) v s = set s v

    /// <summary>
    /// Alias for <c>Store.modify</c>. Modify the store by mapping its current value with a callback
    /// </summary>
    /// <example>
    /// <code>
    ///     let store: IStore&lt;int> = Store.make 2
    ///
    ///     let squareMe() =
    ///         store &lt;~= (fun model -> model * model)
    ///
    ///     Html.div [
    ///         bindFragment store &lt;| fun model -> text $"The value is {model}"
    ///         Html.button [
    ///             onClick (fun _ -> squareMe()) []
    ///             text "Square me"
    ///         ]
    ///     ]
    /// </code>
    /// </example>
    let (<~=) store map = modify map store

    /// <summary>
    /// Alias for <c>Store.modify</c> Modify the store by mapping its current value with a callback
    /// </summary>
    /// <example>
    /// <code>
    ///     let store: IStore&lt;int> = Store.make 2
    ///
    ///     let squareMe() =
    ///         (fun model -> model * model) =~> store
    ///
    ///     Html.div [
    ///         bindFragment store &lt;| fun model -> text $"The value is {model}"
    ///         Html.button [
    ///             onClick (fun _ -> squareMe()) []
    ///             text "Square me"
    ///         ]
    ///     ]
    /// </code>
    /// </example>
    let (=~>) map store = modify map store



/// <exclude/>
[<AutoOpen>]
module StoreExtensions =

    // No uses of this in the Sutil code base.
    //
    // One issue is that 'u' should be cleaned up somewhere, so perhaps turning this into
    // a SutilElement would improve it?
    // Appears to return a store of indexes, whose value will always be the first observable
    // that is currently <c>true</c>
    //
    let firstOf (selectors: IObservable<bool> list) =
        let matches = new HashSet<int>()
        let mutable current = -1
        let s = make current

        let setMatch i state =
            if state then
                matches.Add(i) |> ignore
            else
                matches.Remove(i) |> ignore

        let scan () =
            let next =
                matches
                |> Seq.fold (fun a i -> if a < 0 || i < a then i else a) -1

            if (next <> current) then
                s <~ next
                current <- next

        selectors
        |> List.iteri
            (fun i pred ->
                let u =
                    pred.Subscribe
                        (fun state ->
                            setMatch i state
                            scan ())

                ())

        s


/// <summary>
/// Support for <c>IObservable&lt;Promise&lt;T>></c>
/// </summary>
[<AutoOpen>]
module ObservablePromise =

    [<RequireQualifiedAccess>]
    type PromiseState<'T> =
        | Waiting
        | Result of 'T
        | Error of Exception

    type ObservablePromise<'T>(p : JS.Promise<'T>) =
        let store = make PromiseState.Waiting
        // TODO: Clean up store

        let run () =
                store <~ PromiseState.Waiting
                p |> Promise.map (fun v -> store <~ PromiseState.Result v)
                  |> Promise.catch (fun x -> store <~ PromiseState.Error x)
                  |> ignore

        do
            run()

        interface IObservable<PromiseState<'T>> with
            member this.Subscribe(observer: IObserver<PromiseState<'T>>) = store.Subscribe(observer)

    type JS.Promise<'T> with
        member self.ToObservable() : ObservablePromise<'T> =
            ObservablePromise<'T>(self)

