namespace Sutil

module Elmish =

    open Sutil
    open System
    open Browser.Dom
    open Browser.Types

    module Cmd =

        open System

        // Ported from https://github.com/elmish/elmish

        /// Dispatch - feed new message into the processing loop
        type Dispatch<'Msg> = 'Msg -> unit // Message dispatcher

        /// Effect - return immediately, but may schedule dispatch of a message at any time
        type Effect<'msg> = Dispatch<'msg> -> unit

        /// Cmd - container for effects that may produce messages
        type Cmd<'Msg> = Effect<'Msg> list // List of commands. A command needs a dispatcher to execute

        //
        // All Cmd code take from Fable.Elmish/src/cmd.fs, by Maxel Mangime
        // TODO: Refactor this into Sutil.Elmish module
        //
#if FABLE_COMPILER
        /// <exclude/>
        module internal Timer =
            open System.Timers

            let delay interval callback =
                let t = new Timer(float interval, AutoReset = false)

                t.Elapsed.Add callback
                t.Enabled <- true
                t.Start()
#endif

        let none: Cmd<'Msg> = []

        /// Command to call the effect
        let ofEffect (effect: Effect<'msg>) : Cmd<'msg> =
            [
                effect
            ]

        let map (f: 'MsgA -> 'MsgB) (cmd: Cmd<'MsgA>) : Cmd<'MsgB> =
            cmd |> List.map (fun g -> (fun dispatch -> f >> dispatch) >> g)

        let ofMsg msg : Cmd<'Msg> =
            [
                fun d -> d msg
            ]

        let batch (cmds: Cmd<'Msg> list) : Cmd<'Msg> = cmds |> List.concat

        module OfFunc =
            let either (task: 'args -> _) (a: 'args) (success: _ -> 'msg') (error: _ -> 'msg') =
                [
                    fun d ->
                        try
                            task a |> (success >> d)
                        with x ->
                            x |> (error >> d)
                ]

            let perform (task: 'args -> _) (a: 'args) (success: _ -> 'msg') =
                [
                    fun d ->
                        try
                            task a |> (success >> d)
                        with _ ->
                            ()
                ]

            let attempt (task: 'args -> unit) (a: 'args) (error: _ -> 'msg') =
                [
                    fun d ->
                        try
                            task a
                        with x ->
                            x |> (error >> d)
                ]

            let exec (task: 'args -> _) (a: 'args) =
                [
                    fun d ->
                        try
                            task a
                        with _ ->
                            ()
                ]

        module OfAsyncWith =
            /// Command that will evaluate an async block and map the result
            /// into success or error (of exception)
            let either
                (start: Async<unit> -> unit)
                (task: 'a -> Async<_>)
                (arg: 'a)
                (ofSuccess: _ -> 'msg)
                (ofError: _ -> 'msg)
                : Cmd<'msg>
                =
                let bind dispatch =
                    async {
                        let! r = task arg |> Async.Catch

                        dispatch (
                            match r with
                            | Choice1Of2 x -> ofSuccess x
                            | Choice2Of2 x -> ofError x
                        )
                    }

                [
                    bind >> start
                ]

            /// Command that will evaluate an async block and map the success
            let perform
                (start: Async<unit> -> unit)
                (task: 'a -> Async<_>)
                (arg: 'a)
                (ofSuccess: _ -> 'msg)
                : Cmd<'msg>
                =
                let bind dispatch =
                    async {
                        let! r = task arg |> Async.Catch

                        match r with
                        | Choice1Of2 x -> dispatch (ofSuccess x)
                        | _ -> ()
                    }

                [
                    bind >> start
                ]

            /// Command that will evaluate an async block and map the error (of exception)
            let attempt
                (start: Async<unit> -> unit)
                (task: 'a -> Async<_>)
                (arg: 'a)
                (ofError: _ -> 'msg)
                : Cmd<'msg>
                =
                let bind dispatch =
                    async {
                        let! r = task arg |> Async.Catch

                        match r with
                        | Choice2Of2 x -> dispatch (ofError x)
                        | _ -> ()
                    }

                [
                    bind >> start
                ]

            /// Command that will evaluate an async block to the message
            let result (start: Async<unit> -> unit) (task: Async<'msg>) : Cmd<'msg> =
                let bind dispatch =
                    async {
                        let! r = task
                        dispatch r
                    }

                [
                    bind >> start
                ]

        module OfAsync =
#if FABLE_COMPILER
            let start x =
                Timer.delay 0 (fun _ -> Async.StartImmediate x)
#else
            let inline start x = Async.Start x
#endif
            /// Command that will evaluate an async block and map the result
            /// into success or error (of exception)
            let inline either
                (task: 'a -> Async<_>)
                (arg: 'a)
                (ofSuccess: _ -> 'msg)
                (ofError: _ -> 'msg)
                : Cmd<'msg>
                =
                OfAsyncWith.either start task arg ofSuccess ofError

            /// Command that will evaluate an async block and map the success
            let inline perform (task: 'a -> Async<_>) (arg: 'a) (ofSuccess: _ -> 'msg) : Cmd<'msg> =
                OfAsyncWith.perform start task arg ofSuccess

            /// Command that will evaluate an async block and map the error (of exception)
            let inline attempt (task: 'a -> Async<_>) (arg: 'a) (ofError: _ -> 'msg) : Cmd<'msg> =
                OfAsyncWith.attempt start task arg ofError

            /// Command that will evaluate an async block to the message
            let inline result (task: Async<'msg>) : Cmd<'msg> = OfAsyncWith.result start task

        module OfAsyncImmediate =
            /// Command that will evaluate an async block and map the result
            /// into success or error (of exception)
            let inline either
                (task: 'a -> Async<_>)
                (arg: 'a)
                (ofSuccess: _ -> 'msg)
                (ofError: _ -> 'msg)
                : Cmd<'msg>
                =
                OfAsyncWith.either Async.StartImmediate task arg ofSuccess ofError

            /// Command that will evaluate an async block and map the success
            let inline perform (task: 'a -> Async<_>) (arg: 'a) (ofSuccess: _ -> 'msg) : Cmd<'msg> =
                OfAsyncWith.perform Async.StartImmediate task arg ofSuccess

            /// Command that will evaluate an async block and map the error (of exception)
            let inline attempt (task: 'a -> Async<_>) (arg: 'a) (ofError: _ -> 'msg) : Cmd<'msg> =
                OfAsyncWith.attempt Async.StartImmediate task arg ofError

            /// Command that will evaluate an async block to the message
            let inline result (task: Async<'msg>) : Cmd<'msg> =
                OfAsyncWith.result Async.StartImmediate task

#if FABLE_COMPILER
        module OfPromise =
            /// Command to call `promise` block and map the results
            let either
                (task: 'a -> Fable.Core.JS.Promise<_>)
                (arg: 'a)
                (ofSuccess: _ -> 'msg)
                (ofError: #exn -> 'msg)
                : Cmd<'msg>
                =
                let bind dispatch =
                    (task arg).``then``(ofSuccess >> dispatch).catch (unbox >> ofError >> dispatch)
                    |> ignore

                [
                    bind
                ]

            /// Command to call `promise` block and map the success
            let perform (task: 'a -> Fable.Core.JS.Promise<_>) (arg: 'a) (ofSuccess: _ -> 'msg) =
                let bind dispatch =
                    (task arg).``then`` (ofSuccess >> dispatch) |> ignore

                [
                    bind
                ]

            /// Command to call `promise` block and map the error
            let attempt
                (task: 'a -> Fable.Core.JS.Promise<_>)
                (arg: 'a)
                (ofError: #exn -> 'msg)
                : Cmd<'msg>
                =
                let bind dispatch =
                    (task arg).catch (unbox >> ofError >> dispatch) |> ignore

                [
                    bind
                ]

            /// Command to dispatch the `promise` result
            let result (task: Fable.Core.JS.Promise<'msg>) =
                let bind dispatch = task.``then`` dispatch |> ignore

                [
                    bind
                ]

        [<Obsolete("Use `OfPromise.either` instead")>]
        let inline ofPromise
            (task: 'a -> Fable.Core.JS.Promise<_>)
            (arg: 'a)
            (ofSuccess: _ -> 'msg)
            (ofError: _ -> 'msg)
            : Cmd<'msg>
            =
            OfPromise.either task arg ofSuccess ofError
#else
        open System.Threading.Tasks

        module OfTask =
            /// Command to call a task and map the results
            let inline either
                (task: 'a -> Task<_>)
                (arg: 'a)
                (ofSuccess: _ -> 'msg)
                (ofError: _ -> 'msg)
                : Cmd<'msg>
                =
                OfAsync.either (task >> Async.AwaitTask) arg ofSuccess ofError

            /// Command to call a task and map the success
            let inline perform (task: 'a -> Task<_>) (arg: 'a) (ofSuccess: _ -> 'msg) : Cmd<'msg> =
                OfAsync.perform (task >> Async.AwaitTask) arg ofSuccess

            /// Command to call a task and map the error
            let inline attempt (task: 'a -> Task<_>) (arg: 'a) (ofError: _ -> 'msg) : Cmd<'msg> =
                OfAsync.attempt (task >> Async.AwaitTask) arg ofError

            /// Command and map the task success
            let inline result (task: Task<'msg>) : Cmd<'msg> =
                OfAsync.result (task |> Async.AwaitTask)

        [<Obsolete("Use OfTask.either instead")>]
        let inline ofTask
            (task: 'a -> Task<_>)
            (arg: 'a)
            (ofSuccess: _ -> 'msg)
            (ofError: _ -> 'msg)
            : Cmd<'msg>
            =
            OfTask.either task arg ofSuccess ofError
#endif

    open Cmd

    ///  <exclude />
    type Update<'Model> = ('Model -> 'Model) -> unit // A store updater. Store updates by being passed a model updater

    type StoreCons<'Model, 'Store> = (unit -> 'Model) -> ('Model -> unit) -> 'Store * Update<'Model>

    module internal Helpers =
        type CmdHandler<'Msg>(handler, ?dispose) =
            member _.Handle(cmd: Cmd<'Msg>) : unit = handler cmd

            member _.Dispose() =
                match dispose with
                | Some d -> d ()
                | None -> ()

            interface IDisposable with
                member this.Dispose() = this.Dispose()

#if FABLE_COMPILER
        open Fable.Core

        let cmdHandler (dispatch: 'Msg -> unit) : CmdHandler<'Msg> =
            new CmdHandler<_>(
                List.iter (fun cmd -> JS.setTimeout (fun _ -> cmd dispatch) 0 |> ignore)
            )
#else
        let cmdHandler (dispatch: 'Msg -> unit) : CmdHandler<'Msg> =
            let cts = new Threading.CancellationTokenSource()

            let mb =
                MailboxProcessor.Start(
                    fun inbox ->
                        async {
                            while true do
                                let! msg = inbox.Receive()
                                dispatch msg
                        }
                    , cts.Token
                )

            new CmdHandler<_>(List.iter (fun cmd -> cmd mb.Post), fun _ -> cts.Cancel())
#endif

    let internal makeElmishWithCons
        (init: 'Props -> 'Model * Cmd<'Msg>)
        (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
        (dispose: 'Model -> unit)
        (cons: StoreCons<'Model, 'Store>)
        : 'Props -> 'Store * Dispatch<'Msg>
        =

        let mutable _storeDispatch: ('Store * Dispatch<'Msg>) option = None

        let mutable _cmdHandler = Unchecked.defaultof<Helpers.CmdHandler<'Msg>>
        //new Helpers.CmdHandler<'Msg>(ignore)

        fun props ->
            match _storeDispatch with
            | Some storeDispatch -> storeDispatch
            | None ->
                let store, storeUpdate =
                    cons
                        (fun () ->
                            let m, cmd = init props
                            _cmdHandler.Handle cmd
                            m
                        )
                        (fun m ->
                            _cmdHandler.Dispose()
                            dispose m
                        )

                let dispatch msg =
                    let mutable _cmds = []

                    storeUpdate (fun model ->
                        let model, cmds = update msg model
                        _cmds <- cmds
                        model
                    )

                    _cmdHandler.Handle _cmds

                _cmdHandler <- Helpers.cmdHandler dispatch
                _storeDispatch <- Some(store, dispatch)
                store, dispatch

    let internal makeElmishWithDocument
        (doc: Document)
        (init: 'Props -> 'Model * Cmd<'Msg>)
        (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
        (dispose: 'Model -> unit)
        : 'Props -> IStore<'Model> * Dispatch<'Msg>
        =

        makeElmishWithCons
            init
            update
            dispose
            (fun i d ->
                let s = Store.makeStore i d

                let u =
                    (fun f ->
                        s.Update(f)
                        //Sutil.Internal.CustomEvents.notifySutilUpdated doc
                    )

                upcast s, u
            )

    let internal makeElmishSimpleWithDocument
        (doc: Document)
        (init: 'Props -> 'Model)
        (update: 'Msg -> 'Model -> 'Model)
        (dispose: 'Model -> unit)
        : 'Props -> IStore<'Model> * Dispatch<'Msg>
        =
        let init p = init p, []
        let update msg model = update msg model, []

        makeElmishWithCons
            init
            update
            dispose
            (fun i d ->
                let s = Store.makeStore i d

                let u =
                    (fun f ->
                        s.Update(f)
                        //Sutil.Internal.CustomEvents.notifySutilUpdated doc
                    )

                upcast s, u
            )

    [<RequireQualifiedAccess>]
    module Store =
        ///<summary>
        /// Creates a store and a dispatch method commonly used
        /// in elmish programs, this can be used to model more complex views that require better
        /// control flow and a predictable state.
        /// </summary>
        /// <example>
        /// <code>
        ///     type State = { count: int }
        ///     type Msg =
        ///         | Increment
        ///         | Decrement
        ///         | Reset
        ///     let init _ = { count = 0 }
        ///
        ///     let upddate msg state =
        ///         match msg with
        ///         | Increment -> { state = state.count + 1 }
        ///         | Decrement -> { state = state.count - 1 }
        ///         | Reset -> { state = 0 }
        ///
        ///     let view() =
        ///         let state, dispatch = Store.makeElmishSimple init update ignore ()
        ///
        ///         Html.article [
        ///             disposeOnUnmount [ state ]
        ///             bindFragment state &lt;| fun state -> text $"Count: {state.count}"
        ///
        ///             Html.button [ text "Increment"; onClick (fun _ -> dispatch) [] ]
        ///             Html.button [ text "Decrement"; onClick (fun _ -> dispatch) [] ]
        ///             Html.button [ text "Reset"; onClick (fun _ -> dispatch Reset) [] ]
        ///         ]
        /// </code>
        /// </example>
        let makeElmishSimple<'Props, 'Model, 'Msg>
            (init: 'Props -> 'Model)
            (update: 'Msg -> 'Model -> 'Model)
            (dispose: 'Model -> unit)
            =
            makeElmishSimpleWithDocument document init update dispose

        ///<summary>
        /// Creates a store and a dispatch function as <c>Store.makeElmishSimple</c>
        /// the difference being that this version handles [Elmish commands](https://elmish.github.io/elmish/index.html#Commands)
        /// as well, generally used in more complex UIs given that with commands you can also handle
        /// asynchronous code like fetching resources from a server or calling any
        /// function that returns a promise or async
        /// </summary>
        /// <example>
        /// <code>
        ///     type State = { count: int }
        ///     type Msg =
        ///         | Increment
        ///         | Decrement
        ///         | Reset
        ///         | AsyncIncrement
        ///         | AsyncDecrement
        ///     let init _ = { count = 0 }, Cmd.ofMsg AsyncIncrement
        ///
        ///     let wait1S () =
        ///         async {
        ///             do! Async.Sleep 1000
        ///         }
        ///
        ///     let upddate msg state =
        ///         match msg with
        ///         | Increment -> { state = state.count + 1 }, Cmd.none
        ///         | Decrement -> { state = state.count - 1 }, Cmd.none
        ///         | AsyncIncrement ->
        ///             state, Cmd.ofAsync.perform () wait1S Increment
        ///         | AsyncDecrement->
        ///             state, Cmd.ofAsync.perform () wait1S Decrement
        ///         | Reset -> { state = 0 } Cmd.none
        ///
        ///     let view() =
        ///         let state, dispatch = Store.makeElmish init update ignore ()
        ///
        ///         Html.article [
        ///             disposeOnUnmount [ state ]
        ///             bindFragment state &lt;| fun state -> text $"Count: {state.count}"
        ///
        ///             Html.button [ text "Increment"; onClick (fun _ -> dispatch Increment) [] ]
        ///             Html.button [ text "Async Increment"; onClick (fun _ -> dispatch AsyncIncrement) [] ]
        ///             Html.button [ text "Decrement"; onClick (fun _ -> dispatch Decrement) [] ]
        ///             Html.button [ text "Async Decrement"; onClick (fun _ -> dispatch AsyncDecrement) [] ]
        ///             Html.button [ text "Reset"; onClick (fun _ -> dispatch Reset) [] ]
        ///         ]
        /// </code>
        /// </example>
        let makeElmish<'Props, 'Model, 'Msg>
            (init: 'Props -> 'Model * Cmd<'Msg>)
            (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
            (dispose: 'Model -> unit)
            =
            makeElmishWithDocument document init update dispose
