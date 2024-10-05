module Sutil.Elmish

open Sutil
open Sutil.Elmish.Cmd
open System
open Browser.Dom
open Browser.Types

///  <exclude />
type Update<'Model> = ('Model -> 'Model) -> unit // A store updater. Store updates by being passed a model updater

type StoreCons<'Model, 'Store> = 
    (unit -> 'Model) -> ('Model -> unit) -> 'Store * Update<'Model>
        
module internal Helpers =
    type CmdHandler<'Msg>(handler, ?dispose) =
        member _.Handle(cmd: Cmd<'Msg>): unit = handler cmd
        member _.Dispose() = match dispose with Some d -> d () | None -> ()
        interface IDisposable with
            member this.Dispose() = this.Dispose()

#if FABLE_COMPILER
    open Fable.Core
    let cmdHandler (dispatch: 'Msg -> unit): CmdHandler<'Msg> =
        new CmdHandler<_>(List.iter (fun cmd -> JS.setTimeout (fun _ -> cmd dispatch) 0 |> ignore))
#else
    let cmdHandler (dispatch: 'Msg -> unit): CmdHandler<'Msg> =
        let cts = new Threading.CancellationTokenSource()

        let mb = MailboxProcessor.Start(fun inbox -> async {
            while true do
                let! msg = inbox.Receive()
                dispatch msg
        }, cts.Token)

        new CmdHandler<_>(List.iter (fun cmd -> cmd mb.Post), fun _ -> cts.Cancel())
#endif

let internal makeElmishWithCons (init: 'Props -> 'Model * Cmd<'Msg>)
                        (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
                        (dispose: 'Model -> unit)
                        (cons: StoreCons<'Model, 'Store>)
                        : 'Props -> 'Store * Dispatch<'Msg> =

    let mutable _storeDispatch: ('Store * Dispatch<'Msg>) option = None

    let mutable _cmdHandler = Unchecked.defaultof<Helpers.CmdHandler<'Msg>>
        //new Helpers.CmdHandler<'Msg>(ignore)

    fun props ->
        match _storeDispatch with
        | Some storeDispatch ->
            storeDispatch
        | None ->
            let store, storeUpdate =
                cons
                    (fun () ->
                        let m, cmd = init props
                        _cmdHandler.Handle cmd
                        m)
                    (fun m ->
                        _cmdHandler.Dispose()
                        dispose m)

            let dispatch msg =
                let mutable _cmds = []
                storeUpdate(fun model ->
                    let model, cmds = update msg model
                    _cmds <- cmds
                    model)
                _cmdHandler.Handle _cmds

            _cmdHandler <- Helpers.cmdHandler dispatch
            _storeDispatch <- Some(store, dispatch)
            store, dispatch

let internal makeElmishWithDocument (doc:Document) (init: 'Props -> 'Model * Cmd<'Msg>)
                (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
                (dispose: 'Model -> unit)
                : 'Props -> IStore<'Model> * Dispatch<'Msg> =

    makeElmishWithCons init update dispose (fun i d ->
        let s = Store.makeStore i  d
        let u = (fun f -> s.Update(f))
        upcast s, u)

let internal makeElmishSimpleWithDocument (doc:Document) (init: 'Props -> 'Model)
                (update: 'Msg -> 'Model -> 'Model)
                (dispose: 'Model -> unit)
                : 'Props -> IStore<'Model> * Dispatch<'Msg> =
    let init p = init p, []
    let update msg model = update msg model, []
    makeElmishWithCons init update dispose (fun i d ->
        let s = Store.makeStore i  d
        let u = (fun f -> s.Update(f))
        upcast s, u)

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
