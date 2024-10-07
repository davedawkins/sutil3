module ReactiveStatements

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Html
open Sutil.Bind
open Sutil.CoreElements

open Browser.Dom

let inc n = n + 1
let plural n = if n = 1 then "" else "s"

let view() =
    let count = Store.make 0

    let unsub = count |> Store.iter (fun n ->
        if n >= 10 then
            window.alert("count is dangerously high!")
            count <~ 9
        )

    let handleClick _ =
        count <~= inc   // or: Store.modify count inc

    Html.button [
        disposeOnUnmount [count; unsub ]

        Ev.onClick handleClick

        Bind.el( count,  (fun n -> text $"Clicked {n} time{plural n}"))
    ]
