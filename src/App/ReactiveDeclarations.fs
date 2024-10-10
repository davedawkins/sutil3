module ReactiveDeclarations

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Html
open Sutil.Bind
open Sutil.CoreElements

let view () =
    let count = Store.make 1
    let doubled = count |> Store.map ((*) 2)
    let quadrupled = doubled |> Store.map ((*) 2)

    let handleClick _ = count |> Store.modify (fun n -> n + 1)

    Html.div [
        disposeOnUnmount [
            count
        ]

        Bulma.button [
            Attr.className "block"
            Ev.onClick handleClick
            Bind.el (count, fun n -> text $"Count: {n}")
        ]

        Html.p [
            Attr.className "block"
            Bind.el2 count doubled (fun (c, d) -> text $"{c} * 2 = {d}")
        ]

        Html.p [
            Attr.className "block"
            Bind.el2 doubled quadrupled (fun (d, q) -> text $"{d} * 2 = {q}")
        ]
    ]
