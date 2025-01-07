module TransitionParameters

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Bind
open Sutil.Transition
open Sutil.Html
open Sutil.CoreElements

let view () =
    let visible = Store.make true

    Html.div [
        disposeOnUnmount [
            visible
        ]

        Html.label [
            Html.input [
                Attr.typeCheckbox
                Bind.attr ("checked", visible)
            ]
            text " visible"
        ]

        Html.p [
            text "Flies in and out"
        ]
        |> transition
            [
                fly
                |> withProps [
                    Duration 2000.0
                    Y 200.0
                ]
                |> InOut
            ]
            visible
    ]
