module TransitionInOut

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Html
open Sutil.Bind
open Sutil.CoreElements
open Sutil.Transition

let view () =
    let visible = Store.make true

    Html.div [
        disposeOnUnmount [
            visible
        ]

        Html.label [
            Bulma.inputCheckbox [
                Bind.attr ("checked", visible)
            ]
            text " visible"
        ]

        let flyIn =
            fly
            |> withProps [
                Duration 2000.0
                Y 200.0
            ]

        Html.p [
            text "Flies in and fades out"
        ]
        |>
        transition
            [
                In flyIn
                Out fade
            ]
            visible
    ]
