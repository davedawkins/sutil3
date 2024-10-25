module Transition

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Core
open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind
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
        
        Html.p [
            text "Fades in and out"
        ] |> transition [InOut fade] visible
    ]
