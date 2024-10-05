module Transition

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Core
open Sutil.Dsl
open Sutil.CoreElements
open Sutil.Bind
// open Sutil.Transition

let view() =
    let visible = Store.make true

    Html.div [
        disposeOnUnmount [visible]

        Html.label [
            Html.input [
                Attr.typeCheckbox
                Bind.attr ("checked",visible)
            ]
            text " visible"
        ]
        //transition [InOut fade] visible <|
        Html.p [ text "TODO: Fades in and out" ]
    ]
