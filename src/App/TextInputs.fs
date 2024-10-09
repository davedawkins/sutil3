module TextInputs

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind

let view() =
    let name = Store.make("")

    let nameOrStranger s = if s = "" then "stranger" else s

    Html.div [
        disposeOnUnmount [ name ]

        Bind.el( "inputBinding", name, fun value -> 
            Bulma.inputText [
                Attr.value value
                Attr.placeholder "Enter your name"
                Ev.onInput( fun e ->
                    let inputEl = (e.target :?> Browser.Types.HTMLInputElement)
                    inputEl.value |> Store.set name
                )
            ]
        ) 

        Html.p [
            Attr.className "block"
            Bind.el( "outputBinding", name, fun s -> 
                text $"Hello {nameOrStranger s}")
        ]
    ]
