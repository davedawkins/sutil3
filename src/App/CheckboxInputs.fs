module CheckboxInputs

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Html
open Sutil.Bind

let yes = Store.make (false)

let view () =
    Html.div [

        Bulma.block [
            Html.label [
                Bulma.inputCheckbox [
                    Bind.attr ("checked", yes)
                ]
                text " Enable ejector seat"
            ]
        ]

        Bulma.block [
            Bind.el (
                yes,
                fun ready ->
                    if ready then
                        (Html.p [
                            text "You are ready for launch"
                        ])
                    else
                        (Html.p [
                            text "You won't be going anywhere unless you enable the ejector seat"
                        ])
            )
        ]

        Bulma.block [
            Bulma.button [
                Bind.attr ("disabled", yes |> Store.map not)
                text "Launch"
            ]
        ]
    ]
