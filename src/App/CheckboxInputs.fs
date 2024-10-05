module CheckboxInputs

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Dsl
open Sutil.Bind

let yes = Store.make(false)

let view() =
    Html.div [

        Html.divc "block" [
            Html.label [
                Html.input [
                    Attr.typeCheckbox
                    Bind.attr ("checked",yes)
                ]
                text " Enable ejector seat"
            ]
        ]

        Html.divc "block" [
            showIfElse yes
                (Html.p  [ text "You are ready for launch" ])
                (Html.p  [ text "You won't be going anywhere unless you enable the ejector seat" ])
        ]

        Html.divc "block" [
            Html.button [
                Bind.attr ("disabled", yes |> Store.map not)
                text "Launch"
            ]
        ]
    ]
