module NumericInputs

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Dsl
open Sutil.CoreElements
open Sutil.Bind


let view() =
    let a = Store.make(1)
    let b = Store.make(2)

    Html.div [
        disposeOnUnmount [ a; b ]

        Html.divc "block" [
            Html.input [
                Attr.typeNumber
                Bind.attr ("value",a)
                Attr.min 0
                Attr.max 10
            ]
            Html.input [
                Attr.typeRange
                Bind.attr ("value",a)
                Attr.min 0
                Attr.max 10
            ]
        ]
        Html.divc "block" [
            Html.input [
                Attr.typeNumber
                Bind.attr ("value",b)
                Attr.min 0
                Attr.max 10
            ]
            Html.input [
                Attr.typeRange
                Bind.attr ("value",b)
                Attr.min 0
                Attr.max 10
            ]
        ]
        Html.p [
            Attr.className "block"
            Bind.el2 a b (fun (a',b') -> text $"{a'} + {b'} = {a' + b'}")
        ]
    ]