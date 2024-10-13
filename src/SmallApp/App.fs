module App

open Sutil

open Sutil.Html
open Sutil.Styling
open Sutil.Bind
open type Feliz.length

// let css (s: string) = s

// let _style = css $"""
//     .red {{
//         color: red;
//     }}
// """

let style = [
    rule ".red" [
        Css.color "red"
    ]
]

module Nested = 

    let Nested () =
        Html.p [
            text "...don't affect this element"
        ]
        |> withStyle []

    let css =
        [
            rule "p" [
                Css.color "orange"
                Css.fontFamily "'Comic Sans MS', cursive"
                Css.fontSize (em 2.0)
            ]
        ]

    let view () =
        Html.div [
            Html.p [
                text "These styles..."
            ]
            Html.p [
                text "...don't affect this element"
            ] |> withStyle []
        ] |> withStyle css


let view() = 
    let counter = Store.make 0
    let nameS = Store.make ""

    Html.div [
        text "Container"

        Html.div [

            Bind.el( counter, fun _ -> Html.divc "red" [ text "Hello world " ] )

            Html.divc "red" [
                text "Unstyled"
            ] |> withStyle []

        ] |> withStyle style


    ]

Nested.view() |> Program.mount
