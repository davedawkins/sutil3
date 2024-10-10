module NestedComponents

// Adapted from
// https://svelte.dev/examples

open Sutil.Styling
open Sutil.Html
open Nested

open type Feliz.length

let css =
    [
        rule "p" [
            Css.color "orange"
            Css.fontFamily "'Comic Sans MS', cursive"
            Css.fontSize (em 2.0)
        ]
    ]

let view () =
    withStyle css
    <| Html.div [
        Html.p [
            text "These styles..."
        ]
        Nested()
    ]
