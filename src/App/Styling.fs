module StylingExample

// Adapted from
// https://svelte.dev/examples

open Sutil.Styling
open Sutil.Dsl
open type Feliz.length

let css = [
        rule "p" [
            Css.color "purple"
            Css.fontFamily "'Comic Sans MS', cursive"
            Css.fontSize (em 2.0)
        ]
    ]

let view() =
    withStyle css <| Html.p [
        text "Styled!"
    ]
