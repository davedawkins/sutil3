module Dimensions

// Adapted from
// https://svelte.dev/examples

open type Feliz.length
open type Feliz.borderStyle
open Sutil

open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind

open Sutil.Styling

let style =
    [
        rule "input" [
            Css.displayBlock
            Css.width (percent 50)
        ]
        rule "div.resizing" [
            Css.displayInlineBlock
            Css.border (pt 1, solid, "#dddddd")
            Css.resizeBoth
        ]
    ]

let view () =
    Html.div [
        let w = Store.make 0.0
        let h = Store.make 0.0
        let size = Store.make 42.0
        let editText = Store.make "Edit me, slide him ↑"

        disposeOnUnmount [
            w
            h
            size
            editText
        ]

        Bulma.block [
            Bulma.inputRange [
                Bind.attr ("value", size)
            ]
        ]

        Bulma.block [
            Bulma.inputText [
                Bind.attr ("value", editText)
            ]
        ]

        Html.div [
            Bind.el2 w h <| fun (w', h') -> text $"Size: {w'}px x {h'}px"
        ]

        Html.divc "resizing" [
            CoreElements.listenToResize (fun e ->
                Store.set w (e.clientWidth)
                Store.set h (e.clientHeight)
            )
            Html.span [
                Bind.attr ("style", size |> Store.map (fun n -> $"font-size: {n}px"))
                Bind.el (editText, text)
            ]
        ]
    ]
    |> withStyle style
