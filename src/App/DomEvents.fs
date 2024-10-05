module DomEvents

// Adapted from
// https://svelte.dev/examples

open Sutil
open type Feliz.length
open Sutil.Dsl
open Sutil.Bind
open Sutil.CoreElements

open Sutil.Styling
open Browser.Types

let view() =
    Html.div [
        let m = Store.make (0.0,0.0)

        let handleMousemove (e:MouseEvent) =
            m <~ (e.clientX, e.clientY)

        disposeOnUnmount [m]

        Ev.onMouseMove handleMousemove 
        Bind.el(m,fun (x,y) -> text $"The mouse position is {x} x {y}")
    ] |> withStyle [
        rule "div" [
            Css.width (vw 100)
            Css.height (vh 100)
        ]
    ]