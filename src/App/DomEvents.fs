module DomEvents

// Adapted from
// https://svelte.dev/examples

open Sutil
open type Feliz.length
open Sutil.Html
open Sutil.Bind
open Sutil.CoreElements

open Browser.Types

let view() =
    let mouseXY = Store.make (0.0,0.0)

    let handleMousemove (e:MouseEvent) =
        mouseXY <~ (e.clientX, e.clientY)

    Html.div [
        Attr.style [ 
            Css.width (percent 100)
            Css.height (percent 100)
        ]

        disposeOnUnmount [mouseXY]

        Ev.onMouseMove handleMousemove 

        Bind.el( mouseXY,
            fun (x,y) -> text $"The mouse position is {x} x {y}"
        )
    ] 