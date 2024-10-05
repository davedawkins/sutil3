module Counter

open Sutil
open Sutil.Dsl
open Sutil.CoreElements
open Sutil.Bind

let view() =
    let count = Store.make 0

    Html.div [
        disposeOnUnmount [ count ]

        Html.div [
            Attr.className "block"
            Bind.el(count, fun n -> text $"Counter = {n}")
        ]

        Html.div [
            Attr.className "block"
            Html.button [
                Ev.onClick (fun _ -> count <~= (fun n -> n-1))
                text "-"
            ]

            Html.button [
                Ev.onClick (fun _ -> count <~= (fun n -> n+1))
                text "+"
            ]
        ]
    ]
