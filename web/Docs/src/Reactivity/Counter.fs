module Counter

open Sutil
open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind

let view () =
    let count = Store.make 0

    Html.div [
        disposeOnUnmount [
            count
        ]

        Html.div [
            Attr.className "block"
            Bind.el (count, fun n -> text $"Counter = {n}")
        ]

        Html.div [
            Attr.className "block"
            Bulma.button [
                Ev.onClick (fun _ -> count <~= (fun n -> n - 1))
                text "-"
            ]

            Bulma.button [
                Ev.onClick (fun _ -> count <~= (fun n -> n + 1))
                text "+"
            ]
        ]
    ]
