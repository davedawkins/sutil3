module TransitionEvents

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Bind
open Sutil.Html
open Sutil.CoreElements
open Sutil.Transition

let view () =
    let visible = Store.make true
    let status = Store.make "Waiting..."

    Html.div [
        disposeOnUnmount [
            visible
            status
        ]

        Html.p [
            Attr.className "block"
            text "status: "
            Bind.el (status, text)
        ]

        Html.label [
            Bulma.inputCheckbox [
                Bind.attr ("checked", visible)
            ]
            text " visible"
        ]

        Html.p [
            Ev.onCustomEvent("introstart", (fun _ -> status <~ "intro started"))
            Ev.onCustomEvent("introend", (fun _ -> status <~ "intro ended"))
            Ev.onCustomEvent("outrostart", (fun _ -> status <~ "outro started"))
            Ev.onCustomEvent("outroend", (fun _ -> status <~ "outro ended"))
            text "Flies in and out"
        ]
        |> transition
            [
                fly
                |> withProps [
                    Duration 2000.0
                    Y 200.0
                ]
                |> InOut
            ]
            visible
    ]
