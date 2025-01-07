module CustomEvents

open Sutil
open Sutil.Internal
open type Feliz.length
open Sutil.CoreElements
open Sutil.Html
open Sutil.Bind

open Browser.Types
open System

let customDispatchButton () =
    let r = Random()

    let clickHandler (e: Event) =
        let props: CustomEvents.CustomDispatch<string> list =
            [
                CustomEvents.Bubbles true
                CustomEvents.Detail($"Hello there! %i{r.Next(1000)}")
            ]

        CustomEvents.CustomDispatch.dispatch<string> (e, "on-custom-click", props)

    Bulma.button [
        Ev.onClick clickHandler
        text "I will dispatch an 'on-custom-click' event"
    ]

let view () =
    Html.div [
        let m = Store.make ""

        disposeOnUnmount [
            m
        ]

        Ev.onCustomEvent<string> (
            "on-custom-click",
            (fun (e: CustomEvent<string>) -> e.detail |> Option.defaultValue "" |> Store.set m)
        )

        Html.div [
            customDispatchButton ()

            Bind.el (
                m,
                fun s ->
                    Html.p [
                        text $"Got: [{s}]"

                        Attr.style [
                            Css.marginTop (px 12)
                        ]
                    ]
            )
        ]
    ]
