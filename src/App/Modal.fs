module Modal

open Sutil
open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind
open Sutil.Styling

let view() =
    Html.div [
        let active = Store.make false

        disposeOnUnmount [active]

        Bulma.button [
            text "Launch example modal"
            Ev.onClick (fun _ -> active <~ true)
        ]

        Html.body [
            Html.div [
                Attr.className "modal"
                Bind.toggleClass(active,"is-active")

                Html.div [ Attr.className "modal-background" ]
                Html.div [
                    Attr.className "modal-content"
                    Bulma.box [
                        text "All Your Modal Content Goes Here"
                    ]
                ]
                Bulma.button [
                    Attr.className "modal-close"
                    Ev.onClick (fun _ -> active <~ false) 
                    Attr.ariaLabel "close"
                ]
            ]
        ] |> withStyle []
    ]
