module Modal

open Sutil
open Sutil.Dsl
open Sutil.CoreElements
open Sutil.Bind
open Sutil.Styling

let view() =
    Html.div [
        let active = Store.make false

        disposeOnUnmount [active]

        Html.button [
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
                    Html.div [
                        Attr.className "box"
                        text "All Your Modal Content Goes Here"
                    ]
                ]
                Html.button [
                    Attr.className "modal-close"
                    Ev.onClick (fun _ -> active <~ false) 
                    Attr.ariaLabel "close"
                ]
            ]
        ] |> withStyle []
    ]
