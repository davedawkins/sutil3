module App

open Browser.Dom

open Dsl
open Bind
open Core

let view() = 
    let counter = Store.make 0

    Html.div [
        Html.text "Hello World"
        // Html.fragment [
        //     Html.div "Single element fragment"
        // ]
        // Html.fragment [
        //     Html.div "Dual element fragment (1)"
        //     Html.div "Dual element fragment (2)"
        // ]
        // Html.fragment [
        //     Html.div "Before nested"
        //     Html.fragment [
        //         Html.div "Dual element fragment (1)"
        //         Html.div "Dual element fragment (2)"
        //     ]
        //     Html.div "After nested"
        // ]
        Html.button [
            Html.text "Press Me"
            Ev.onClick (fun e -> 
                console.log("Pressed")
                counter.Value <- counter.Value + 1
            )
        ]
        Html.div [
            Bind.el( counter, fun n -> 
                match n with
                // | 0 -> 
                //     Html.fragment [
                //         Html.div "Counter is zero"
                //         Html.div "Press button to increment"
                //     ]
                // | 1 -> 
                //     Html.fragment [
                //         Html.div "Counter is 1"
                //     ]
                // | 5 -> 
                //     Html.fragment [
                //     ]
                | _ -> Html.text (sprintf "%d" n)
            )
        ]
        Html.div [
            Html.input [
                Attr.placeholder ("Enter name")
                Ev.onInput( fun e ->
                    console.log( e.targetElement.value )
                )

            ]
        ]
    ]

view() |> mountAsChild "sutil-app"
