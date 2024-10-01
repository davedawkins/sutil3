module App

open Dsl
open Bind
open Core

let view() = 
    let counter = Store.make 0
    let nameS = Store.make ""

    Html.div [
        Html.div "Hello World"

        Html.button [
            Html.text "Press Me"
            Ev.onClick (fun _ ->  counter.Value <- counter.Value + 1)
        ]

        Html.div [
            Bind.el( counter, fun n -> Html.text (sprintf "%d" n))
        ]

        Bind.el( nameS, fun name ->
            Html.div [
                Html.input [
                    Attr.placeholder ("Enter name")
                    Ev.onInput( fun e ->
                        nameS.Value <- e.targetElement.value 
                    )
                ]
                Html.div [
                    Html.text (sprintf "Hello %s" name)
                ]
            ]
        )
    ]

view() |> mountAsChild "sutil-app"
