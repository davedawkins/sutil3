module App

open Dsl
open Bind
open Core
open Style

let style = [

    rule ".red" [
        "color", "red"
    ]

]

let view() = 
    let counter = Store.make 0
    let nameS = Store.make ""

    Html.div [
        Html.divc "red" [ text "Hello World" ]

        Html.button [
            text "Press Me"
            Ev.onClick (fun _ ->  counter.Value <- counter.Value + 1)
        ]

        Html.div [
            Bind.el( counter, fun n -> text (sprintf "%d" n))
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
                    text (sprintf "Hello %s" name)
                ]
            ]
        )
    ] |> withStyle style

view() |> mountAsChild "sutil-app"
