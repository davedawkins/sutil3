module App

open Sutil

open Dsl
open Bind
open Core
open Style


let css (s: string) = s

let _style = css $"""
    .red {{
        color: red;
    }}
"""

let style = [
    rule ".red" [
        "color", "red"
    ]
]

let view() = 
    let counter = Store.make 0
    let nameS = Store.make ""

    Html.fragment [
    Html.div [
        Ev.onMount (fun _ ->
            Fable.Core.JS.console.log("Mounted")
        )

        Html.divc "red" [ text "Hello World" ]

        Html.button [
            text "+"
            Ev.onClick (fun _ ->  counter.Value + 1 |> Store.set counter)
        ]

        Html.button [
            text "-"
            Ev.onClick (fun _ ->  counter.Value - 1 |> Store.set counter)
        ]

        Html.div [
            Bind.el( counter, fun n -> text (sprintf "n=%d" n))
        ]

        Bind.el( counter, fun n -> 
            Html.fragment [ 
                for i in 0 .. n-1 do Html.div (sprintf "%d" i)
            ] )

        Bind.el( nameS, fun name ->
            Html.div [
                Html.input [
                    Attr.placeholder ("Enter name")
                    Ev.onInputT( fun e ->
                        e.targetElement.value |> Store.set nameS
                    )
                ]
                if name = "" then
                    Html.p "Hello stranger"
                else
                    Html.div [
                        Ev.onMount (fun _ -> Fable.Core.JS.console.log "mounted")
                        Ev.onUnmount (fun _ -> Fable.Core.JS.console.log "unmounted")
                        text (sprintf "Hello %s" name)
                    ]
            ]
        )
    ] ] |> withStyle style

view() |> Program.mount
