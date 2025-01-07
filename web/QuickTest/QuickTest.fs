module App

open Sutil

open Sutil.CoreElements
open Sutil.Html
open Sutil.Bind

// Run `./build.sh quicktest --watch` and then add some code here.
//
// When you save, the changes will be reflected in the browser immediately.
//
// Please don't add this file to your commits, it's just for quick prototyping.
// Your commit will fails if you do so.
//
// To get a minimal style that is easy to work with, we are using:
// Pico.css: https://picocss.com/

let view () =
    let count = Store.make 0

    Html.div [
        Html.h3 "Use this file to prototype, test or reproduce issues."

        Html.hr []

        disposeOnUnmount [
            count
        ]

        Html.p [
            Bind.el (count, fun n -> Html.div $"Counter = {n}")
        ]

        Html.div [
            prop.className "grid"
            Html.button [
                Ev.onClick (fun _ -> count <~= (fun n -> n - 1))
                text "-"
            ]

            Html.button [
                Ev.onClick (fun _ -> count <~= (fun n -> n + 1))
                text "+"
            ]
        ]
    ]

view () |> Program.mount
