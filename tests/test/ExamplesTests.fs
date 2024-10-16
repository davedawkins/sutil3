module ExamplesTests

open Describe

#if HEADLESS
open WebTestRunner
#endif

open Sutil
open Sutil.Html
open Sutil.Bind
open Sutil.CoreElements
open Sutil.Styling
open Browser.CssExtensions
open Browser.DomExtensions

let log s = Fable.Core.JS.console.log s

open Fable.Core.JsInterop

let [<Literal>] RED = "rgb(255, 0, 0)"
let [<Literal>] BLACK = "rgb(0, 0, 0)"

describe "Style" <| fun () ->

    // Simplest case
    it "example ReactiveDeclarations" <| fun () -> promise {
        let count = Store.make 1
        let doubled = count |> Store.map ((*) 2)
        let quadrupled = doubled |> Store.map ((*) 2)

        let handleClick () = count |> Store.modify (fun n -> n + 1)

        let view () =
            Html.div [
                disposeOnUnmount [
                    count
                ]

                Html.button [
                    Ev.onClick (fun _ -> handleClick())
                    Bind.el (count, fun n -> text $"Count: {n}")
                ]

                Html.p [
                    Bind.el2 count doubled (fun (c, d) -> text $"{c} * 2 = {d}")
                ]

                Html.p [
                    Bind.el2 doubled quadrupled (fun (d, q) -> text $"{d} * 2 = {q}")
                ]
            ]

        view() |> mountTestApp

        Expect.queryText "div>p:nth-child(2)" "1 * 2 = 2"
        Expect.queryText "div>p:nth-child(3)" "2 * 2 = 4"

        handleClick()

        Expect.queryText "div>p:nth-child(2)" "2 * 2 = 4"
        Expect.queryText "div>p:nth-child(3)" "4 * 2 = 8"

        return ()
    }

    it "example GroupInputs" <| fun () -> promise {

        let menu =
            [
                "Cookies and cream"
                "Mint choc chip"
                "Raspberry ripple"
            ]

        let rec join (flavours: string list) = 
            flavours |> String.concat ", "

        let flavours = Store.make ( [ menu |> List.head ])
        let scoops = Store.make 1

        let view () =

            Html.div [
                disposeOnUnmount [
                    flavours
                    scoops
                ]

                Html.div [
                    Bind.el2
                        scoops
                        flavours
                        (fun (s, f) ->
                            match (s, f) with
                            | (_, []) -> text "Please select at least one flavour"
                            | (s, f) when f.Length > s -> text "Can't order more flavours than scoops!"
                            | _ -> text $"You ordered {s} scoop(s) of {join f}"
                        )
                ]
            ]

        view() |> mountTestApp

        Expect.querySingleTextChild "div>div" "You ordered 1 scoop(s) of Cookies and cream"

        menu |> List.take 1 |> Store.set flavours

        Expect.querySingleTextChild "div>div" "You ordered 1 scoop(s) of Cookies and cream"

        menu |> List.take 0 |> Store.set flavours

        Expect.querySingleTextChild "div>div" "Please select at least one flavour"


        return ()
    }

let init() =
    ()
