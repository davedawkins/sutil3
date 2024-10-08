module SelectMultiple

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Html
open Sutil.Bind
open Sutil.CoreElements

let menu =
    [
        "Cookies and cream"
        "Mint choc chip"
        "Raspberry ripple"
    ]

let scoopMenu =
    [
        "One scoop"
        "Two scoops"
        "Three scoops"
    ]

// Text helpers
let plural word n =
    let s =
        if n = 1 then
            ""
        else
            "s"

    $"{word}{s}"

let scoop_s n = plural "scoop" n

let rec join (flavours: string list) =
    match flavours with
    | [] -> ""
    | x :: [ y ] -> $"{x} and {y}"
    | [ x ] -> x
    | x :: xs -> $"{x}, {join xs}"

// HTML helpers
let block children = Html.divc "block" children

// Control with only 1 label child
let controlLabel children =
    Html.divc "control" [
        Html.label children
    ]

let label s =
    Html.labelc "label" [
        text s
    ]

// Main component view
let view () =
    let flavours =
        Store.make (
            [
                menu |> List.head
            ]
        )

    let scoops = Store.make (1)

    Html.div [
        disposeOnUnmount [
            flavours
            scoops
        ]

        block [
            label "Scoops"
            scoopMenu
            |> List.mapi (fun i scoopChoice ->
                controlLabel [
                    Attr.className "radio"
                    Bulma.inputRadio [
                        Bind.radioValue scoops
                        i + 1 |> string |> Attr.value
                    ]
                    text $" {scoopChoice}"
                ]
            )
            |> Html.fragment
        ]

        block [
            label "Flavours"
            Html.divc "select is-multiple" [
                Html.select [
                    Attr.multiple true
                    Bind.selectMultiple flavours
                    Html.fragment [
                        for flavour in menu do
                            Html.option [
                                Attr.value flavour
                                text $" {flavour}"
                            ]
                    ]
                ]
            ]
        ]

        block [
            Bind.el2
                scoops
                flavours
                (fun (s, f) ->
                    match (s, f) with
                    | (_, []) -> text "Please select at least one flavour"
                    | (s, f) when f.Length > s -> text "Can't order more flavours than scoops!"
                    | _ -> text $"You ordered {s} {scoop_s s} of {join f}"
                )
        ]
    ]
