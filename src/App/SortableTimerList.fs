module SortableTimerList

open System

open Sutil
open Sutil.Elmish
open Sutil.BulmaEngine
open Sutil.Html
open Sutil.Styling

open DragDropListSort

open type Feliz.length

type Label =
    {
        Id: int
        Text: string
    }

type Model =
    {
        Labels: Label list
    }

type Message =
    | Drag of DragOperation
    | Nop

let init () =
    {
        Labels =
            [
                {
                    Id = 1
                    Text = "Exercise"
                }
                {
                    Id = 2
                    Text = "Study"
                }
                {
                    Id = 3
                    Text = "Lunch"
                }
                {
                    Id = 4
                    Text = "Admin"
                }
                {
                    Id = 5
                    Text = "Practise"
                }
            ]
    },
    Cmd.none

let update msg (model: Model) =
    match msg with
    | Nop -> model, Cmd.none
    | Drag op ->
        match op with
        | InsertBefore(srcId, tgtId) ->
            let src = model.Labels |> List.find (fun l -> l.Id = srcId)

            let rec edit list =
                match list with
                | [] -> []
                | x :: xs when x.Id = srcId -> edit xs
                | x :: xs when x.Id = tgtId -> src :: x :: edit xs
                | x :: xs -> x :: edit xs

            { model with
                Labels = edit model.Labels
            },
            Cmd.none
        | InsertAfter(srcId, tgtId) ->
            let src = model.Labels |> List.find (fun l -> l.Id = srcId)

            let rec edit list =
                match list with
                | [] -> []
                | x :: xs when x.Id = srcId -> edit xs
                | x :: xs when x.Id = tgtId -> x :: src :: edit xs
                | x :: xs -> x :: edit xs

            { model with
                Labels = edit model.Labels
            },
            Cmd.none
        | _ -> model, Cmd.none

let buttonStyle =
    [
        rule ".timer-button" [
            Css.border (px 1, Feliz.borderStyle.solid, "gray")
            Css.borderRadius (px 5)
            Css.margin (px 12)
        ]
        rule ".timer-button.running" [
            Css.backgroundColor "#e3fff8"
        ]
    ]

// Styles for the drag operation
// The DragDropListSort component could include a default style and allow us to override
let dragDropStyle =
    [
        rule ".dragging-init" [
            // Styles the dragged element
            Css.opacity 0.75
            Css.transformScale 0.75
        ]
        rule ".dragging" [
            // Styles the original element 10ms after drag starts

            // Gives initial popping effect
            //Css.transition "0.1s"
            //Css.transitionTimingFunctionEaseOut

            Css.opacity 1.0
            Css.transformScale 1.0
        ]
        rule "li.insert-after::after, li.insert-before::before" [
            Css.custom ("content", "''")
            Css.border (px 1, Feliz.borderStyle.solid, "#09154f")
            Css.displayBlock
        ]
    ]

let view () =

    let model, dispatch = () |> Store.makeElmish init update ignore

    let labels = model |> Store.map (fun m -> m.Labels)

    let makeLabel (label: Label) =
        let format (t: float) =
            let totalSec = int t
            let s = totalSec % 60
            let m = totalSec / 60

            if m = 0 then
                sprintf "%02d" s
            else
                sprintf "%02d:%02d" m s

        let displayTime (label: string) (elapsed: IObservable<bool * float>) =
            elapsed
            |> Store.map (fun (run, t) ->
                if not run then
                    label
                else
                    sprintf "%s: %ss" label (format t)
            )
            |> Html.text

        Html.li [
            // DragDropSortList looks for this. eachk could do this automatically
            CoreElements.onElementMounted (fun el ->
                Internal.JsMap.setKey  el "_key" label.Id 
            )

            TimerWithButton.create (displayTime label.Text)
                |> CoreElements.append [
                    Attr.className "timer-button"
                ] // Add this class to the TimerWithButton element
        ]

    bulma.columns [
        columns.isCentered
        bulma.column [
            column.is12Mobile
            column.is8Tablet
            column.is8Desktop
            column.is6Widescreen
            Html.ul [
                DragDropListSort.create labels makeLabel (fun l -> l.Id) [] (dispatch << Drag)
            ]
        ]
    ] |> withStyle (buttonStyle @ dragDropStyle)
