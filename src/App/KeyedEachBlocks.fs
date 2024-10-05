module KeyedEachBlocks

// Adapted from
// https://svelte.dev/examples

open Sutil

open Sutil.Dsl
open Sutil.CoreElements
open Sutil.Bind

open Sutil.Styling

open type Feliz.length

type Thing = { Id : int; Color : string }
let Color t = t.Color
let Id t = t.Id

let ThingView (viewId : int) (thing : Thing) : SutilElement =
        let initialColor = thing |> Color

        let thingStyle = [
            rule "span" [
                Css.displayInlineBlock
                Css.padding( em 0.2, em 0.5 )
                Css.margin( zero, em 0.2, em 0.2, zero )
                Css.width (em 8)
                Css.textAlignCenter
                Css.borderRadius (em 0.2)
                Css.color "#eeeeee"
            ]
        ]

        Html.div [
            Html.p [
                Html.span [ Attr.style [ Css.backgroundColor thing.Color ]; text $"{thing.Id} {thing.Color} #{viewId}" ]
                Html.span [ Attr.style [ Css.backgroundColor initialColor ]; text "initial" ]
            ] |> withStyle thingStyle
        ]

let view() =
    let nextId = Helpers.createIdGenerator()

    let makeThing thing = ThingView (nextId()) thing

    let things = Store.make [
        { Id = 1; Color = "darkblue" }
        { Id = 2; Color = "indigo" }
        { Id = 3; Color = "deeppink" }
        { Id = 4; Color = "salmon" }
        { Id = 5; Color = "gold" }
    ]

    let handleClick _ =
        things |> Store.modify List.tail

    Html.div [
        disposeOnUnmount [things]

        Html.button [
            Ev.onClick handleClick
            text "Remove first thing"
        ]

        Html.div [
            Attr.style [ Css.displayGrid; Css.gridTemplateColumns [fr 1; fr 1]; Css.gap (em 1) ]

            Html.div [
                Html.h2 [ text "Keyed" ]
                Bind.each( things, makeThing, Id )
            ]

            Html.div [
                Html.h2 [ text "Unkeyed" ]
                Bind.each( things, makeThing )
            ]
        ]
    ]
