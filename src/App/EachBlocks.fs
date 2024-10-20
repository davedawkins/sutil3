module EachBlocks

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind

type Cat =
    {
        Id: string
        Name: string
    }
    static member Create( id : string, name : string ) = { Id = id; Name = name }

let view () =
    let extraCat = Cat.Create( "0Bmhjf0rKe8", "Surprise Kitten" )

    let cats =
        Store.make [
            Cat.Create( "J---aiyznGQ", "Keyboard Cat" )
            Cat.Create( "z_AbfPXTKms", "Maru" )
            Cat.Create( "OUtn3pvWmpg", "Henri The Existential Cat")
        ]

    let addCat cat =
        cats
        |> Store.modify (fun x ->
            x
            @ [
                cat
            ]
        )

    Html.div [
        disposeOnUnmount [
            cats
        ]

        Html.h4 [
            text "The Famous Cats of YouTube"
        ]
        Html.ul [
            // Each with dynamic binding, and index.
            // If the list changes, the view will update accordingly.
            // It isn't necessary to make a store to loop over a data structure,
            // see StaticEach.fs and StaticEachWithIndex.fs
            Bind.eachi (
                cats,
                (fun (i, cat) ->
                    Html.li [
                        Html.a [
                            Attr.target "_blank"
                            Attr.href $"https://www.youtube.com/watch?v={cat.Id}"
                            text $"{i + 1}: {cat.Name}"
                        ]
                    ]
                )
            )
        ]
        Bulma.button [
            Attr.style [
                Css.marginTop 12
            ]
            text "More Cats"
            Bind.attr ("disabled", (cats |> Store.map (fun cats' -> cats'.Length = 4)))
            Ev.onClick (fun _ -> addCat extraCat)
        ]
    ]
