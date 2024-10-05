module Nested

// Adapted from
// https://svelte.dev/examples

open Sutil.Dsl
open Sutil.Styling

let Nested() =
   Html.p [ text "...don't affect this element" ] |> withStyle []

