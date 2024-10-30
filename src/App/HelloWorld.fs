module HelloWorld

// Adapted from https://svelte.dev/playground/hello-world

open Sutil.Html

let name = "World"

let view () =
    Html.div [
        text $"Hello {name}!"
    ]
