module EventModifiers

// Adapted from
// https://svelte.dev/examples

open Sutil.Html

open Browser.Dom

let handleClick _ = window.alert ("no more alerts")

let view () =
    Bulma.button [
        Ev.once ("click", handleClick)
        text "Click me"
    ]
