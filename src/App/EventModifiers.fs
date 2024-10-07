module EventModifiers

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Html
open Sutil.CoreElements

open Browser.Dom

let handleClick _ =
    window.alert("no more alerts")

let view() = Html.button [
    Sutil.Dom.EventListeners.once "click" handleClick
    onClick handleClick [Once]
    text "Click me"
]
