/// <summary>
/// Support for listening and reacting to window resize events
/// </summary>
module Sutil.ResizeObserver

// Ported from Svelte

open Browser.Types
open Browser.Dom
open Browser.CssExtensions
open System
open Core.Sutil2
open Sutil.Dom

let isCrossOrigin = false // TODO

type private ResizeSubscriber = {
    Callback: unit -> unit
    Id : int
}

let [<Literal>] private ResizeObserverKey = "__sutil_resize"

type ResizeObserver( el : HTMLElement ) =
    let mutable iframe : HTMLIFrameElement = Unchecked.defaultof<_>
    let mutable subId = 0
    let mutable unsubscribe : Unsubscriber = Unchecked.defaultof<_>
    let mutable subscribers = []

    let notify _ =
        subscribers |> List.iter (fun sub -> sub.Callback())

    do
        let computedStyle = window.getComputedStyle(el)
        let zIndex =  (try int(computedStyle.zIndex) with |_ -> 0) - 1;

        if computedStyle.position = "static" || computedStyle.position = "" then
            el.style.position <- "relative"

        iframe <- downcast (documentOf el).createElement("iframe")
        let style = sprintf "%sz-index: %i;" "display: block; position: absolute; top: 0; left: 0; width: 100%; height: 100%; overflow: hidden; border: 0; opacity: 0; pointer-events: none;" zIndex
        iframe.setAttribute("style", style)
        iframe.setAttribute("aria-hidden", "true")
        iframe.setAttribute("tabindex", "-1")

        if isCrossOrigin then
            iframe.setAttribute("src", "data:text/html,<script>onresize=function(){parent.postMessage(0,'*')}</script>")

            unsubscribe <- listen "message" window
                (fun e -> if JsHelpers.eq3 (JsMap.getKey e "source") iframe.contentWindow then notify(e))
        else
            iframe.setAttribute("src", "about:blank")
            iframe.onload <- (fun e ->
                unsubscribe <- listen "resize" iframe.contentWindow notify)

        el.appendChild(iframe) |> ignore

    member _.Subscribe(callback : (unit -> unit)) =
        let sub = { Callback = callback; Id = subId }
        subId <- subId + 1
        subscribers <- sub :: subscribers
        (fun () -> subscribers <- subscribers |> List.filter (fun s -> s.Id <> sub.Id)) |>  Dispose.makeDisposable

    member _.Dispose() =
        try unsubscribe() with |_ -> ()
        if not (isNull iframe) then
            iframe.parentNode.removeChild(iframe) |> ignore

    interface IDisposable with
        member this.Dispose() = this.Dispose()

let getResizer (el:HTMLElement) : ResizeObserver =
    JsMap.getCreate el ResizeObserverKey (fun () -> new ResizeObserver(el))
