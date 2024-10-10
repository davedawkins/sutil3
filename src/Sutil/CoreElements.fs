module Sutil.CoreElements

open Sutil
open Sutil.Core
open Sutil.Internal
open Browser.Types
open Sutil.Internal.CustomEvents

/// Call these unit functions when the parent element is unmounted
let unsubscribeOnUnmount (fns: (unit -> unit) seq) =
    SutilElement.Define(
        "unsubscribeOnUnmount",
        (fun context ->
            fns
            |> Seq.iter ((Dispose.addUnsubscribe context.ParentElement "unsubscribeOnUnmount"))
        )
    )

/// Call _.Dispose() for these IDisposables when the parent element is unmounted
let disposeOnUnmount (fns: (System.IDisposable) seq) =
    SutilElement.Define(
        "disposeOnUnmount",
        (fun ctx -> fns |> Seq.iter (Dispose.addDisposable ctx.ParentElement "disposeOnUmount"))
    )

/// Call this function when the element is mounted
let hookParent (f: HTMLElement -> unit) =
    SutilElement.Define(
        "hookParent",
        (fun context ->
            EventListeners.add
                context.ParentElement
                MOUNT
                (fun e -> (e.target :?> HTMLElement) |> f)
            |> ignore

            SutilResult.Of(SutilResultType.Effected "hookParent", context.ParentElement)
        )
    )

/// <summary>
/// Raw html that will be parsed and added as a child of the parent element
/// </summary>
let html (text: string) : SutilElement =
    SutilElement.Define(
        "html",
        fun ctx ->
            let host = ctx.CreateElement "div"

            // Parse HTML and add to new node
            host.innerHTML <- text.Trim()

            // Tell Patcher that no point in trying repair this section
            host.setAttribute ("data-sutil-imported", "html")

            // Add into the DOM
            ctx.AppendNode (ctx.Parent) host

            // Let styling know that a new node needs marking up
            ctx.OnImportedNode host

            // Let code highligher (index.html) know that new code needs marking up
            Sutil.Internal.CustomEvents.notifySutilUpdated (host.ownerDocument)

            SutilResult.Of(Appended, host)
    )

/// Call the dispatch function when the parent element is resized
let listenToResize (dispatch: HTMLElement -> unit) : SutilElement =
    SutilElement.Define(
        "listenToResize",
        fun ctx ->
            let parent: HTMLElement = ctx.ParentElement
            let notify () = dispatch parent

            EventListeners.once
                CustomEvents.MOUNT
                parent
                (fun _ ->
                    SutilEffect.RegisterDisposable(
                        parent,
                        "listenToResize",
                        (ResizeObserver.getResizer parent).Subscribe(notify)
                    )

                    DomHelpers.rafu notify
                )
            |> ignore

    )

let postProcessElementsWithName
    (name: string)
    (f: HTMLElement -> unit)
    (se: SutilElement)
    : SutilElement
    =

    let run (context: BuildContext) =
        let result = se |> Sutil.Core.mount context null
        result.Node.asElement |> Option.iter f
        result

    SutilElement.Define(name, run)

/// Pass the generated HTMLElement for child 'se' to the handler function
let postProcessElements (handler: HTMLElement -> unit) (se: SutilElement) : SutilElement =
    postProcessElementsWithName ("postProcessElement") handler se

open Core.Sutil2

let headStylesheet (url: string) : SutilElement =
    SutilElement.Define("headStyleSheet", fun ctx -> DomEdit.setHeadStylesheet ctx.Document url)

let headScript (url: string) : SutilElement =
    SutilElement.Define("headScript", fun ctx -> DomEdit.setHeadScript ctx.Document url)

let headEmbedScript (source: string) : SutilElement =
    SutilElement.Define(
        "headEmbedScript",
        fun ctx -> DomEdit.setHeadEmbedScript ctx.Document source
    )

let headTitle (title: string) : SutilElement =
    SutilElement.Define("headTitle", fun ctx -> DomEdit.setHeadTitle ctx.Document title)
