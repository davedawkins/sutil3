module Sutil.CoreElements

open Sutil
open Sutil.Core
open Sutil.Internal
open Browser.Types
open Sutil.Internal.CustomEvents

let debug (se: SutilElement) =
    Log.Console.log ("debug: Adding element")

    SutilElement.BuildMap(
        (fun ctx ->
            Log.Console.log ("debug: Mapping build context to enable logging")
            ctx.WithLogEnabled()
        ),
        se
    )

let __debug (se: SutilElement) =
    SutilElement.Define(
        "debug",
        (fun context -> se |> Sutil.Core.mount (context.WithLogEnabled()) null)
    )

let unsubscribeOnUnmount (fns: (unit -> unit) seq) =
    SutilElement.Define(
        "unsubscribeOnUnmount",
        (fun context ->
            fns
            |> Seq.iter ((Dispose.addUnsubscribe context.ParentElement "unsubscribeOnUnmount"))
        )
    )

let disposeOnUnmount (fns: (System.IDisposable) seq) =
    SutilElement.Define(
        "disposeOnUnmount",
        (fun ctx -> fns |> Seq.iter (Dispose.addDisposable ctx.ParentElement "disposeOnUmount"))
    )

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
            ctx.Parent.asElement
            |> Option.map (fun el ->
                let host = ctx.CreateElement "div"

                // Parse HTML and add to new node
                host.innerHTML <- text.Trim()

                // Tell Patcher that no point in trying repair this section
                host.setAttribute ("data-sutil-imported", "html")

                // Add into the DOM
                ctx.AppendNode el host

                // Let styling know that a new node needs marking up
                ctx.OnImportedNode host

                // Let code highligher (index.html) know that new code needs marking up
                Sutil.Internal.CustomEvents.notifySutilUpdated (host.ownerDocument)

                SutilResult.Of(Appended, host)
            )
            |> Option.defaultWith (fun _ -> failwith "Not an element")
    )

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

// let postProcess (f : SutilEffect -> SutilEffect) (view : SutilElement) : SutilElement =
//     SutilElement.Define( "postProcess", fun ctx -> ctx |> build view |> f )

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

let postProcessElements (f: HTMLElement -> unit) (se: SutilElement) : SutilElement =
    postProcessElementsWithName ("postProcessElement") f se

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
