module Sutil.CoreElements

open Sutil
open Sutil.Core
open Sutil.Internal
open Browser.Types
open Sutil.Internal.CustomEvents

/// Call this function when the element is mounted
let onElementMounted<'Element when 'Element :> HTMLElement> (f: 'Element -> unit) : SutilElement =
    Log.Console.info("onElementMounted: ctor")
    Basic.event
        CustomEvents.MOUNT
        (fun (e : Event) -> 
            Log.Console.log("onElementMounted: ", e.target.asElement |> DomHelpers.toStringOutline)
            (e.target.asElement :?> 'Element) |> f
        )

let hookParent (f : HTMLElement -> unit) = onElementMounted f

/// Call these unit functions when the parent element is unmounted
let unsubscribeOnUnmount (fns: (unit -> unit) seq) =
    onElementMounted
        (fun el -> fns |> Seq.iter (Dispose.addUnsubscribe el "unsubscribeOnUnmount"))

/// Call _.Dispose() for these IDisposables when the parent element is unmounted
let disposeOnUnmount (fns: (System.IDisposable) seq) =
    onElementMounted 
        (fun el -> fns |> Seq.iter (Dispose.addDisposable el "disposeOnUmount") )

/// Helper function for creating a resource that will be disposed automatically when the element is unmounted.
/// The handler is called once, when the containing element is mounted
let bindDisposable<'Element when 'Element :> HTMLElement>(handler: 'Element -> System.IDisposable) =
    onElementMounted( fun el ->
        Dispose.addDisposable el "bindSubEl" (handler el)
    )

/// Helper function for making a subscription to an IObservable that will be released when the element is unmounted
/// A subscription is made on the source when the container element is mounted.
let bindSubscribe<'T,'Element when 'Element :> HTMLElement> (source: System.IObservable<'T>) (handler: 'Element -> 'T -> unit) =
    bindDisposable (fun el -> source.Subscribe (handler el))

let private _html_log = Log.create("html")
_html_log.enabled <- true

/// <summary>
/// Raw html that will be parsed and added as a child of the parent element
/// </summary>
let html (text: string) : SutilElement =
    SutilElement.DefineBinding(
        "html",
        Basic.el "div" [
            Basic.attr "data-sutil-imported" "html" // Tell Patcher that no point in trying repair this section
        ],
        fun ctx ->
            let host = ctx.Current :?> HTMLElement  // We know this will be a div, passed as init in DefineBinding
            
            // Parse HTML and add to new node
            host.innerHTML <- text.Trim()
            
            //Let styling (eg) know that a new node needs marking up
            host
            |> DomHelpers.children
            |> Seq.iter (ctx.NotifyNodeImported)

            // Let code highligher (index.html) know that new code could need marking up
            Sutil.Internal.CustomEvents.notifySutilUpdated (host.ownerDocument)
    )

/// Call the dispatch function when the parent element is resized
let listenToResize (dispatch: HTMLElement -> unit) : SutilElement =
    onElementMounted <|
        fun parent -> 
            let notify() = dispatch parent
            SutilEffect.RegisterDisposable(
                parent,
                "listenToResize",
                (ResizeObserver.getResizer parent).Subscribe( notify )
            )
            DomHelpers.rafu notify

let postProcessElementsWithName
    (name: string)
    (f: HTMLElement -> unit)
    (element: SutilElement)
    : SutilElement
    =

        // let run (context: BuildContext) =
    //     let result = se |> Sutil.Core.mount context null
    //     result.Node.asElement |> Option.iter f
    //     result
    SutilElement.DefineMapping(
        name,

        (fun context ->
                context
                    .WithOnImportedNode(fun (node:Node) -> 
                        Log.Console.log("postProcess: " + name + ": " + (node.textContent))
                        node.asElement |> Option.iter f
                    )
        ),

        element
    )
//    SutilElement.Define(name, run)

/// Pass the generated HTMLElement for child 'se' to the handler function
let postProcessElements (handler: HTMLElement -> unit) (se: SutilElement) : SutilElement =
    postProcessElementsWithName ("postProcessElement") handler se
