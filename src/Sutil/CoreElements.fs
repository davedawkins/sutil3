module Sutil.CoreElements

open Sutil
open Sutil.Core
open Sutil.Dom
open Browser.Types
open Sutil.Dom.CustomEvents
open Sutil.Dom.TypeHelpers


let debug (se : SutilElement) =
    SutilElement.SideEffect(
        "debug",
        (fun context ->
            se
            |> Sutil.Core.mount (context.WithLogEnabled()) null
            |> CreatedNode
        )
    )


let unsubscribeOnUnmount (fns : (unit -> unit) seq ) =
    SutilElement.SideEffect(
        "unsubscribeOnUnmount",
        (fun context ->
            fns |> Seq.iter ((Dispose.addUnsubscribe context.ParentElement))            
            DomEffect
        )
    )


let disposeOnUnmount (fns : (System.IDisposable) seq ) =
    SutilElement.SideEffect(
        "disposeOnUnmount",
        (fun context ->
            fns |> Seq.iter (Dispose.addDisposable context.ParentElement)            
            DomEffect
        )
    )


let hookParent (f : HTMLElement -> unit) =
    SutilElement.SideEffect(
        "hookParent",
        (fun context ->
            EventListeners.add
                context.ParentElement
                MOUNT
                (fun e -> (e.target :?> HTMLElement) |> f) |> ignore
            
            DomEffect
        )
    )


/// <summary>
/// Raw html that will be parsed and added as a child of the parent element
/// </summary>
let html (text : string) : SutilElement =
    SutilElement.Define( "html",
    fun ctx ->
        ctx.Parent.asElement 
        |> Option.map (fun el ->
            let host = ctx.CreateElement "div"

            // Parse HTML and add to new node
            host.innerHTML <- text.Trim()

            // Tell Patcher that no point in trying repair this section
            host.setAttribute( "data-sutil-imported", "html")

            // Add into the DOM
            ctx.AppendNode el host

            // Let styling know that a new node needs marking up
            ctx.OnImportedNode host

            // Let code highligher (index.html) know that new code needs marking up
            Sutil.Dom.CustomEvents.notifySutilUpdated (host.ownerDocument)

            CreatedNode host
        )
        |> Option.defaultValue (SutilEffectResult.DomEffect)
    )

// let postProcess (f : SutilEffect -> SutilEffect) (view : SutilElement) : SutilElement =
//     SutilElement.Define( "postProcess", fun ctx -> ctx |> build view |> f )

let postProcessElementsWithName (name : string) (f : HTMLElement -> unit) (se : SutilElement) : SutilElement =

    let run ( context : BuildContext )  =
        let el = 
            se 
            |> Sutil.Core.mount 
                    context //(context.WithLogEnabled()) 
                    null 
            |> asElement
        f el
        CreatedNode el

    SutilElement.SideEffect (name, run)

let postProcessElements (f : HTMLElement -> unit) (se : SutilElement) : SutilElement =
    postProcessElementsWithName ("postProcessElement") f se 

open Core.Sutil2

let headStylesheet (url : string) : SutilElement =
    SutilElement.Define( "headStyleSheet",
        fun ctx -> 
            DomEdit.setHeadStylesheet ctx.Document url 
            DomEffect
    )

let headScript (url : string) : SutilElement =
    SutilElement.Define( "headScript",
        fun ctx -> 
            DomEdit.setHeadScript ctx.Document url 
            DomEffect
    )

let headEmbedScript (source : string) : SutilElement =
    SutilElement.Define( "headEmbedScript",
        fun ctx -> 
            DomEdit.setHeadEmbedScript ctx.Document source 
            DomEffect
    )

let headTitle (title : string) : SutilElement =
    SutilElement.Define( "headTitle",
        fun ctx -> 
            DomEdit.setHeadTitle ctx.Document title
            DomEffect
    )