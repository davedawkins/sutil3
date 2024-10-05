module Sutil.CoreElements

open Sutil
open Sutil.Dom
open Sutil.Dom.Types
open Browser.Types
open Sutil.Dom.CustomEvents
open Sutil.Dom.TypeHelpers

let unsubscribeOnUnmount (fns : (unit -> unit) seq ) =
    SutilElement.SideEffect(
        "unsubscribeOnUnmount",
        (fun context ->
            Log.Console.log("unsubscribeOnUnmount: ", context.ParentElement.outerHTML )
            fns |> Seq.iter ((Dispose.addUnsubscribe context.ParentElement)<<Unsubscribe)            
            EffectedNode context.ParentElement
        )
    )


let disposeOnUnmount (fns : (System.IDisposable) seq ) =
    SutilElement.SideEffect(
        "disposeOnUnmount",
        (fun context ->
            fns |> Seq.iter (Dispose.addDisposable context.ParentElement)            
            EffectedNode context.ParentElement
        )
    )


let hookParent (f : HTMLElement -> unit) =
    SutilElement.SideEffect(
        "hookParent",
        (fun context ->
            EventListeners.add
                context.ParentElement
                Mount
                (fun e -> (e.target :?> HTMLElement) |> f) |> ignore
            
            EffectedNode context.ParentElement
        )
    )


/// <summary>
/// Raw html that will be parsed and added as a child of the parent element
/// </summary>
let html (text : string) : SutilElement =
    SutilElement.Define( "html",
    fun ctx ->
        ctx.Parent.asElement 
        |> Option.iter (fun el ->
            el.innerHTML <- text.Trim()

            // Fix me
            // ctx.Class
            // |> Option.iter (fun cls -> visitElementChildren el (fun ch -> ClassHelpers.addToClasslist cls ch ))

            // match JsMap.getKey (ctx.ParentElement) (NodeKey.StyleClass) with
            // | None -> ()
            // | Some styleClass ->
            //     visitElementChildren el (fun ch ->
            //         ClassHelpers.addToClasslist styleClass ch
            //         //applyCustomRules ns ch
            //     )

            // Event.notifyUpdated ctx.Document
        )

        let nodes = ctx.ParentNode.childNodes.toSeq() |> Seq.toArray

        if nodes.Length = 1 then
            nodes[0] |> CreatedNode
        else
            failwith "Expected single root node"
            // let group = SutilEffect.MakeGroup( "html", ctx.Parent, ctx.Previous )
            // nodes |> Seq.iter (fun n -> group.AddChild(DomNode n))
            // group |> Group |> sutilResult
    )

// let postProcess (f : SutilEffect -> SutilEffect) (view : SutilElement) : SutilElement =
//     SutilElement.Define( "postProcess", fun ctx -> ctx |> build view |> f )

let postProcessElementsWithName (name : string) (f : HTMLElement -> unit) (se : SutilElement) : SutilElement =

    let run ( context : BuildContext )  =
        let el = Sutil.Core.mount context null se |> asElement
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
            EffectedNode (ctx.ParentElement)
    )

let headScript (url : string) : SutilElement =
    SutilElement.Define( "headScript",
        fun ctx -> 
            DomEdit.setHeadScript ctx.Document url 
            EffectedNode (ctx.ParentElement)
    )

let headEmbedScript (source : string) : SutilElement =
    SutilElement.Define( "headEmbedScript",
        fun ctx -> 
            DomEdit.setHeadEmbedScript ctx.Document source 
            EffectedNode (ctx.ParentElement)
    )

let headTitle (title : string) : SutilElement =
    SutilElement.Define( "headTitle",
        fun ctx -> 
            DomEdit.setHeadTitle ctx.Document title
            EffectedNode (ctx.ParentElement)
    )