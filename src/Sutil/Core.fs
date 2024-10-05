
module Sutil.Core

open Browser.Dom
open Browser.Types

open Sutil
open Sutil.Dom.DomHelpers
open Sutil.Dom
open Sutil.Dom.CustomEvents
open Sutil.Dom.Types
open Sutil.Dom.TypeHelpers

let private logElement (context : BuildContext) (current : Node) (velement : VirtualDom.Element) = 
    Fable.Core.JS.console.log(sprintf "New element: %s" (velement.ToString()) )
    velement

let private logPatch (context : BuildContext) (current : Node) (patchAction) = 
    //Fable.Core.JS.console.log( "Patch action: ", Fable.Core.JS.JSON.stringify( patchAction, space = 4) )
    Fable.Core.JS.console.log( 
        sprintf "Patch action:\n current= %s\n action=%s\n parent=%s" 
            (outerHTML current)
            (patchAction.ToString())
            (context.ParentElement.outerHTML)
            )
    patchAction

type SutilEffect =
    static member RegisterDisposable( node : Node, disposable : System.IDisposable ) =
        Dispose.addDisposable node disposable
    static member RegisterUnsubscribe( node : Node, disposable :Unsubscribable ) =
        Dispose.addUnsubscribe node disposable

let internal notifySutilEvents  (node : Node) =
    if isConnected (node.parentNode) then
        CustomDispatch<_>.dispatch( node, Connected )
        CustomDispatch<_>.dispatch( node, Mount )

        node
        |> descendants
        |> Seq.filter isElementNode
        |> Seq.iter (fun n ->  CustomDispatch<_>.dispatch(n, Mount))

let exclusive (se : SutilElement) =
    ()

/// Find new nodes that were created during the 
/// patch operation, and welcome them into the DOM
/// so they can run their onMount handlers etc
let rec private notifyNewNodes (result, node) =
    match result with
    | Patch.Replaced | Patch.Appended ->
        notifySutilEvents node
    | Patch.Patched (results) ->
        results |> Array.iter notifyNewNodes
    | _ -> ()

let create (context : BuildContext) (se : SutilElement) : Node =
    se
    |> VirtualDom.fromSutil                 // SutilElement -> VirtualDom.Element
    |> VirtualDom.toDom context

let mount (context : BuildContext) (current : Node) (se : SutilElement) : Node =
    se
    |> VirtualDom.fromSutil
    |> logElement context current           // SutilElement -> VirtualDom.Element
    |> Patch.calculatePatch current         // Calculate patch for new element
    |> logPatch context current             // Log the patch (debug)
    |> Patch.applyPatch context current     // Apply patch
    |> fun (result, node) ->
        notifyNewNodes (result,node)        // Notify new nodes
        node                                // Return new node

let mountAsChild (parentElementId : string)  (se : SutilElement) =
    let container = document.getElementById(parentElementId)

    if isNull container then
        failwith ("Cannot find element for mount point: " + parentElementId)

    let context = BuildContext.Create()

    se 
    |> mount context null 

module Sutil2 =
    let attr (name,value) = 
        SutilElement.Attribute( name, value )

    let el (tag : string) (children : SutilElement seq) =
        SutilElement.Element( tag, children |> Seq.toArray )

    let text s = SutilElement.Text s

    let listen event (target : EventTarget) fn = 
        EventListeners.add target event fn

    let once event node fn = EventListeners.once event node fn

    let fragment children = SutilElement.Fragment (children |> Seq.toArray)

    module Interop =
        let get data name = 
            JsMap.getKey data name

        let set data name value =
            JsMap.setKey data name value

    let build (se : SutilElement) (ctx : BuildContext) =
        mount ctx null se
    
    module Logging =
        let error (s : string) = Log.Console.log("Error: " + s)

    [<AutoOpen>]
    module Ext =
        type BuildContext with
            member __.ParentNode = __.Parent
            member __.Document = document

    let documentOf (node : Node) = node.ownerDocument
