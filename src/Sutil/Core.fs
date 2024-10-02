
module Sutil.Core

open CoreTypes
open Browser.Dom
open Browser.Types

open DomHelpers.CustomEvents

let private logPatch (patchAction) = 
    //Fable.Core.JS.console.log( "Patch action: ", Fable.Core.JS.JSON.stringify( patchAction, space = 4) )
    // Fable.Core.JS.console.log( "Patch action: ", patchAction.ToString() )
    patchAction

let internal notifySutilEvents  (node : Node) =
    if DomHelpers.isConnected (node.parentNode) then
        Fable.Core.JS.console.log( "Connected", node)

        CustomDispatch<_>.dispatch( node, CustomEvent.Connected )
        CustomDispatch<_>.dispatch( node, CustomEvent.Mount )

        node
        |> DomHelpers.descendants
        |> Seq.filter DomHelpers.isElementNode
        |> Seq.iter (fun n ->  CustomDispatch<_>.dispatch(n,CustomEvent.Mount))
    else
        Fable.Core.JS.console.log( "Not connected", node)

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

let mount (context : BuildContext) (current : Node) (se : SutilElement) : Node =
    se
    |> VirtualDom.fromSutil                 // SutilElement -> VirtualDom.Element
    |> Patch.calculatePatch current         // Calculate patch for new element
    //|> logPatch                             // Log the patch (debug)
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

    // se 
    // |> VirtualDom.fromSutil 
    // |> VirtualDom.toDom context
    // |> fun node ->
    //     DomHelpers.append container node
    //     notifySutilEvents node

let unsubscribeOnUnmount (fns : (unit -> unit) seq ) =
    SutilElement.SideEffect(
        "unsubscribeOnUnmount",
        (fun context ->
            DomHelpers.EventListeners.add
                context.ParentElement
                CustomEvent.Unmount
                (fun _ -> fns |> Seq.iter (fun u -> u())) 
            
        )
    )


let disposeOnUnmount (fns : (System.IDisposable) seq ) =
    SutilElement.SideEffect(
        "disposeOnUnmount",
        (fun context ->
            DomHelpers.EventListeners.add
                context.ParentElement
                CustomEvent.Unmount
                (fun _ -> fns |> Seq.iter (fun u -> u.Dispose())) 
            
        )
    )


let hookParent (f : HTMLElement -> unit) =
    SutilElement.SideEffect(
        "hookParent",
        (fun context ->
            DomHelpers.EventListeners.add
                context.ParentElement
                CustomEvent.Mount
                (fun e -> (e.target :?> HTMLElement) |> f) 
            
        )
    )