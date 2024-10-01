
module Bind

open Core
open CoreTypes
open Store
open Browser.Types

/// Find new nodes that were created during the 
/// patch operation, and welcome them into the DOM
/// so they can run their onMount handlers etc
let rec private notifyNewNodes (result, node) =
    match result with
    | Patch.Replaced | Patch.Appended ->
        Core.notifySutilEvents node
    | Patch.Patched (results) ->
        results |> Array.iter notifyNewNodes
    | _ -> ()

let private logPatch (patchAction) = 
    //Fable.Core.JS.console.log( "Patch action: ", Fable.Core.JS.JSON.stringify( patchAction, space = 4) )
    // Fable.Core.JS.console.log( "Patch action: ", patchAction.ToString() )
    patchAction

// Simplest binding
type Bind =
    static member el( source : 'T observable, view : ('T -> SutilElement) ) =
        SutilElement.SideEffect <|
            fun context -> 

                let mutable currentNode : Node = null

                source.Subscribe( fun value ->

                    value
                    |> view                                 // Create a SutilElement from the new value
                    |> VirtualDom.fromSutil                 // SutilElement -> VirtualDom.Element
                    |> Patch.calculatePatch currentNode     // Calculate patch for new element
                    |> logPatch                             // Log the patch (debug)
                    |> Patch.applyPatch context currentNode // Apply patch
                    |> fun (result, node) ->
                        notifyNewNodes (result,node)        // Notify new nodes
                        currentNode <- node                 // Record current node

                ) |> DomHelpers.Dispose.addDisposable (context.ParentElement) // FIXME: Put this in the disposals list
    
