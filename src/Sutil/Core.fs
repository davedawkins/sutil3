
module Core

open CoreTypes
open Browser.Dom
open Browser.Types

open DomHelpers.CustomEvents

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

let mountAsChild (parentElementId : string)  (se : SutilElement) =
    let container = document.getElementById(parentElementId)

    if isNull container then
        failwith ("Cannot find element for mount point: " + parentElementId)

    let context = BuildContext.Create()

    se 
    |> VirtualDom.fromSutil 
    |> VirtualDom.toDom context
    |> fun node ->
        DomHelpers.append container node
        notifySutilEvents node
