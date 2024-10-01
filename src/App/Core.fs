
module Core

open CoreTypes
open Browser.Dom

let mountAsChild (parentElementId : string)  (se : SutilElement) =
    let container = document.getElementById(parentElementId)

    if isNull container then
        failwith ("Cannot find element for mount point: " + parentElementId)

    let context = BuildContext.Create()
    se 
    |> VirtualDom.fromSutil 
    |> VirtualDom.toDom context
    |> DomHelpers.append container

