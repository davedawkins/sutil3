
module Core

open CoreTypes
open Browser.Dom

let newNodesOf (result : SutilBuildResult) =
    match result with
    | PatchedNode _ | PatchedElement _ -> None
    | DomNode n -> Some [n]
    | DomElement (e,_) -> Some [e]
    | DomSideEffect -> None
//        | DomFragment ns -> ns |> Array.toList |> Some

let toNode (result : SutilBuildResult) =
    match result with
    | PatchedNode _ | PatchedElement _ -> None
    | DomNode n -> Some n
    | DomElement (e,_) -> Some e
    | DomSideEffect -> None
//        | DomFragment ns -> ns |> Array.toList |> Some

let iterNewNodes  (nodeAction) (result : SutilBuildResult) =
    result |> newNodesOf |> Option.iter nodeAction

let insertResultNode (context : BuildContext) (result : SutilBuildResult) =
    result |> toNode |> Option.iter (fun node ->
        DomHelpers.replace context.ParentElement context.Existing node
    )

let insertResultNodes (context : BuildContext) (result : SutilBuildResult) =
    result |> iterNewNodes (fun nodes ->
        DomHelpers.replaceNodes context.ParentElement [| context.Existing |] (Array.ofList nodes)
    )

let mountAsChild (parentElementId : string)  (se : SutilElement) =
    let container = document.getElementById(parentElementId)

    if isNull container then
        failwith ("Cannot find element for mount point: " + parentElementId)

    let ve = VirtualDom.fromSutil se
    let de = VirtualDom.toDom ve

    DomHelpers.append container de
