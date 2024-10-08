module Sutil.Core

open Browser.Dom
open Browser.Types

open Sutil
open Sutil.Internal
open Sutil.Internal.DomHelpers
open Sutil.Internal.CustomEvents
open Sutil.Internal.TypeHelpers
open VirtualDom
open CalculatePatch

let private _log = Log.create ("Core")

_log.enabled <- false

[<AutoOpen>]
module CoreExtensions =

    type SutilElement with
        static member Define(name: string, f: BuildContext -> SutilResult) =
            SutilElement.SideEffect(name, f)

        static member Define(name: string, f: BuildContext -> unit) =
            SutilElement.SideEffect(
                name,
                fun ctx ->
                    f (ctx)
                    SutilResult.Of(Effected name, ctx.ParentElement)
            )

        static member DefineBinding(name: string, f: BuildContext -> unit) =
            SutilElement.BindElement(name, f)

        static member HiddenDiv(name: string) =
            SutilElement.Element(
                "div",
                [|
                    SutilElement.Attribute("style", "display:none")
                    SutilElement.Attribute("data-name", name)
                |]
            )

let private logElement (context: BuildContext) (current: Node) (velement: VirtualElement) =
    if context.LogElementEnabled then
        _log.trace (">> -------------------------------------------------------")
        _log.trace (sprintf "New element: %s" (velement.AsString()))

    velement

let private logPatch (context: BuildContext) (current: Node) (patchAction: NodeAction) =
    if context.LogPatchEnabled then
        _log.trace (
            sprintf
                "Patch action:\n current= %s\n action=%s\n parent=%s"
                (outerHTML current)
                (patchAction.ToString())
                (context.ParentElement.outerHTML)
        )

    patchAction

type SutilEffect =
    static member RegisterDisposable(node: Node, name: string, disposable: System.IDisposable) =
        Dispose.addDisposable node name disposable

    static member RegisterUnsubscribe(node: Node, name: string, disposable: Unsubscriber) =
        Dispose.addUnsubscribe node name disposable

let internal notifyMount (node: Node) =
    if _log.enabled then
        _log.trace ("Mount: notifying", Internal.DomHelpers.toStringOutline node)

    CustomDispatch<_>.dispatch (node, MOUNT)

let internal notifySutilEvents (node: Node) =
    if _log.enabled then
        _log.trace ("Mount: New node", node |> DomHelpers.toString)

    if isConnected (node.parentNode) then
        CustomDispatch<_>.dispatch (node, CONNECTED)
        notifyMount node

        node
        // The mount handlers for bindings can change the tree, so traverse the tree from the bottom
        // up, and in reverse child order. Edits *shouldn't* change the next pointers the nodes
        // we have yet to visit...
        |> descendantsDepthFirstReverse
        |> Seq.toArray // ... but just to be sure. Mount handlers should probably check that the node is connected (still)
        |> Seq.filter isElementNode
        |> Seq.iter notifyMount
    else if _log.enabled then
        _log.trace ("Not connected: ", Internal.DomHelpers.toStringSummary node)

/// Find new nodes that were created during the
/// patch operation, and welcome them into the DOM
/// so they can run their onMount handlers etc
let rec private notifyNewNodes (result: SutilResult) =
    match result.Result with
    | Replaced
    | Appended -> notifySutilEvents result.Node
    | Patched(results) -> notifySutilEvents result.Node
    // results |> Array.iter (fun patchResult ->
    //     match patchResult with
    //     | ChildResult result | EffectResult result -> notifyNewNodes result
    //     | _ -> ())
    | _ -> ()

let mount (context: BuildContext) (current: Node) (se: SutilElement) : SutilResult =
    if _log.enabled then
        _log.trace ("Mount: building", "parent=", context.Parent |> DomHelpers.toStringOutline)

    let context = context.WithLogEnabled()

    se
    |> VirtualDom.fromSutil
    |> context.VirtualElementMapper
    |> logElement context current // SutilElement -> VirtualDom.Element
    |> Patch.calculate current // Calculate patch for new element
    |> logPatch context current // Log the patch (debug)
    |> Patch.apply context current // Apply patch
    |> fun (result) ->
        notifyNewNodes result // Notify new nodes
        result

/// Helper functions in porting core library from Sutil2.
/// TODO: Refactor these away
module Sutil2 =
    let attr (name, value) = SutilElement.Attribute(name, value)

    let el (tag: string) (children: SutilElement seq) =
        SutilElement.Element(tag, children |> Seq.toArray)

    let text s = SutilElement.Text s

    let listen event (target: EventTarget) fn = EventListeners.add target event fn

    let once event node fn = EventListeners.once event node fn

    let fragment children =
        SutilElement.Fragment(children |> Seq.toArray)

    module Interop =
        let get data name = JsMap.getKey data name

        let set data name value = JsMap.setKey data name value

    let build (se: SutilElement) (ctx: BuildContext) = mount ctx null se

    module Logging =
        let error (s: string) = _log.trace ("Error: " + s)

    [<AutoOpen>]
    module Ext =
        type BuildContext with
            member __.ParentNode = __.Parent
            member __.Document = document

    let documentOf (node: Node) = node.ownerDocument


module Retry =

    let mount (context: BuildContext) (current: Node) (se: SutilElement) : SutilResult =
        if _log.enabled then
            _log.trace ("Mount: building", "parent=", context.Parent |> DomHelpers.toStringOutline)

        let context = context.WithLogEnabled()

        se
        |> VirtualDom.fromSutil
        |> context.VirtualElementMapper
        |> logElement context current // SutilElement -> VirtualDom.Element
        |> Patch.calculate current // Calculate patch for new element
        |> logPatch context current // Log the patch (debug)
        |> Patch.apply context current // Apply patch
        |> fun (result) ->
            notifyNewNodes result // Notify new nodes
            result
