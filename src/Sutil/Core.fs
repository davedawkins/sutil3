module Sutil.Core

open Sutil
open Sutil.Internal
open VirtualDom
open Patch

let private _log = Log.create ("Core")

_log.enabled <- true

[<AutoOpen>]
module CoreExtensions =

    type SutilElement with

        static member DefineBinding(name: string, init : SutilElement, f: BuildContext -> unit) =
            SutilElement.BindElement(
                name, 
                init, 
                f
            )

        static member DefineBinding(name: string, init : SutilElement, f: BuildContext -> System.IDisposable) =
            SutilElement.DefineBinding(
                name, 
                init, 
                (fun ctx -> ctx |> f |> Dispose.addDisposable ctx.ParentElement name)
            )

        static member DefineBinding(name: string, f: BuildContext -> System.IDisposable) =
            SutilElement.DefineBinding(
                name,
                Basic.el "div" [],
                f
        )

        static member DefineMapping(name : string, f: BuildContext -> BuildContext, child : SutilElement ) =
            SutilElement.MappingElement(name, f, child)

        static member HiddenDiv(name: string) =
            SutilElement.Element(
                "div",
                [|
                    SutilElement.Attribute("style", "display:none")
                    SutilElement.Attribute("data-name", name)
                |]
            )

let private forceLog (f : unit -> unit) =
    let save = _log.enabled
    _log.enabled <- true
    f()
    _log.enabled <- save

let private logElement (context: BuildContext) (velement: VirtualElement) =
    if context.LogElementEnabled then
        fun () ->
            _log.trace (">> -------------------------------------------------------")
            _log.trace (sprintf "New element: %s" (velement.AsString()))
        |> forceLog

    velement

let private logPatch (context: BuildContext) (patchAction: NodeAction) =
    if context.LogPatchEnabled then
        fun () ->
            _log.trace (
                sprintf
                    "Patch action:\n current= %s\n action=%s\n parent=%s"
                    (Node.outerHTML context.Current)
                    (patchAction.ToString())
                    (context.ParentElement.outerHTML)
            )
        |> forceLog

    patchAction

type SutilEffect =
    static member RegisterDisposable(node: Browser.Types.Node, name: string, disposable: System.IDisposable) =
        Dispose.addDisposable node name disposable

    static member RegisterUnsubscribe(node: Browser.Types.Node, name: string, disposable: Unsubscriber) =
        Dispose.addUnsubscribe node name disposable

let internal notifyMount (node: Browser.Types.Node) =
    if _log.enabled then
        _log.trace ("Mount: notifying", Internal.Node.toStringOutline node)

    CustomEvents.CustomDispatch<_>.dispatch (node, CustomEvents.MOUNT)

let internal notifySutilEvents (node: Browser.Types.Node) =
    // _log.enabled <- true

    if _log.enabled then
        _log.trace ("Mount: New node", node |> Node.toString)

    if Node.isConnected (node) then
        CustomEvents.CustomDispatch<_>.dispatch (node, CustomEvents.CONNECTED)
        notifyMount node

        node
        // The mount handlers for bindings can change the tree, so traverse the tree from the bottom
        // up, and in reverse child order. Edits *shouldn't* change the next pointers the nodes
        // we have yet to visit...
        |> Node.descendantsDepthFirstReverse
        |> Seq.toArray // ... but just to be sure. Mount handlers should probably check that the node is connected (still)
        |> Seq.filter Sutil.Internal.Node.isElementNode
        |> Seq.iter notifyMount

        Sutil.Internal.CustomEvents.notifySutilUpdated (node.ownerDocument)
    else if _log.enabled then
        _log.trace ("Not connected: ", Internal.Node.toStringSummary node)


let tryFindNewNode ( result : SutilResult ) : Browser.Types.Node option =
    match result.Result with
    | Replaced
    | Appended -> Some result.Node
    | Patched(results) -> Some result.Node
    | _ -> None

/// Find new nodes that were created during the
/// patch operation, and welcome them into the DOM
/// so they can run their onMount handlers etc
let notifyNewNodes (result: SutilResult) =
    result |> tryFindNewNode |> Option.iter notifySutilEvents

type BuildOptions =
    {
        BuildVirtualElement : BuildContext -> SutilElement -> VirtualElement
        CalculatePatches : BuildContext -> VirtualElement -> NodeAction
        ApplyPatches : BuildContext -> NodeAction -> SutilResult
    }
    with
        static member Create() =
            {
                BuildVirtualElement = fun _ se -> VirtualDom.fromSutil se
                CalculatePatches = fun ctx ve -> Patch.calculate (ctx.Current) ve
                ApplyPatches = fun ctx action -> 
                    match Patch.apply ctx action with
                    | Ok r -> r
                    | Error s -> 
                        failwith s
            }

        member __.WithBuildVirtualElement ( create : BuildContext -> SutilElement -> VirtualElement ) =
            {  __ with BuildVirtualElement = create }

        member __.WithCalculatePatches ( calc : BuildContext -> VirtualElement -> NodeAction ) =
            {  __ with CalculatePatches = calc }

        member __.WithApplyPatchess ( apply : BuildContext -> NodeAction -> SutilResult ) =
            {  __ with ApplyPatches = apply }

        member __.WithPostBuildVirtualElement ( post : VirtualElement -> VirtualElement ) =
            __.WithBuildVirtualElement( fun ctx se -> __.BuildVirtualElement ctx se |> post )

        member __.WithPostCalculatePatches ( post : NodeAction -> NodeAction ) =
            __.WithCalculatePatches( fun ctx action -> __.CalculatePatches ctx action |> post )

let buildWith (options : BuildOptions) (context : BuildContext) (sutilElement : SutilElement) : SutilResult =
    sutilElement
    |> options.BuildVirtualElement context
    |> options.CalculatePatches context
    |> options.ApplyPatches context

let buildWithLogging (options : BuildOptions) (context: BuildContext) (sutilElement: SutilElement) : SutilResult =
    let options = 
        if _log.enabled then
            _log.trace ("Mount: building", "parent=", context.Parent |> Node.toStringOutline)
            options
                .WithPostBuildVirtualElement( logElement context )
                .WithPostCalculatePatches( logPatch context )
        else
            options

    let context = context.WithLogEnabled()

    buildWith options context sutilElement

module VDomV1 =
    let makeOptions() =
        BuildOptions.Create()

module VDomV2 =
    let makeOptions() =
        BuildOptions
            .Create()
            .WithCalculatePatches( fun ctx ve -> 
#if NO_PATCH
                // Behave like Sutil 2.x
                match isNull (ctx.Current) with 
                | true -> NodeAction.Insert ve
                | false -> NodeAction.Replace ve
#else
                Patch.calculate (ctx.Current) ve
#endif
            )
            .WithApplyPatchess( fun ctx action -> 
                match Patch.apply ctx action with
                | Ok r -> r
                | Error s -> 
                    failwith s)

let makeOptions() =
    VDomV2.makeOptions()

let notify (result : SutilResult) =
    notifyNewNodes result
    result

let mountWith (mapOptions : BuildOptions -> BuildOptions) (context: BuildContext) (sutilElement: SutilElement) : SutilResult =
    sutilElement
    |> buildWithLogging (makeOptions() |> mapOptions) context
    |> notify

let mount (context: BuildContext) (sutilElement: SutilElement) : SutilResult =
    mountWith (id) context sutilElement

/// Helper functions in porting core library from Sutil2.
/// TODO: Refactor these away
module Sutil2 =
    let attr (name, value) = SutilElement.Attribute(name, value)

    let el (tag: string) (children: SutilElement seq) =
        SutilElement.Element(tag, children |> Seq.toArray)

    let text s = SutilElement.Text s

    let listen event (target: Browser.Types.EventTarget) fn = EventListeners.add target event fn

    let once event node fn = EventListeners.once event node fn

    let fragment children =
        SutilElement.Fragment(children |> Seq.toArray)

    module Interop =
        let get data name = JsMap.getKey data name

        let set data name value = JsMap.setKey data name value

    module Logging =
        let error (s: string) = _log.trace ("Error: " + s)

    [<AutoOpen>]
    module Ext =
        type BuildContext with
            member __.ParentNode = __.Parent
            member __.Document = Browser.Dom.document

    let documentOf (node: Browser.Types.Node) = node.ownerDocument
