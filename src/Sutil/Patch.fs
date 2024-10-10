module Sutil.Patch

// Compare a VirtualDom.Element with a real DOM node, and return
// an Action that will either replace the DOM node or patch it

open Browser.Types
open Sutil.Internal
open Sutil.Internal.TypeHelpers

type VElement = VirtualElement
open VirtualDom

let private _log = Log.create ("Patch")
_log.enabled <- false

type PatchAction =
    | SetAttr of string * string
    | RemoveAttr of string * string
    | AddEvent of string * (Browser.Types.Event -> unit) * (Internal.CustomEvents.EventOption[])
    | RemoveEvent of string * (Browser.Types.Event -> unit)
    | SetInnerText of string
    | ApplyEffect of VElement
    | ChildAction of (int * Action)

    override __.ToString() : string =
        match __ with
        | SetAttr(name, value) -> "SetAttr '" + name + "' = '" + value + "'"
        | RemoveAttr(name, value) -> "RemoveAttr '" + name + "' = '" + value + "'"
        | AddEvent(name, _, _) -> "AddEvent '" + name + "'"
        | RemoveEvent(name, _) -> "RemoveEvent " + name + "'"
        | SetInnerText(s) -> "SetInnerText '" + s + "'"
        | ApplyEffect(ve) -> "ApplyEffect '" + (ve.AsEffect() |> fst) + "'"
        | ChildAction(i, action) -> "ChildAction #" + (string i) + "[" + action.ToString() + "]"

// What we're going to do to an existing (node, vnode) pair
and Action =
    | AsIs
    | Patch of VElement * PatchAction[]
    | ActionApplyEffect of VElement * SutilSideEffect
    | ReplaceNew of VElement
    | AppendNew of VElement
    | Remove

    override __.ToString() : string =
        match __ with
        | Patch(_, actions) ->
            sprintf "[%s]" (actions |> Array.map (_.ToString()) |> String.concat ",")
        | ReplaceNew _ -> "ReplaceNew"
        | AppendNew e -> "AppendNew " + e.AsString()
        | Remove -> "Remove"
        | ActionApplyEffect _ -> "Effect"
        | AsIs -> "AsIs"

let asArray (n: NamedNodeMap) =
    [|
        for i in 0 .. (n.length - 1) do
            let a = n.item (i)
            a.name, a.value
    |]

let diffAttributes (a: NamedNodeMap) (attrs: (string * obj)[]) =
    let a = a |> asArray |> Array.sortBy fst
    let b = attrs |> Array.sortBy fst

    let anames = a |> Array.map fst
    let bnames = b |> Array.map fst
    let allnames = Array.append anames bnames |> Set |> Set.toArray
    let ma = Map a
    let mb = Map b

    [|
        for name in allnames do
            match ma.TryFind name, mb.TryFind name |> Option.map string with
            | Some va, Some vb when va <> vb -> SetAttr(name, vb)
            | None, Some vb -> SetAttr(name, vb)
            | Some x, None -> RemoveAttr(name, x)
            | _ -> ()
    |]

[<Literal>]
let SUTIL_IMPORTED = "sutil-imported"

let tryGetData (name: string) (el: HTMLElement) : string option =
    if Fable.Core.JsInterop.isIn name el.dataset then
        Some(el.dataset[name])
    else
        None

let getImportedBy (el: HTMLElement) : string option = el |> tryGetData SUTIL_IMPORTED

let rec private calculatePatch (existing: Node) (ve: VElement) : Action =

    if isTextNode existing && ve.IsTextNode then
        if existing.textContent <> ve.InnerText then
            Patch(
                ve,
                [|
                    SetInnerText ve.InnerText
                |]
            )
        else
            AsIs

    elif (existing.asElement |> Option.bind getImportedBy).IsSome then
        if
            (existing.asElement
             |> Option.bind getImportedBy
             |> Option.map (fun importedBy ->
                 match ve.Type with
                 | SideEffectNode(name, _) -> name = importedBy
                 | _ -> false
             )
             |> Option.defaultValue false)
        then
            AsIs
        else
            Remove

    elif
        isElementNode existing
        && ve.IsElementNode
        && ve.Tag = (asElement existing).tagName.ToLower()
    then
        [|
            yield! (diffAttributes (existing.attributes) (ve.Attributes))

            // Always add the events, we clear them out first
            yield! (ve.Events |> Array.map AddEvent)

            let existingChildren = DomHelpers.children existing |> Seq.toArray

            // All children, but with associated DOM index
            let childPatches =
                ve.ChildrenWithDomIndex
                |> Array.map (fun (domIndex, child) ->
                    // match: DOM node exists for virtual DOM node,  virtual node is a DOM node
                    match (domIndex >= 0 && domIndex < existingChildren.Length, domIndex >= 0) with

                    | true, true -> // Compare existing DOM node with VE
                        ChildAction(domIndex, calculate (existingChildren[domIndex]) child)

                    | false, true -> // No existing DOM node, create VE as Dom
                        ChildAction(domIndex, AppendNew child)

                    | true, false -> // Can't happen, since we didn't append existing DOM nodes to this array
                        failwith "Internal error"

                    | false, false -> // Not a DOM child
                        ApplyEffect child
                )

            yield! childPatches

            let n = ve.DomChildren.Length

            if existingChildren.Length > n then
                for i in (existingChildren.Length - 1) .. -1 .. n do
                    yield ChildAction(i, Remove)
        |]
        |> (fun patches ->
            if patches.Length > 0 then
                Patch(ve, patches)
            else
                AsIs
        )

    elif ve.IsDomNode then
        if isNull existing then
            AppendNew ve
        else
            ReplaceNew ve

    elif (ve.IsEffectNode) then
        if isNull existing then
            ActionApplyEffect(ve, ve.AsEffect())
        else
            ReplaceNew ve

    else
        failwith "Unexpected virtual node"

and calculate (existing: Node) (ve: VElement) : Action =
    let patch = calculatePatch existing ve

    if (ve.BuildMappers.Length > 0) then
        let ctx = BuildContext.Create(null) |> ve.MapContext

        if ctx.LogElementEnabled then
            _log.trace (
                "---------------------------------------------------------------------------"
            )

            _log.trace (sprintf "Patch element: %s" (ve.AsString()))

        if ctx.LogPatchEnabled then
            _log.trace (
                sprintf
                    "Patch action:\n current= %s\n action=%s\nparent=%s"
                    (Sutil.Internal.DomHelpers.outerHTML existing)
                    (patch.ToString())
                    (if isNull existing then
                         ""
                     else
                         existing.parentElement |> DomHelpers.toStringSummary)
            )

    patch

let rec private applyPatchAction (context: BuildContext) (a: PatchAction) : PatchResult =
    let current = context.Parent

    let nodeChildren = current |> DomHelpers.children |> Seq.toArray

    let nodeChild ix =
        if ix >= 0 && ix < nodeChildren.Length then
            nodeChildren[ix]
        else
            null

    match a with
    | SetAttr(name, value) ->
        if (name = "data-binding") then
            JsMap.setKey current "__sutil_ctx" (context.WithParent(current.parentNode))

        if _log.enabled then
            _log.trace ("SetAttr: ", (current |> Internal.DomHelpers.toStringSummary), name, value)

        DomEdit.setAttribute (asElement current) name value
        AttrSet

    | RemoveAttr(name, _) ->
        if _log.enabled then
            _log.trace ("RemoveAttr: ", (current |> Internal.DomHelpers.toStringSummary), name)

        DomEdit.removeAttribute (asElement current) name
        AttrRemoved

    | AddEvent(name, value, options) ->
        if _log.enabled then
            _log.trace ("AddEvent: ", (current |> Internal.DomHelpers.toStringSummary), name)

        if
            options
            |> Array.exists (
                function
                | Internal.CustomEvents.Once -> true
            )
        then
            EventListeners.once name current value |> ignore
        else
            EventListeners.add current name value |> ignore

        EventAdded

    | RemoveEvent(name, value) ->
        if _log.enabled then
            _log.trace ("RemoveEvent: ", (current |> Internal.DomHelpers.toStringSummary), name)

        (asElement current).removeEventListener (name, value)
        EventRemoved

    | SetInnerText text ->
        if _log.enabled then
            _log.trace ("SetInnerText: ", (current |> Internal.DomHelpers.toStringSummary), text)

        current.textContent <- text
        TextSet

    | ChildAction(ix, action) ->
        if _log.enabled then
            _log.trace ("Child: ", ix, nodeChild ix |> Internal.DomHelpers.toStringSummary)

        let childResult =
            applyAction
                (context.WithAppendNode(fun parent node ->
                    DomEdit.insertAfter parent node (nodeChild (ix - 1))
                ))
                (nodeChild ix)
                action

        ChildResult childResult

    | ApplyEffect ve ->
        match ve.Type with

        | SideEffectNode(name, effect) ->
            if isNull current then
                failwith "Cannot apply effect to null node"

            if _log.enabled then
                _log.trace (
                    "Apply Effect: ",
                    (current |> Internal.DomHelpers.toStringSummary),
                    name
                )

            EffectResult(effect context)

        | _ -> failwith "Not a side-effect"

and private applyAction (context: BuildContext) (current: Node) (action: Action) : SutilResult =

    match action with
    | AsIs -> (Unchanged, current) |> SutilResult.Of

    | Remove ->
        if _log.enabled then
            _log.trace ("Remove: ", (current |> Internal.DomHelpers.toStringSummary))

        DomEdit.remove current
        (Removed, current) |> SutilResult.Of

    | ReplaceNew(ve) ->
        let de = VirtualDom.toDom context ve

        if _log.enabled then
            _log.trace (
                "Replace: ",
                (current |> Internal.DomHelpers.toStringSummary),
                " with ",
                (de |> DomHelpers.toStringSummary)
            )

        DomEdit.replace context.ParentElement current de
        (Replaced, de) |> SutilResult.Of

    | AppendNew ve ->
        let de = VirtualDom.toDom context ve

        if not (de.isSameNode (context.ParentElement)) then
            _log.trace ("Append: ", (de |> Internal.DomHelpers.toStringSummary))
            context.AppendNode context.ParentElement de

        (Appended, de) |> SutilResult.Of

    | ActionApplyEffect(ve, (name, effect)) ->
        _log.trace ("Apply: ", (name))
        (effect (ve.MapContext context))

    | Patch(ve, patches) ->
        if _log.enabled then
            _log.trace ("Patch: ", (current |> Internal.DomHelpers.toStringSummary))

        if not (isNull current) then
            Dispose.disposeNode (current) // Cleanup event listeners and disposables
        else if _log.enabled then
            _log.trace ("Patch: current is null")

        let result =
            patches
            |> Array.map (
                applyPatchAction (
                    context.WithParent(current).WithAppendNode(DomEdit.append) |> ve.MapContext
                )
            )

        (Patched result, current) |> SutilResult.Of

let apply (context: BuildContext) (current: Node) (action: Action) : SutilResult =
    applyAction context current action
