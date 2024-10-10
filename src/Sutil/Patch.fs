module Sutil.Patch

// Compare a VirtualDom.Element with a real DOM node, and return
// an Action that will either replace the DOM node or patch it

open Browser.Types
open Sutil.Internal
open Sutil.Internal.TypeHelpers
open Sutil.CalculatePatch

open VirtualDom

let private _log = Log.create ("Patch")
_log.enabled <- false

module private Helpers =
    let private asArray (n: NamedNodeMap) =
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

    let private tryGetData (name: string) (el: HTMLElement) : string option =
        if Fable.Core.JsInterop.isIn name el.dataset then
            Some(el.dataset[name])
        else
            None

    let getImportedBy (el: HTMLElement) : string option = el |> tryGetData SUTIL_IMPORTED

open Helpers

let rec private calculatePatch (existing: Node) (ve: VirtualElement) : NodeAction =

    if isTextNode existing && ve.IsTextNode then

        // Both are text nodes. If they have different text we can just update the inner text, otherwise nothing to do.

        if existing.textContent <> ve.InnerText then
            Patch(
                [|
                    SetInnerText ve.InnerText
                |]
            )
        else
            AsIs

    elif (existing.asElement |> Option.bind getImportedBy).IsSome then

        // The DOM node was imported by an effect. If the virtual element name matches
        // the 'imported-by' attribute then we don't need to do anything. Otherwise, remove the DOM node.

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
            Remove ve

    elif
        isElementNode existing
        && ve.IsElementNode
        && ve.Tag = (asElement existing).tagName.ToLower()

    then

        // Both are elements with matching tags. Generate the patches needed to
        // edit DOM node so that it matches the virtual element, including children.

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
                        ChildAction(domIndex, Insert child)

                    | true, false -> // Can't happen, since we didn't append existing DOM nodes to this array
                        failwith "Internal error"

                    | false, false -> // Not a DOM child
                        PatchAction.ApplyEffect(child.AsEffect())
                )

            yield! childPatches

            let n = ve.DomChildren.Length

            if existingChildren.Length > n then
                for i in (existingChildren.Length - 1) .. -1 .. n do
                    yield ChildAction(i, Remove (VirtualElement.Empty))
        |]
        |> (fun patches ->
            if patches.Length > 0 then
                Patch patches
            else
                AsIs
        )

    elif ve.IsDomNode then

        // The virtual element corresponds to a real DOM node type (ie Text or HTMLElement), so if there is no
        // current DOM node we must Insert, otherwise Replace. We know we must replace because the checks above
        // have already decided we can't patch the existing node.

        if isNull existing then
            // Instantiate the virtual element and insert it at the current location
            Insert ve
        else
            // Instantiate the virtual element and replace the existing DOM node
            Replace (VirtualElement.Empty, ve)

    elif (ve.IsEffectNode) then

        // We have a virtual element which is a side-effect, so we need to apply the effect.
        // If there is a corresponding DOM node then we need to remove it, since it doesn't match
        // this virtual element (the imported-by check further up for a case where an effect can
        // match a DOM node).
        // We overload the Replace action in this instance - it will remove the existing node
        // and apply the virtual element.
        if isNull existing then
            Patch(
                [|
                    ApplyEffect(ve.AsEffect())
                |]
            )
        else
            Replace (VirtualElement.Empty,ve)

    else
        failwith "Unexpected virtual node"

/// Calculate a NodeAction for a given Node and VirtualElement. The Node may be null.
and calculate (existing: Node) (ve: VirtualElement) : NodeAction = calculatePatch existing ve

/// Apply a patch to context.Parent
/// The return value allows us to discover newly-created nodes
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
            applyNodeAction
                (context.WithAppendNode(fun parent node ->
                    DomEdit.insertAfter parent node (nodeChild (ix - 1))
                ))
                (nodeChild ix)
                action

        ChildResult childResult

    | PatchAction.ApplyEffect(name, effect) ->
        if isNull current then
            failwith "Cannot apply effect to null node"

        if _log.enabled then
            _log.trace ("Apply Effect: ", (current |> Internal.DomHelpers.toStringSummary), name)

        EffectResult(effect context)

and private applyNodeAction
    (context: BuildContext)
    (current: Node)
    (action: NodeAction)
    : SutilResult
    =

    match action with
    | AsIs -> (Unchanged, current) |> SutilResult.Of

    | Remove _ ->
        if _log.enabled then
            _log.trace ("Remove: ", (current |> Internal.DomHelpers.toStringSummary))

        DomEdit.remove current
        (Removed, current) |> SutilResult.Of

    | Replace(_,ve) ->
        if ve.IsEffectNode then
            DomEdit.remove current
            let (_, effect) = ve.AsEffect()
            effect context
        //            (Replaced, de) |> SutilResult.Of
        else
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

    | Insert ve ->
        let de = VirtualDom.toDom context ve

        if not (de.isSameNode (context.ParentElement)) then
            _log.trace ("Append: ", (de |> Internal.DomHelpers.toStringSummary))
            context.AppendNode context.ParentElement de

        (Appended, de) |> SutilResult.Of

    | Patch patches ->
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
                    context.WithParent(current).WithAppendNode(DomEdit.append) //|> ve.MapContext
                )
            )

        (Patched result, current) |> SutilResult.Of

let apply (context: BuildContext) (current: Node) (action: NodeAction) : SutilResult =
    applyNodeAction context current action
