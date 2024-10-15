module Sutil.CalculatePatch

// New attempt at calculating and applying patches while preserving existing DOM
// When a VirtualElement is rendered to DOM, the VE is stored on the Node in a key
// named "__sutil_ve"
// When a new VE is ready to be mounted at the root mount position, then:

// - if no DOM child can be found with a VE stored, then this is the first render, so
//   go ahead and create the DOM node, and store the new VE on the DOM

// - if a DOM child is found with a key, then its stored VE is compared with the
//   new VE, and actions are calculated. 
//   The actions are then executed against the DOM child.

// Since we only ever calculate actions against VEs that we've generated, then we
// will only ever add/remove attributes that we are in control of. Third party 
// attributes will left untouched (unless they happen to collide with attributes
// the user is adding/removing).
// The should apply to children too. We are careful to create a mapping from 
// a given VE to its associated DOM node (see VirtualElement.Key), so that when
// we are executing patch actions we can find the DOM node independently of any
// third party nodes that might have been added to the same parent.

open Browser.Types
open Sutil
open Sutil.Internal
open TypeHelpers

open VirtualDom

let private _log = Log.create ("Patch")
_log.enabled <- true

// A way to find a DOM node given a parent node
// type DomRef =
//     | Key of string 
//     with
//         member __.TryFindNode (parent : Node) : Node option = None

/// Actions we can perform on an existing DOM node to bring it into alignment with a VirtualElement
type PatchAction =
    | SetAttr of string * string
    | RemoveAttr of string * string
    | AddEvent of string * (Browser.Types.Event -> unit) * (Internal.CustomEvents.EventOption[])
    | RemoveEvent of string * (Browser.Types.Event -> unit)
    | SetInnerText of string
    | ChildAction of (int * NodeAction)
    override __.ToString() : string =
        match __ with
        | SetAttr(name, value) -> "SetAttr '" + name + "' = '" + value + "'"
        | RemoveAttr(name, value) -> "RemoveAttr '" + name + "' = '" + value + "'"
        | AddEvent(name, _, _) -> "AddEvent '" + name + "'"
        | RemoveEvent(name, _) -> "RemoveEvent " + name + "'"
        | SetInnerText(s) -> "SetInnerText '" + s + "'"
        | ChildAction(i, action) -> "ChildAction #" + (string i) + "[" + action.ToString() + "]"

/// We do exactly one of the following when we calculate what to do with a given VirtualElement and
/// its associated DOM node (if it exists).
///
/// - Do nothing (AsIs)
///   The current DOM node matches the VirtualElement (AsIs)
///
/// - Remove the DOM node (Remove)
///   there is no corresponding VirtualElement (Remove). This happens when we have more DOM nodes
///   than VirtualElements (such as when we're calculating actions for the children of a DOM node)
///
/// - Insert a new DOM node (Insert)
///   there is no corresponding DOM node
///
/// - Replace the DOM node (Replace)
///   the corresponding DOM node cannot be patched to the Virtual Element. For example, one is a Text
///   node and the other is an HTMLElement, or the two HTMLElements have different `tagName`s
///
/// - Patch the DOM node (Patch)
///   apply one or more patches to the DOM node so it matches the VirtualElement. For example, set/remove attributes.
///   A patch contains an array of PatchActions

and NodeAction =
    | AsIs
    | Remove of VirtualElement // Provides key of DOM node to remove
    | Insert of VirtualElement
    | Replace of VirtualElement * VirtualElement
    | Patch of VirtualElement * PatchAction[]

    override __.ToString() : string =
        match __ with
        | Patch (_,actions) -> sprintf "[%s]" (actions |> Array.map (_.ToString()) |> String.concat ",")
        | Replace _ -> "Replace"
        | Insert e -> "Insert " + e.AsString()
        | Remove e -> "Remove"
        | AsIs -> "AsIs"

module private Helpers = 

    // For now, we are going to say that any given mount point in the DOM has only one Sutil node as a direct
    // child.
    let tryFindRoot (parent : Node) : VirtualElement option =
        parent 
        |> DomHelpers.children
        |> Seq.choose (VirtualElement.TryFind)
        |> Seq.tryHead

    let diffAttributes (attrsA : (string * obj)[]) (attrsB: (string * obj)[]) =
        let a = attrsA |> Array.sortBy fst |> Array.map (fun (n,v) -> n, string v)
        let b = attrsB |> Array.sortBy fst |> Array.map (fun (n,v) -> n, string v)

        let anames = a |> Array.map fst
        let bnames = b |> Array.map fst
        let allnames = Array.append anames bnames |> Set |> Set.toArray
        let ma = Map a
        let mb = Map b

        [|
            for name in allnames do
                match ma.TryFind name, mb.TryFind name with
                | Some va, Some vb when va <> vb -> SetAttr(name, vb)
                | None, Some vb -> SetAttr(name, vb)
                | Some x, None -> RemoveAttr(name, x)
                | _ -> ()
        |]

    let tryGetDomNode (parent : Node) (ve : VirtualElement) : Node option =
        DomHelpers.children parent
        |> Seq.tryFind (fun node -> JsMap.getKeyWith node VIRTUAL_ELEMENT_KEY "" = ve.Key)
        // ve.GetKey()
        // |> Key
        // |> _.TryFindNode(parent)

    let tryWithDomNode (parent : Node) (map : Node -> 'r) (ve : VirtualElement) : Result<'r,string> =
        tryGetDomNode parent ve
        |> Option.map (map>>Ok)
        |> Option.defaultWith (fun () ->
            Log.Console.trace("DOM node does not exist")
            (Error "DOM node does not exist: ")        
        )

open Helpers

let private getVirtualChildren (node : Node) : (VirtualElement * Node)[] =
    let nodeChildren = node |> DomHelpers.children |> Seq.toArray

    Fable.Core.JS.console.log("Node: ", nodeChildren )

    nodeChildren
    |> Array.choose (fun (node : Node) -> 
            let ve : VirtualElement option = JsMap.tryGetKey node VIRTUAL_ELEMENT_KEY
            ve |> Option.map (fun ve -> ve, node)
        )

let rec private calculatePatch (node : Node) (existing: VirtualElement) (latest: VirtualElement) : NodeAction =

    // It'a important to ignore the children of existing. Bindings may have changed the children,
    // so we must build the existing=children array by finding children of node that are have virtual
    // elements


    if (existing.IsTextNode && latest.IsTextNode) then

        if (existing.InnerText <> latest.InnerText) then

            // Update text node
            Patch (latest, [| SetInnerText latest.InnerText |])

        else   
            // Nothing to do
            AsIs

    elif (existing.IsElementNode && latest.IsElementNode) then

        if existing.Tag <> latest.Tag then
            // We have to build a new element with the new tag
            // This will remove all 3rd-party attributes, event handlers and children
            // It could be possible to transfer them all over to the element but is that
            // really a requirement?
            Replace (existing,latest)

        else
            let existingChildren = getVirtualChildren node
            [| 
                // Add / remove attributes
                yield! (diffAttributes (existing.Attributes) (latest.Attributes))

                // Always add the events, we clear them out first
                yield! (latest.Events |> Array.map AddEvent)

                let existingN = existingChildren.Length
                let latestN = latest.Children.Length

                let n = System.Math.Min( existingN, latestN )

                // Compare child pairs
                // TODO: We can calculate these pairs using user keys too
                for i in 0 .. (n-1) do
                    let virtualChild, domChild = existingChildren[i]
                    yield ChildAction( i, calculatePatch domChild virtualChild (latest.Children[i]) )

                // Add children
                if existingN < latestN then
                    yield!
                        latest.Children 
                        |> Seq.skip (existingN)
                        |> Seq.mapi (fun i ve -> ChildAction(existingN + i, Insert ve))

                // Remove children
                if existingN > latestN then
                    yield!
                        existingChildren 
                        |> Seq.skip (latestN)
                        |> Seq.mapi (fun i ve -> ChildAction(latestN + i, Remove (fst ve)))
                        |> Seq.rev

            |] |> (fun actions -> Patch (latest, actions))

    elif existing.IsDomNode then

        if latest.IsDomNode then
            Replace (existing,latest)
        else
            Remove existing

    else
        failwith "Unexpected virtual element"

let calculate (node: Node) (ve: VirtualElement) : NodeAction =
    match VirtualElement.TryFind node with
    | Some ve0 when ve0.Key = ve.Key ->
        Log.Console.log("calculate:")
        Log.Console.log(" ve0: ", ve0.AsString())
        Log.Console.log(" ve : ", ve.AsString())
        calculatePatch (node : Node) ve0 ve
    | Some ve0 when ve0.Key <> ve.Key ->
        Fable.Core.JS.console.log("Keys don't match")
        Fable.Core.JS.console.log(node.parentElement |> DomHelpers.children |> Seq.map (DomHelpers.toString) |> String.concat "\n")
        Fable.Core.JS.console.log("Fail: ", node)
        Fable.Core.JS.console.log("Fail: ", ve.AsString())
        failwith ("Virtual element found but keys don't match: " + ve0.Key + " <> " + ve.Key)
    | _ ->
        if isNull node then
            Log.Console.log("calculate: first render: Insert")
            Insert ve
        else
            failwith "Node was expected to have an associated VirtualElement"        

let rec applyPatchAction (context : BuildContext) (patchAction : PatchAction) : Result<PatchResult,string> =
    let current = context.Current

    let veChildren = getVirtualChildren current

    let nodeChild ix =
        if ix >= 0 && ix < veChildren.Length then
            veChildren[ix] |> snd
        else
            null

    match patchAction with
    | SetAttr(name, value) ->
        if (name = "data-binding") then
            JsMap.setKey current "__sutil_ctx" (context.WithParent(current.parentNode))

        if _log.enabled then
            _log.trace ("SetAttr: ", (current |> Internal.DomHelpers.toStringSummary), name, value)

        DomEdit.setAttribute (asElement current) name value
        Ok AttrSet

    | RemoveAttr(name, _) ->
        if _log.enabled then
            _log.trace ("RemoveAttr: ", (current |> Internal.DomHelpers.toStringSummary), name)

        DomEdit.removeAttribute (asElement current) name
        Ok AttrRemoved

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

        Ok EventAdded

    | RemoveEvent(name, value) ->
        if _log.enabled then
            _log.trace ("RemoveEvent: ", (current |> Internal.DomHelpers.toStringSummary), name)

        (asElement current).removeEventListener (name, value)
        Ok EventRemoved

    | SetInnerText text ->
        if _log.enabled then
            _log.trace ("SetInnerText: ", (current |> Internal.DomHelpers.toStringSummary), text)

        current.textContent <- text
        Ok TextSet

    | ChildAction(ix, action) ->
        if _log.enabled then
            _log.trace ("Child: ", ix, nodeChild ix |> Internal.DomHelpers.toStringSummary)

        let childResult =
            applyNodeAction
                (context
                    .WithAppendNode(fun parent node ->
                        DomEdit.insertAfter parent node (nodeChild (ix - 1))
                    )
                    .WithParent(current)
                    .WithCurrent(nodeChild ix)
                )
                action

        match childResult with
        | Ok r -> Ok (ChildResult r)
        | Error s -> Error s

and applyNodeAction (context : BuildContext) (nodeAction : NodeAction) : Result<SutilResult, string> =
    let ok (r, node) = (r, node) |> SutilResult.Of |> Ok

    let current = context.Current

    match nodeAction with

    | AsIs ->
        (Unchanged, (null : Node)) |> ok

    | Remove existing ->
        DomEdit.remove current
        (Removed, current) |> SutilResult.Of |> Ok

    | Replace (existing, latest) ->
        let context = latest.MapContext context
        let newNode = VirtualDom.toDom context latest
        DomEdit.replace current.parentElement current newNode
        (Replaced, newNode) |> SutilResult.Of |> Ok

    | Insert (latest) ->
        let context = latest.MapContext context
        let newNode = VirtualDom.toDom context latest
        DomEdit.append context.Parent newNode
        (Appended, newNode) |> ok

    | Patch (latest, patchActions) ->

        if (isNull current) then
            Error "Node to be patched cannot be found"
        else
            Dispose.disposeNode current // Does NOT delete the node! Unregisters event handlers, disposes associated subscriptions etc

            let context = 
                context
                |> _.WithAppendNode(DomEdit.append) 
                |> latest.MapContext

            let results =
                patchActions 
                |> Array.fold (fun results patchAction -> 
                    match results with
                    | (Error _) :: _ -> results // Short circuit as soon as one error appears
                    | _ -> applyPatchAction context patchAction ::results
                ) []

            match results with
            | (Error s) :: _ -> (Error s)
            | _ -> 
                let patchResults : PatchResult[] = 
                    (results |> List.rev |> Array.ofList |> Array.choose (function Ok r -> Some r | _ -> None))

                JsMap.setKey current VIRTUAL_ELEMENT_KEY latest
                context.NotifyNodeImported current

                ((Patched patchResults), current)
                |> SutilResult.Of
                |> Ok

 

let apply (context: BuildContext) (action: NodeAction) : Result<SutilResult,string> =
    applyNodeAction context action