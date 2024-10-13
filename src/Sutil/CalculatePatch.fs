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

open VirtualDom

let private _log = Log.create ("Patch")
_log.enabled <- false

// A way to find a DOM node given a parent node
type DomRef =
    | Key of string 
    with
        member __.TryFindNode (parent : Node) : Node option = None

/// Actions we can perform on an existing DOM node to bring it into alignment with a VirtualElement
type PatchAction =
    | SetAttr of string * string
    | RemoveAttr of string * string
    | AddEvent of string * (Browser.Types.Event -> unit) * (Internal.CustomEvents.EventOption[])
    | RemoveEvent of string * (Browser.Types.Event -> unit)
    | SetInnerText of string
    | ApplyEffect of SutilEffect
    | ChildAction of (int * NodeAction)
    // | InsertChild of VirtualElement
    // | RemoveChild of VirtualElement
    override __.ToString() : string =
        match __ with
        | SetAttr(name, value) -> "SetAttr '" + name + "' = '" + value + "'"
        | RemoveAttr(name, value) -> "RemoveAttr '" + name + "' = '" + value + "'"
        | AddEvent(name, _, _) -> "AddEvent '" + name + "'"
        | RemoveEvent(name, _) -> "RemoveEvent " + name + "'"
        // | InsertChild(ve) -> "InsertChild " + ve.Tag + "'"
        // | RemoveChild(ve) -> "RemoveChild " + ve.Tag + "'"
        | SetInnerText(s) -> "SetInnerText '" + s + "'"
        | ApplyEffect(name, _) -> "ApplyEffect '" + name + "'"
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

    let [<Literal>] VE = "__sutil_ve"

    // For now, we are going to say that any given mount point in the DOM has only one Sutil node as a direct
    // child.
    let tryFindRoot (parent : Node) : VirtualElement option =
        parent 
        |> DomHelpers.children
        |> Seq.choose (fun n -> JsMap.tryGetKey n VE)
        |> Seq.tryHead

    let private asArray (n: NamedNodeMap) =
        [|
            for i in 0 .. (n.length - 1) do
                let a = n.item (i)
                a.name, a.value
        |]

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
       ve.GetKey()
        |> Key
        |> _.TryFindNode(parent)

    let tryWithDomNode (parent : Node) (map : Node -> 'r) (ve : VirtualElement) : Result<'r,string> =
        tryGetDomNode parent ve
        |> Option.map (map>>Ok)
        |> Option.defaultValue
            (Error "DOM node does not exist")        

open Helpers

let rec private calculatePatch (existing: VirtualElement) (latest: VirtualElement) : NodeAction =
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
            [| 
                // Add / remove attributes
                yield! (diffAttributes (existing.Attributes) (latest.Attributes))


                let n = System.Math.Min( existing.Children.Length, latest.Children.Length )

                // Compare child pairs
                // TODO: We can calculate these pairs using user keys too
                for i in 0 .. (n-1) do
                    yield ChildAction(-1, calculatePatch (existing.Children[i]) (latest.Children[i]) )

                // Add children
                if existing.Children.Length < latest.Children.Length then
                    yield!
                        latest.Children 
                        |> Seq.take (existing.Children.Length)
                        |> Seq.map (fun ve -> ChildAction(-1, Insert ve))

                // Remove children
                if existing.Children.Length > latest.Children.Length then
                    yield!
                        existing.Children 
                        |> Seq.take (latest.Children.Length)
                        |> Seq.map (fun ve -> ChildAction(-1, Remove ve))

            |] |> (fun actions -> Patch (latest, actions))

    elif existing.IsDomNode then

        if latest.IsDomNode then
            Replace (existing,latest)
        else
            Remove existing

    else
        failwith "Unexpected virtual element"

let rec private calculate (parent: Node) (ve: VirtualElement) : NodeAction =
    match tryFindRoot parent with
    | Some ve0 ->
        calculatePatch ve0 ve
    | None ->
        Insert ve        


let applyPatchAction (context : BuildContext) (patchAction : PatchAction) : Result<PatchResult,string> =
    Error ""

let applyNodeAction (context : BuildContext) (nodeAction : NodeAction) : Result<SutilResult, string> =
    let ok (r, node) = (r, node) |> SutilResult.Of |> Ok

    match nodeAction with

    | AsIs ->
        (Unchanged, (null : Node)) |> ok

    | Remove existing ->
        existing |> tryWithDomNode context.Parent (fun domNode ->
            DomEdit.remove domNode
            (Removed, domNode) |> SutilResult.Of
        ) 

    | Replace (existing, latest) ->
        existing |> tryWithDomNode context.Parent (fun existingNode ->
            let newNode = VirtualDom.toDom context latest
            DomEdit.replace context.Parent existingNode newNode
            (Replaced, newNode) |> SutilResult.Of
        )
 
    | Insert (latest) ->
        let newNode = VirtualDom.toDom context latest
        DomEdit.append context.Parent newNode
        (Appended, newNode) |> ok

    | Patch (latest, patchActions) ->

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

            ((Patched patchResults), context.Parent)
            |> SutilResult.Of
            |> Ok