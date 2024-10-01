
module Patch 

// Compare a VirtualDom.Element with a real DOM node, and return
// an Action that will either replace the DOM node or patch it

open Browser.Types
open DomHelpers

type VElement = VirtualDom.Element

type PatchAction =
    | SetAttr of string * string
    | RemoveAttr of string * string
    | SetInnerText of string
    | ChildAction of (int * Action)

// What we're going to do to an existing (node, vnode) pair
and Action =
    | AsIs
    | Patch of PatchAction[]
    | ReplaceNew of VElement
    | AppendNew of VElement
    | Remove
        
type Result =
    | Replaced
    | Appended
    | Removed
    | Patched
    | Unchanged

let asArray (n : NamedNodeMap) =
    [| 
        for i in 0 .. (n.length-1) do 
            let a = n.item(i)
            a.name, a.value
    |]

let diffAttributes (a : NamedNodeMap) (attrs : (string * string)[]) =
    let a = a |> asArray |> Array.sortBy fst
    let b = attrs |> Array.sortBy fst

    let anames = a |> Array.map fst
    let bnames = b |> Array.map fst
    let allnames = Array.append anames bnames |> Set |> Set.toArray
    let ma = Map a
    let mb = Map b

    [|
        for name in allnames do
            match ma.TryFind name, mb.TryFind name with
            | Some va, Some vb when va <> vb ->
                SetAttr (name, vb)
            | None, Some vb ->
                SetAttr (name, vb)
            | Some x, None ->
                RemoveAttr (name,x)
            | _ -> ()
    |]

let rec calculatePatch (existing : Node) (ve : VElement) : Action =
    //Fable.Core.JS.console.log("calculate patch: ", (if isNull existing then "null" :> obj else existing), ve )

    if isTextNode existing && ve.IsTextNode then
        if existing.textContent <> ve.Text then
            Patch [| SetInnerText ve.Text |]
        else 
            AsIs
    elif isElementNode existing && ve.IsElementNode && ve.Tag = (asElement existing).tagName.ToLower() then
        [|
            yield! (diffAttributes (existing.attributes) (ve.Attributes) )
            // TODO: Events, side-effects

            yield! Helpers.pairOptionals (DomHelpers.children existing) (ve.Children)
            |> Seq.mapi (fun i (x, y) ->
                match (x, y) with   
                | Some x, Some y -> 
                    ChildAction (i,calculatePatch x y)
                | Some x, None -> 
                    ChildAction (i, Remove)
                | None, Some x -> 
                    ChildAction (i, AppendNew x)
                | _ -> failwith "Internal error"
            )
            |> Seq.filter (function ChildAction(_,AsIs) -> false| _ -> true)
        |]
        |> (fun patches -> if patches.Length > 0 then Patch patches else AsIs)

    elif isNull existing then
        AppendNew ve
    else
        ReplaceNew ve

let rec applyPatch (context : CoreTypes.BuildContext) (node : Node) (action: Action) : (Result * Node) =

    let nodeChildren = 
        node |> DomHelpers.children |> Seq.toArray

    let apply (a : PatchAction) =
        match a with
        | SetAttr (name,value) -> 
            (asElement node).setAttribute(name,value)

        | RemoveAttr (name,_) -> 
            (asElement node).removeAttribute(name)

        | SetInnerText text ->
            node.textContent <- text

        | ChildAction (ix, action) ->
            let result = applyPatch (context.WithParent(node)) (nodeChildren[ix]) action
            ()

    match action with
    | AsIs ->
        Unchanged, node

    | Remove ->
        Removed, node

    | ReplaceNew (ve) ->
        let de = VirtualDom.toDom context ve
        DomHelpers.replace context.ParentElement node de
        Replaced, de

    | AppendNew ve ->
        let de = VirtualDom.toDom context ve
        DomHelpers.append context.ParentElement de
        Appended, de

    | Patch patches ->
        patches |> Array.iter apply
        Patched, node
