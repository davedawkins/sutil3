
module Patch 

open Browser.Types
open DomHelpers

type VElement = VirtualDom.Element

type NodeAction =
    | SetAttr of string * string
    | RemoveAttr of string * string
    | SetInnerText of string
    | ReplaceNew of VElement

type Action = 
    | NodeAction of (Node * NodeAction)
    | AppendNew of VElement

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

let rec calculatePatch (existing : Node) (ve : VElement) : Action[] =
    if isTextNode existing && ve.IsTextNode then
        [|
            if existing.textContent <> ve.Text then
                NodeAction (existing, SetInnerText ve.Text) 
        |]
    elif isElementNode existing && ve.IsElementNode && ve.Tag = (asElement existing).tagName then
        [|
            yield! diffAttributes (existing.attributes) (ve.Attributes) |> Array.map (fun a -> NodeAction (existing,a))
            // FIXME: Children, events, side-effects??
        |]
    else
        [| 
            if isNull existing then
                AppendNew ve
            else
                NodeAction (existing, ReplaceNew ve)
        |] 

let applyPatch (parent : HTMLElement) (actions : Action[]) =
    let apply results (a : Action) =

        match a with
        | NodeAction (node, na) ->

            match na with
            | SetAttr (name,value) -> 
                (asElement node).setAttribute(name,value)
                node

            | RemoveAttr (name,_) -> 
                (asElement node).removeAttribute(name)
                node

            | SetInnerText text ->
                node.textContent <- text
                node

            | ReplaceNew (ve) ->
                let de = VirtualDom.toDom ve
                DomHelpers.replace parent node de
                de

        | AppendNew ve ->
            let de = VirtualDom.toDom ve
            DomHelpers.append parent de
            de

    actions |> Array.fold apply (null : Node)
