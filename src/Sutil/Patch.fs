
module Sutil.Patch 

// Compare a VirtualDom.Element with a real DOM node, and return
// an Action that will either replace the DOM node or patch it

open Browser.Types
open Sutil.Dom
open Sutil.Dom.DomHelpers
open Sutil.Dom.TypeHelpers

type VElement = VirtualDom.Element

type PatchAction =
    | SetAttr of string * string
    | RemoveAttr of string * string
    | AddEvent of string * (Browser.Types.Event -> unit)
    | RemoveEvent of string * (Browser.Types.Event -> unit)
    | SetInnerText of string
    | ApplyEffect of VElement
    | ChildAction of (int * Action)
    // override __.ToString (): string = 
    //     match __ with
    //     | Patch (actions) ->
    //         sprintf "Patch: [%s]" (actions |> Array.map (_.ToString()) |> String.concat ",")
    //     | _ -> sprintf "%A" __

// What we're going to do to an existing (node, vnode) pair
and Action =
    | AsIs
    | Patch of PatchAction[]
    | ActionApplyEffect of VElement
    | ReplaceNew of VElement
    | AppendNew of VElement
    | Remove
    override __.ToString (): string = 
        match __ with
        | Patch (actions) ->
            sprintf "Patch: [%s]" (actions |> Array.map (_.ToString()) |> String.concat ",")
        | ReplaceNew _ -> "ReplaceNew"
        | AppendNew e -> "AppendNew " + e.ToString()
        | Remove  -> "Remove"
        | ActionApplyEffect _ -> "Effect"
        | AsIs  -> "AsIs"

type Result =
    | Replaced
    | Appended
    | Effected of string
    | Removed
    | Patched of ((Result * Node)[])
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
        if existing.textContent <> ve.InnerText then
            Patch [| SetInnerText ve.InnerText |]
        else 
            AsIs
    elif isElementNode existing && ve.IsElementNode && ve.Tag = (asElement existing).tagName.ToLower() then
        [|
            yield! (diffAttributes (existing.attributes) (ve.Attributes) )
            // TODO: side-effects
    
            // Always add the events, we clear them out first
            yield! (ve.Events |> Array.map AddEvent)

            yield! Helpers.pairOptionals (DomHelpers.children existing) (ve.DomChildren)
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

            yield! (ve.EffectChildren |> Array.map (ApplyEffect))
        |]
        |> (fun patches -> if patches.Length > 0 then Patch patches else AsIs)

    // Could we end up trying to patch a DOM node with a SideEffect/Mapper?
    // ReplaceNew would be OK, since we get the side
    elif ve.IsDomNode then
        if isNull existing then
            AppendNew ve
        else
            ReplaceNew ve
    else
        if not (ve.IsEffectNode) then 
            failwith "Unexpected virtual node"
        ActionApplyEffect ve


let rec applyPatch (context : CoreTypes.BuildContext) (current : Node) (action: Action) : (Result * Node) =

    let nodeChildren = 
        current |> DomHelpers.children |> Seq.toArray

    let nodeChild ix =
        if ix < nodeChildren.Length then nodeChildren[ix] else null

    let apply (a : PatchAction) : (Result * Node) option=
        // Fable.Core.JS.console.log(sprintf "apply: %A" a)
        match a with
        | SetAttr (name,value) -> 
            DomEdit.setAttribute (asElement current) name value
            None

        | RemoveAttr (name,_) -> 
            DomEdit.removeAttribute (asElement current) name
            None

        | AddEvent(name,value) -> 
            EventListeners.add current name value |> ignore
            None

        | RemoveEvent (name,value) -> 
            (asElement current).removeEventListener(name,value)
            None

        | SetInnerText text ->
            current.textContent <- text
            None

        | ChildAction (ix, action) ->
            applyPatch (context.WithParent(current)) (nodeChild ix) action |> Some

        | ApplyEffect ve ->
            match ve.Type with
            | VirtualDom.SideEffectNode (name, effect) ->
                if isNull current then failwith "Cannot apply effect to null node"
                Log.Console.log("Applying effect", name, " to ", context.ParentElement.outerHTML)
                let result = effect (context.WithParent current)
                None
            | _ -> failwith "Not a side-effect"


    match action with
    | AsIs ->
        Unchanged, current

    | Remove ->
        DomEdit.remove current
        Removed, current

    | ReplaceNew (ve) ->
        let de = VirtualDom.toDom context ve
        DomEdit.replace context.ParentElement current de
        Replaced, de

    | AppendNew ve ->
        let de = VirtualDom.toDom context ve
        if not (de.isSameNode(context.ParentElement)) then
            context.Mount context.ParentElement de
        Appended, de

    | ActionApplyEffect ve ->
        Effected (ve.Tag), VirtualDom.toDom context ve

    | Patch patches ->
        if not (isNull current) then 
            Log.Console.log(
                "Patch: Disposing event listeners and side-effects\n", 
                outerHTML current )
            Dispose.disposeNode (current) // Cleanup event listeners and disposables
        let result = patches |> Array.choose apply
        Patched result, current
