
module Sutil.Styling

/// <exclude/>
type StyleRule =
    { SelectorSpec: string
      //Selector: CssRules.CssSelector
      Style: (string * obj) list }

/// <exclude/>
type KeyFrame =
    { StartAt: int
      Style: (string * obj) list }

/// <exclude/>
type KeyFrames = { Name: string; Frames: KeyFrame list }

/// <exclude/>
type MediaRule =
    { Condition: string
      Rules: StyleSheetDefinition list }

/// <exclude/>
and StyleSheetDefinition =
    | Rule of StyleRule
    | KeyFrames of KeyFrames
    | MediaRule of MediaRule

/// <exclude/>
type StyleSheetDefinitions = StyleSheetDefinition list

/// <exclude/>
type NamedStyleSheet =
    { Name: string
      StyleSheet: StyleSheetDefinitions }

open Sutil.Dom
open Sutil.Dom.TypeHelpers

let private toLines (s : string) =
    s.Split( [| '\n' |] )

let private fromLines (s : string seq) =
    s |> String.concat "\n"

let private indent (s : string) =
    s |> toLines |> Array.map (fun line -> "  " + line) |> fromLines

type Rule = {
    Selector : string
    Styles: (string * obj)[]
}
with 
    static member Render( selector : string, body : string ) =
        [
            sprintf "%s {" selector
            indent body
            "}"
            ""
        ] |> fromLines

    member __.ToCss() = 
        Rule.Render( 
            __.Selector, 
            (__.Styles |> Array.map (fun (name,value) -> sprintf "%s: %A;" name value) ) |> fromLines
        )

type StyleSheet = Rule seq

let private renderRules (rules : Rule seq) =
    rules |> Seq.map _.ToCss() |> fromLines

let private renderRulesNested (parentSelector : string) (rules : Rule seq) =
    Rule.Render( 
        parentSelector,
        [
            ""
            yield! rules  |> Seq.map (fun r -> "& " + r.ToCss()) 
        ] |> fromLines
    )



/// <summary>
/// Define a CSS keyframe as part of a keyframes sequence
/// See also: <seealso cref="M:Sutil.Styling.keyframes"/>
/// </summary>
let keyframe startAt style =
    {
        StartAt = startAt
        Style = style
    }

/// <summary>
/// Define a CSS keyframes sequence
/// </summary>
/// <example>
/// <code>
///    keyframes "dashdraw" [
///         keyframe 0 [
///             Css.custom("stroke-dashoffset", "10")
///         ]
///     ]
/// </code>
/// </example>
let keyframes name frames =
    KeyFrames {
        Name = name
        Frames = frames
    }

// let internal showEl (el : HTMLElement) isVisible =
//     if isVisible then
//         if Interop.exists el "_display" then
//             addStyleAttr el "display" (Interop.get el "_display")
//         else
//             removeStyleAttr el "display"
//     else
//         addStyleAttr el "display" "none"
//     let ev = Interop.customEvent (if isVisible then Event.Show else Event.Hide) {|  |}
//     el.dispatchEvent(ev) |> ignore
//     ()

let internal makeMediaRule condition rules =
    MediaRule { Condition = condition; Rules = rules }


let rule (selector : string) (styles : (string * obj) seq) =
    {
        Selector = selector 
        Styles = styles |> Seq.toArray
    }

let withStyle (styleSheet : StyleSheet) (el : SutilElement) =

    let addStyle ( context : BuildContext )  =

        let el = Sutil.Core.mount context null el |> asElement
        let elId = Id.getId el

        if elId = "" then 
            failwith "Not a sutil element"

        let className = sprintf "sutil-%s" elId
        let selector = sprintf ".%s" className

        el.classList.add( className )

        let css = styleSheet |> renderRulesNested selector

        StyleDomHelpers.addGlobalStyleSheet( css ) |> ignore

        CreatedNode el

    SutilElement.SideEffect ("withStyle", addStyle)
