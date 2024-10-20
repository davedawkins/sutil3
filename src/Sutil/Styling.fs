module Sutil.Styling

open Sutil.Internal

[<Literal>]
let private MODULE_NAME = "Styling"

let private log = Log.create (MODULE_NAME)

module Types =
    type SutilCssSelector = string

    /// Attribute name with value. Eg. ("background-color", "white")
    type SutilStyleAttribute = string * obj

    /// <exclude/>
    /// Selector with associated style attributes
    type SutilStyleRule =
        {
            Selector: string
            Styles: SutilStyleAttribute[]
        }

    /// <exclude/>
    /// Keyframe entry. StartAt is a percentage value, ranging from 0-100
    type SutilStyleKeyFrame =
        {
            StartAt: int
            Styles: SutilStyleAttribute list
        }

    /// <exclude/>
    type SutilStyleKeyFrames =
        {
            Name: string
            Frames: SutilStyleKeyFrame list
        }

    /// <exclude/>
    type SutilStyleMediaRule =
        {
            Condition: string
            Rules: SutilStyleSheetDefinition list
        }

    /// <exclude/>
    and SutilStyleSheetDefinition =
        | Rule of SutilStyleRule
        | KeyFrames of SutilStyleKeyFrames
        | MediaRule of SutilStyleMediaRule

    type SutilStyleSheet =
        {
            Definitions: SutilStyleSheetDefinition[]
        }

        static member Of(rules: SutilStyleRule seq) =
            {
                Definitions = rules |> Seq.map Rule |> Seq.toArray
            }

open Types

module internal Renderer =

    open System
    open Browser.Types

    let private toLines (s: string) =
        s.Split(
            [|
                '\n'
            |]
        )

    let private fromLines (s: string seq) = s |> String.concat "\n"

    let private indent (s: string) =
        s |> toLines |> Array.map (fun line -> "  " + line) |> fromLines

    let parseStyleAttr (style: string) =
        style.Split(
            [|
                ';'
            |],
            StringSplitOptions.RemoveEmptyEntries
        )
        |> Array.collect (fun entry ->
            entry.Split(
                [|
                    ':'
                |],
                2
            )
            |> Array.chunkBySize 2
            |> Array.map (fun pair -> pair.[0].Trim(), pair.[1].Trim())
        )

    let emitStyleAttr (keyValues: (string * string) array) =
        keyValues |> Array.map (fun (k, v) -> sprintf "%s:%s;" k v) |> String.concat ""

    let filterStyleAttr name style =
        parseStyleAttr style |> Array.filter (fun (k, v) -> k <> name) |> emitStyleAttr

    // let getStyleAttr (el : HTMLElement) =
    //     match el.getAttribute("style") with
    //     | null -> ""
    //     | s -> s

    // let addStyleAttr (el : HTMLElement) name value =
    //     let style = getStyleAttr el |> filterStyleAttr name
    //     el.setAttribute( "style", sprintf "%s%s:%s;" style name value )

    // let removeStyleAttr (el : HTMLElement) name =
    //     if log.enabled then log.trace( sprintf "filter by %s: %A -> %A" name (getStyleAttr el) (getStyleAttr el |> filterStyleAttr name) )
    //     el.setAttribute( "style", getStyleAttr el |> filterStyleAttr name )

    // let newStyleElement (doc : Document)=
    //     let head = "head" |> Sutil.Internal.Node.findElement doc
    //     let style = doc.createElement("style")
    //     head.appendChild(style :> Node) |> ignore
    //     style

    let splitMapJoin (delim: char) (f: string -> string) (s: string) =
        s.Split(
            [|
                delim
            |],
            StringSplitOptions.RemoveEmptyEntries
        )
        |> Array.map f
        |> fun values -> String.Join(string delim, values)

    let mapPseudo (f: string -> string) (s: string) =
        let i = s.IndexOf(':')

        if i < 0 then
            f s
        else
            f (s.Substring(0, i)) + (s.Substring(i))

    let isPseudo s =
        s = "hover"
        || s = "active"
        || s = "visited"
        || s = "link"
        || s = "before"
        || s = "after"
        || s = "checked"
        || s = "marker"

    let isGlobal s = s = "body" || s = "html"

    let specifySelector (styleName: string) (selectors: string) =
        if (styleName = "") then
            selectors
        else
            let trans s =
                if isPseudo s || isGlobal s then
                    s
                else
                    sprintf "%s.%s" s styleName // button -> button.styleA

            splitMapJoin ',' (splitMapJoin ' ' (mapPseudo trans)) selectors

    let private styleListToText (css: list<SutilStyleAttribute>) =
        " {\n"
        + String.Join("\n", css |> Seq.map (fun (nm, v) -> $"    {nm}: {v};"))
        + " }\n"

    let private frameToText (f: SutilStyleKeyFrame) =
        sprintf "%d%% %s" f.StartAt (styleListToText f.Styles)

    let private framesToText (frames: SutilStyleKeyFrames) =
        sprintf
            "@keyframes %s {\n%s\n}\n"
            frames.Name
            (String.Join("\n", frames.Frames |> List.map frameToText))

    let private isSutilRule (nm: string, v) = nm.StartsWith("sutil")

    let private ruleToText
        (classMap: Map<string, SutilStyleRule>)
        (styleName: string)
        (rule: SutilStyleRule)
        =
        //rule.SelectorSpec + (styleListToText rule.Style)

        let rec styleText (r: SutilStyleRule) =
            r.Styles
            |> Seq.filter (not << isSutilRule)
            |> Seq.map (fun (nm, v) ->
                if (nm.EndsWith("()")) then
                    match classMap.TryFind nm[0 .. -3] with
                    | Some subrule -> styleText subrule
                    | _ ->
                        Fable.Core.JS.console.warn (
                            "No class found for substitution: ",
                            nm[0 .. -3]
                        )

                        ""
                else
                    $"    {nm}: {v};"
            )
            |> String.concat "\n"

        [
            specifySelector styleName rule.Selector
            " {\n"
            styleText rule
            "}\n"
        ]
        |> String.concat ""

    let rec mediaRuleToText classMap styleName rule =
        sprintf
            "@media %s {\n%s\n}\n"
            (rule.Condition)
            (rule.Rules |> List.map (entryToText classMap styleName) |> String.concat "\n")

    and entryToText classMap (styleName: string) =
        function
        | Rule rule -> ruleToText classMap styleName rule
        | KeyFrames frames -> framesToText frames
        | MediaRule rule -> mediaRuleToText classMap styleName rule

    let private isClassChar c =
        System.Char.IsLetterOrDigit(c) || c = '-' || c = '_'

    let private isClassName (s: string) =
        s.ToCharArray() |> Array.forall isClassChar

    let private isClassOnly (s: string) =
        s.Length >= 2 && s[0] = '.' && isClassName (s.Substring(1))

    let getClassMap (definitions: SutilStyleSheetDefinition seq) =
        definitions
        |> Seq.choose (fun d ->
            match d with
            | Rule r when isClassOnly r.Selector -> Some(r.Selector.Substring(1), r)
            | _ -> None
        )
        |> Map

    let internal styleSheetWithScopeAsText
        (scopeName: string)
        (styleSheet: SutilStyleSheet)
        : string
        =

        let classMap = getClassMap (styleSheet.Definitions)

        styleSheet.Definitions
        |> Array.map (entryToText classMap scopeName)
        |> String.concat "\n"

    let internal styleSheetAsText (styleSheet: SutilStyleSheet) =
        styleSheetWithScopeAsText "" styleSheet

let makeMediaRule condition rules =
    MediaRule
        {
            Condition = condition
            Rules = rules
        }

let includeRule (name: string) = (name + "()"), ("" :> obj)

let rule (selector: SutilCssSelector) (styles: SutilStyleAttribute seq) : SutilStyleRule =
    {
        Selector = selector
        Styles = styles |> Seq.toArray
    }

/// <summary>
/// Define a CSS keyframe as part of a keyframes sequence
/// See also: <seealso cref="M:Sutil.Styling.keyframes"/>
/// </summary>
let keyframe (startAt: int) (styles: SutilStyleAttribute seq) =
    {
        StartAt = startAt
        Styles = styles |> Seq.toList
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
    KeyFrames
        {
            Name = name
            Frames = frames
        }

[<Literal>]
let private WITH_STYLE = "withStyle"

[<Literal>]
let private SUTIL_SCOPE = "sutil-scope"

let private addScopeForNode (scopeName: string) (node: Browser.Types.Node) : unit =

    let rec run (scopeName: string) (node: Browser.Types.Node) : unit =
        node.asElement
        |> Option.iter (fun el ->

            let isScoped =
                el.classList
                |> ClassHelpers.toSeq
                |> Seq.exists (fun name -> (name.StartsWith SUTIL_SCOPE))

            if not isScoped then
                // Log.Console.log("run: Adding scope '" + scopeName + "' to " + (Node.toStringSummary(node)) )
                el.classList
                |> ClassHelpers.toArray
                |> (Array.append (Array.singleton scopeName))
                |> ClassHelpers.setClassList el

                el |> Node.children |> Seq.iter (run scopeName)
            // else
            //     Log.Console.log("run: ALREADY scoped: " + (Node.toStringSummary(node)) )

        )

    run scopeName node

open Core

let withStyle (rules: SutilStyleRule seq) (sutilElement: SutilElement) =
    let buildScope() =
        let scopeName = sprintf "%s-%d" SUTIL_SCOPE (Globals.NextId())
        // Log.Console.log("Building scope " + scopeName)

        rules
        |> SutilStyleSheet.Of
        |> Renderer.styleSheetWithScopeAsText scopeName
        |> StyleDomHelpers.addGlobalStyleSheet
        |> ignore

        scopeName

    SutilElement.DefineMapping(
        WITH_STYLE,

        (fun context ->
                context
                    .WithOnImportedNode(addScopeForNode (buildScope()))
        ),

        sutilElement 
    )