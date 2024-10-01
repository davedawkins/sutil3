
module Style 

open DomHelpers
open Browser.Types

let private toLines (s : string) =
    s.Split( [| '\n' |] )

let private fromLines (s : string seq) =
    s |> String.concat "\n"

let private indent (s : string) =
    s |> toLines |> Array.map (fun line -> "  " + line) |> fromLines

type Rule = {
    Selector : string
    Styles: (string * string)[]
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
            (__.Styles |> Array.map (fun (name,value) -> sprintf "%s: %s;" name value) ) |> fromLines
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

let rule (selector : string) (styles : (string * string) seq) =
    {
        Selector = selector 
        Styles = styles |> Seq.toArray
    }

let withStyle (styleSheet : StyleSheet) (el : CoreTypes.SutilElement) =

    let addStyle ( node : Node ) : Node =
        let el = asElement node
        let elId = SutilKeys.getId node
        if elId = "" then 
            Fable.Core.JS.console.log(node)
            failwith "Not a sutil element"

        let className = sprintf "sutil-%s" elId
        let selector = sprintf ".%s" className

        el.classList.add( className )

        let css = styleSheet |> renderRulesNested selector

        StyleDomHelpers.addGlobalStyleSheet( css ) |> ignore

        el

    CoreTypes.SutilElement.MapElement (addStyle, el)
