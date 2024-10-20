[<RequireQualifiedAccess>]
module Bulma

// An alternative to Bulma.styleHelpers from Sutil 2.0

open Sutil

/// Just enough of a DSL to define the Bulma elements. This means we don't need to
/// make a dependency on Sutil.Html and Feliz.Engine
/// 
module SimpleDsl =
    [<RequireQualifiedAccess>]
    module Length =
        let inline percent (n : int) = sprintf "%d%%" n

    [<RequireQualifiedAccess>]
    module Css =
        let maxWidth n : (string * string) = ("max-width", n)

    [<RequireQualifiedAccess>]
    module Attr =
        let className cls = Basic.attr "class" cls
        let typeFile = Basic.attr "type" "file"
        let typeText = Basic.attr "type" "text"
        let typeRadio = Basic.attr "type" "radio"
        let typeRange = Basic.attr "type" "range"
        let typeCheckbox = Basic.attr "type" "checkbox"
        let typeNumber = Basic.attr "type" "number"
        let style (children : (string * string) seq) = 
            Basic.attr "style" (children |> Seq.map (fun (k,v) -> sprintf "%s: %A" k v) |> String.concat ";")
        
    [<RequireQualifiedAccess>]
    module Html =
        let inline elc tag cls children = Basic.el tag [ Attr.className cls; yield! children ]
        let inline divc cls children = elc "div" cls children
        let inline h1c cls children = elc "h1" cls children
        let inline h2c cls children = elc "h2" cls children
        let inline h3c cls children = elc "h3" cls children
        let inline h4c cls children = elc "h4" cls children
        let inline h5c cls children = elc "h5" cls children
        let inline buttonc cls children = elc "button" cls children
        let inline inputc cls children = elc "input" cls children
        let inline form children = Basic.el "form" children

open SimpleDsl

let block (children: SutilElement seq) = Html.divc "block" children

let box (children: SutilElement seq) = Html.divc "box" children

let form (children: SutilElement seq) =
    Html.form [
        Attr.className "block"
        yield! children
    ]

let h1 (children: SutilElement seq) = Html.h1c "title is-1" children

let h2 (children: SutilElement seq) = Html.h2c "title is-2" children

let h3 (children: SutilElement seq) = Html.h3c "title is-3" children

let h4 (children: SutilElement seq) = Html.h4c "title is-4" children

let h5 (children: SutilElement seq) = Html.h5c "title is-5" children

let button (children: SutilElement seq) = Html.buttonc "button" children

let inputFile (children: SutilElement seq) =
    Html.inputc "file-cta" [
        yield! children
        Attr.typeFile
    ]

let inputText (children: SutilElement seq) =
    Html.inputc "input" [
        yield! children
        Attr.typeText
    ]

let inputRadio (children: SutilElement seq) =
    Html.inputc "radio" [
        yield! children
        Attr.typeRadio
    ]

let inputCheckbox (children: SutilElement seq) =
    Html.inputc "checkbox" [
        yield! children
        Attr.typeCheckbox
    ]

let inputNumber (children: SutilElement seq) =
    Html.inputc "input is-small" [
        yield! children
        Attr.typeNumber
        Attr.style [
            Css.maxWidth (Length.percent 50)
        ]
    ]

let inputRange (children: SutilElement seq) =
    Html.inputc "input is-small" [
        yield! children
        Attr.typeRange
        Attr.style [
            Css.maxWidth (Length.percent 50)
        ]
    ]
