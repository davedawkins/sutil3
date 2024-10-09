[<RequireQualifiedAccess>]
module Bulma

// An alternative to Bulma.styleHelpers from Sutil 2.0

open Sutil
open Sutil.Html
open type Feliz.length

let block (children : SutilElement seq) = 
    Html.divc "block" children
    
let box (children : SutilElement seq) = 
    Html.divc "box" children

let form (children : SutilElement seq) = 
    Html.form [ Attr.className "block"; yield! children ]

let h1 (children : SutilElement seq) = 
    Html.h1c "title is-1" children

let h2 (children : SutilElement seq) = 
    Html.h2c "title is-2" children

let h3 (children : SutilElement seq) = 
    Html.h3c "title is-3" children

let h4 (children : SutilElement seq) = 
    Html.h4c "title is-4" children

let h5 (children : SutilElement seq) = 
    Html.h5c "title is-5" children

let button (children : SutilElement seq) =
    Html.buttonc "button" children

let inputFile (children : SutilElement seq) =
    Html.inputc "file-cta" [
        yield! children
        Attr.typeFile
    ]

let inputText (children : SutilElement seq) =
    Html.inputc "input" [
        yield! children
        Attr.typeText
    ]

let inputRadio (children : SutilElement seq) =
    Html.inputc "radio" [
        yield! children
        Attr.typeRadio
    ]

let inputCheckbox (children : SutilElement seq) =
    Html.inputc "checkbox" [
        yield! children
        Attr.typeCheckbox
    ]

let inputNumber (children : SutilElement seq) =
    Html.inputc "input is-small" [
        yield! children
        Attr.typeNumber
        Attr.style [ Css.maxWidth (percent 50) ]
    ]

let inputRange (children : SutilElement seq) =
    Html.inputc "input is-small" [
        yield! children
        Attr.typeRange
        Attr.style [ Css.maxWidth (percent 50) ]
    ]



    