
module Dsl 

open Browser.Types

open Core
open CoreTypes

open EventHelpers
open Fable.Core.JsInterop

// This can be replaced by Feliz.Engine very easily

type HtmlEngine() =
    let makeElement (name, children) =
        SutilElement.Element (name, children |> Seq.toArray)

    let makeElementC (cls, name, children) =
        SutilElement.Element (
            name, 
            [|
                SutilElement.Attribute ("class", cls)
                yield! children
            |]
        )
        

    // let makeFragment (children) =
    //     SutilElement.Fragment (children |> Seq.toArray)

    // member __.fragment (children) = 
    //     makeFragment children
    
    member __.div (text : string) = 
        makeElement ("div", [ __.text text ] )
    
    member __.div (children : SutilElement seq) = 
        makeElement ("div", children )
    
    member __.divc (cls : string) (children : SutilElement seq) = 
        makeElementC (cls, "div", children )
    
    member __.button (children) = 
        makeElement ("button", children )
    
    member __.text(value) = 
        SutilElement.Text value

    member __.input (children) =
        makeElement("input", children)

type AttrEngine() =
    member __.value (v: string) = SutilElement.Attribute ("value", v)
    member __.className (v: string) = SutilElement.Attribute ("class", v)
    member __.placeholder (v : string) = SutilElement.Attribute("placeholder", v)

type EventEngine() =

    member __.onClick( handler ) =
        SutilElement.Event( "click", handler )

    member __.onInput( handler : TypedEvent<HTMLInputElement> -> unit)=
        SutilElement.Event( 
            "input", 
            !!handler 
        )

let Html = HtmlEngine()
let Ev = EventEngine()
let Attr = AttrEngine()

do ()