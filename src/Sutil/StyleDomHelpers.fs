module Sutil.StyleDomHelpers

open Browser.Types
open Browser.Dom
open Sutil.Internal

let newStyleElement (doc: Document) =
    let head = "head" |> DomHelpers.findElement doc
    let style = doc.createElement ("style")
    head.appendChild (style :> Node) |> ignore
    style

let private addStyleSheet (doc: Document) (css: string) =
    let style = newStyleElement doc
    css |> DomEdit.text |> style.appendChild |> ignore
    (fun () -> style.parentElement.removeChild (style) |> ignore)

let addGlobalStyleSheet (css: string) = addStyleSheet document css
