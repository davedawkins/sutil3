module Sutil.StyleDomHelpers

open Sutil.Internal

let newStyleElement (doc: Browser.Types.Document) =
    let head = "head" |> Node.findElement doc
    let style = doc.createElement ("style")
    head.appendChild (style) |> ignore
    style

let private addStyleSheet (doc: Browser.Types.Document) (css: string) =
    let style = newStyleElement doc
    css |> DomEdit.text |> style.appendChild |> ignore
    (fun () -> style.parentElement.removeChild (style) |> ignore)

let addGlobalStyleSheet (css: string) = addStyleSheet Browser.Dom.document css
