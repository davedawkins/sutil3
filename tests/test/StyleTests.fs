module StyleTest

open Describe

#if HEADLESS
open WebTestRunner
#endif

open Sutil
open Sutil.Internal
open Sutil.Html
open Sutil.Bind
open Sutil.CoreElements
open Sutil.Styling
open Browser.CssExtensions
open Browser.DomExtensions

let log s = Fable.Core.JS.console.log s

open Fable.Core.JsInterop


describe "Style" <| fun () ->

    // Simplest case
    it "withStyle colors text" <| fun () -> promise {
        let colorRed = [ rule ".red" [ Css.color "red" ] ]
        let mutable node : Browser.Types.HTMLElement = Unchecked.defaultof<_>
        let app =
            Html.divc "red" [ 
                CoreElements.onElementMounted (fun el -> node <- el)
                text "Hello World" 
            ] |> withStyle colorRed

        mountTestApp app

        Expect.queryText "div" "Hello World"

        let style = Browser.Dom.window.getComputedStyle(node)
        Expect.areEqual( style.color, "rgb(255, 0, 0)", "Text is red" )
    }


let init() =
    ()
