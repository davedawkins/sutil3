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

let [<Literal>] RED = "rgb(255, 0, 0)"
let [<Literal>] BLACK = "rgb(0, 0, 0)"

describe "Style" <| fun () ->

    // Simplest case
    it "withStyle colors text" <| fun () -> promise {
        let colorRed = [ rule ".red" [ Css.color "red" ] ]

        let app =
            Html.divc "red" [ 
                text "Hello World" 
            ] |> withStyle colorRed

        mountTestApp app

        Expect.queryText "div" "Hello World"

        Expect.queryStyle "div" "Text is red" (fun style -> style.color = RED) 
    }

    it "withStyle colors text after binding" <| fun () -> promise {
        let colorRed = [ rule ".red" [ Css.color "red" ] ]
        let store = Store.make 0

        let app =
            Html.div [
                text "Container"
                Bind.el (store, fun n ->
                    Html.divc "red" [ 
                        text (string n) 
                    ] |> withStyle colorRed )
            ]

        mountTestApp app

        let DIV1 = "div"
        let DIV2 = "div>div"

        Expect.queryText DIV2 "0"

        Expect.queryStyle DIV1 "Text is black" (fun style -> style.color = BLACK) 
        Expect.queryStyle DIV2 "Text is red" (fun style -> style.color = RED) 

        store |> Store.modify ((+)1)

        Expect.queryText DIV2 "1"

        Expect.queryStyle DIV1 "Text is black" (fun style -> style.color = BLACK) 
        Expect.queryStyle DIV2 "Text is red" (fun style -> style.color = RED) 
    }


let init() =
    ()
