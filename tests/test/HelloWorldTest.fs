module HelloWorldTest

open Describe

#if HEADLESS
open WebTestRunner
#endif

open Sutil
open Sutil.Dsl

describe "Hello World" <| fun () ->
    it "says hello" <| fun () -> promise {
        Html.div "Hello World" |> mountTestApp
        Expect.queryText "div" "Hello World"
    }

let init() =
    System.Console.WriteLine("HelloWorld init")
