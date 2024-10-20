module BindingTest

open Describe

#if HEADLESS
open WebTestRunner
#endif

open Sutil
open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind

type Record = {
    Id : int
    Value : string
}

let viewItem (r : Record) =
    Html.div [
        text r.Value
    ]

let viewItemO (r : System.IObservable<Record>) =
    Bind.el( r, viewItem )

describe "Sutil.Binding" <| fun () ->

    it "Binding has minimal required elements" <| fun () -> promise {
        let data = Store.make 0
        let app =

            // We expect this to be nothing more complicated than this in terms of Node structure
            // <div><div>0</div></div>
            //
            Html.div [
                Bind.el( data, fun n -> 
                    Html.div [
                        text (string n)
                    ] )
            ]

        mountTestApp app

        Expect.querySingleTextChild"div>div" "0"
        data |> Store.modify ((+)1)
        Expect.querySingleTextChild"div>div" "1"

        return ()
    }

    it "Binding allows change in element tag" <| fun () -> promise {
        let data = Store.make 0
        let app =

            // We expect this to be nothing more complicated than this in terms of Node structure
            //    <div><div>0</div></div>
            // or when n > 0:
            //    <div><span>1</span></div>
            Html.div [
                Bind.el( data, fun n -> 
                    match n with
                    | 0 -> Html.div [ text (string n) ] 
                    | _ -> Html.span [ text (string n) ] 
                )
            ]

        mountTestApp app

        Expect.querySingleTextChild"div>div" "0"
        data |> Store.modify ((+)1)
        Expect.querySingleTextChild"div>span" "1"

        return ()
    }



    // it "Doesn't dispose internal state for observable view function" <| fun () -> promise {
    //     let items = Store.make [|
    //             { Id = 0; Value = "Apples" }
    //             { Id = 1; Value = "Oranges" }
    //             { Id = 2; Value = "Pears" }
    //         |]

    //     let app =
    //         BindArray.each( items, viewItemO, (fun x -> x.Id))

    //     mountTestApp app

    //     Expect.queryTextContains "div:nth-child(1)" "Apples"
    //     // Expect.queryTextContains "div:nth-child(2)" "Oranges"
    //     // Expect.queryTextContains "div:nth-child(3)" "Pears"

    //     // items.Update( fun _ -> [|
    //     //         { Id = 0; Value = "Bananas" }
    //     //         { Id = 1; Value = "Oranges" }
    //     //         { Id = 2; Value = "Pears" }
    //     //     |]
    //     // )

    //     // Expect.queryTextContains "div:nth-child(1)" "Bananas"
    //     // Expect.queryTextContains "div:nth-child(2)" "Oranges"
    //     // Expect.queryTextContains "div:nth-child(3)" "Pears"


    //     // items.Update( fun _ -> [|
    //     //         { Id = 0; Value = "Pineapples" }
    //     //         { Id = 1; Value = "Oranges" }
    //     //         { Id = 2; Value = "Pears" }
    //     //     |]
    //     // )

    //     // Expect.queryTextContains "div:nth-child(1)" "Pineapples"
    //     // Expect.queryTextContains "div:nth-child(2)" "Oranges"
    //     // Expect.queryTextContains "div:nth-child(3)" "Pears"

    //     return ()
    // }

    it "Shows exception if binding fails" <| fun () -> promise {
        let data = Store.make 0
        let app =
            Bind.el( data, fun n -> failwith "expected-exception")
        mountTestApp app
        Expect.queryTextContains "div" "expected-exception"
        return ()
    }

    // This is going to fail until we implement keyed elements that the Patcher 
    // can re-order
    it "Doesn't dispose items when re-ordering" <| fun () -> promise {
        let sort = Store.make false

        let items = [ 3; 4; 2; 1 ]

        let sortedList useSort =
            items |> List.sortBy (fun n -> if useSort then n else 0)

        let mutable unmountCount = 0
        let app =
            Html.div [
                Bind.each(
                    (sort |> Store.map sortedList),
                    (fun (n : int) ->
                        Html.div [
                            unsubscribeOnUnmount [fun _ -> unmountCount <- unmountCount + 1]
                            text (string n)
                        ]),
                    (fun n -> n)
                )
            ]

        mountTestApp app

        Expect.queryText "div>div:nth-child(1)" "3"
        Expect.queryText "div>div:nth-child(2)" "4"
        Expect.queryText "div>div:nth-child(3)" "2"
        Expect.queryText "div>div:nth-child(4)" "1"

        Expect.areEqual(unmountCount, 0)

        true |> Store.set sort

        Expect.queryText "div>div:nth-child(1)" "1"
        Expect.queryText "div>div:nth-child(2)" "2"
        Expect.queryText "div>div:nth-child(3)" "3"
        Expect.queryText "div>div:nth-child(4)" "4"

        Expect.areEqual(unmountCount, 0)

        return ()
    }

    it "Bind counter" <| fun () -> promise {
        let store = Store.make 0
        let app =
            Html.div [
                Bind.el(store, Html.div)
            ]

        mountTestApp app

        Expect.queryNumChildren "div" 1
        Expect.queryText "div>div" "0"

        store |> Store.modify ((+)1)

        Expect.queryNumChildren "div" 1
        Expect.queryText "div>div" "1"
    }

    it "Bind dispose div" <| fun () ->promise {
        let store = Store.make 0
        let mutable disposed = 0
        let mutable mounted = 0
        let app =
            Html.divc "A" [
                Bind.el(store, fun n ->
                    Html.divc "B" [
                        Ev.onMount (fun _ -> mounted <- mounted + 1)
                        unsubscribeOnUnmount [ (fun _ -> disposed <- disposed + 1) ]
                        text (n.ToString())
                    ]
                )
            ]

        mountTestApp app

        Expect.areEqual( mounted - disposed, 1, "#1: mounted and disposed count differ by 1")
        Expect.areEqual( mounted, 1, "#1: mounted only once")

        Expect.queryText "div>div" "0"
        Expect.assertTrue (disposed = 0) "Not yet disposed"
        Log.Console.log("MODIFY STORE")

        store |> Store.modify ((+)1)

        Expect.areEqual(disposed, 1, "Element was disposed")
        Expect.areEqual( mounted - disposed, 1, "#2: mounted and disposed count differ by 1")
        Expect.areEqual( mounted, 2, "#2: mounted only once")
        Expect.queryText "div>div" "1"
    }


    it "Bind disposal nestx2" <| fun () ->promise {
        let storeInner : IStore<int> = Store.make 0
        let storeOuter = Store.make 0
        let mutable disposed = 0
        let mutable mounted = 0

        let app =
            Html.div [
                Bind.el(storeOuter, fun n1 ->
                    Html.div [
                        Bind.el(storeInner, fun n2 ->
                            Html.div [
                                Ev.onMount (fun _ -> 
                                    mounted <- mounted + 1)
                                unsubscribeOnUnmount [ (fun _ ->  
                                    disposed <- disposed + 1) ]
                                text (n2.ToString())
                            ]
                        )
                    ])
            ]

        mountTestApp app
        Expect.areEqual( mounted - disposed, 1, "#1: mounted and disposed count differ by 1")
        Expect.areEqual( mounted, 1, "#1: mounted only once")

        Expect.areEqual(Store.countSubscribers storeInner,1, "1 inner subscriber before store update")
        Expect.areEqual(Store.countSubscribers storeOuter,1, "1 outer subscriber before store update")

        storeOuter |> Store.modify ((+)1)

        Expect.areEqual(Store.countSubscribers storeInner,1, "1 inner subscriber after store update")
        Expect.areEqual(Store.countSubscribers storeOuter,1, "1 outer subscriber after store update")

        Expect.areEqual( mounted - disposed, 1, "#2: mounted and disposed count differ by 1")

        Expect.areEqual( disposed,1, "inner was disposed exactly once")
    }

    it "Bind disposal nestx3" <| fun () ->promise {
        let storeInner = Store.make 0
        let storeOuter = Store.make 0
        let storeOuter2 = Store.make 0
        let mutable disposed = 0
        let mutable numRenders = 0

        let reset() =
            numRenders <- 0

        let render() =
            numRenders <- numRenders + 1

        let app() =
            Html.div [
                do render()

                Bind.el(storeOuter2, fun n1 ->
                    do render()
                    Html.div [
                        Bind.el(storeOuter, fun n2 ->
                            do render()
                            Html.div [
                                Bind.el(storeInner, fun n3 ->
                                    do render()
                                    Html.div [
                                        unsubscribeOnUnmount [ (fun _ -> disposed <- disposed + 1) ]
                                        text (sprintf "%d %d %d" n1 n2 n3)
                                    ]
                                )
                            ])
                    ])
            ]

        reset()
        mountTestApp (app())

        Expect.areEqual(storeInner |> Store.countSubscribers,1,"1 inner sub")
        Expect.areEqual(storeOuter |> Store.countSubscribers,1,"1 outer sub")
        Expect.areEqual(disposed,0,"disposed")
        Expect.areEqual(numRenders,4,"numRenders #1")

        reset()
        storeOuter2 |> Store.modify ((+)1)
        Expect.areEqual(disposed,1,"disposed")

        Expect.areEqual(storeInner |> Store.countSubscribers,1,"1 inner sub after modify #1")
        Expect.areEqual(storeOuter |> Store.countSubscribers,1,"1 outer sub after modify #1")
        Expect.areEqual(disposed,1,"disposed")
        Expect.areEqual(numRenders,3,"numRenders #2")

        reset()
        storeOuter2 |> Store.modify ((+)1)

        Expect.areEqual(storeInner |> Store.countSubscribers,1,"1 inner sub after modify #2")
        Expect.areEqual(storeOuter |> Store.countSubscribers,1,"1 outer sub after modify #2")
        Expect.areEqual(disposed,2,"disposed")
        Expect.areEqual(numRenders,3,"numRenders #3")

        reset()
        storeOuter |> Store.modify ((+)1)

        Expect.areEqual(storeInner |> Store.countSubscribers,1,"1 inner sub after modify #3")
        Expect.areEqual(storeOuter |> Store.countSubscribers,1,"1 outer sub after modify #3")
        Expect.areEqual(disposed,3,"disposed")
        Expect.areEqual(numRenders,2,"numRenders #4")
    }

    // Test the patcher to make sure it replaces a real element with an effect rather
    // than just run the  effect
    it "Binding to non-bind effect" <| fun _ -> promise {
        let content = Store.make "Loading.."
        let mutable renderCount = 0

        let app() =
            Html.span [
                Bind.el(
                    "bind",
                    content,
                    fun html ->
                        Html.fragment [
                            do renderCount <- renderCount + 1
                            Html.parse html
                            Html.parse ("<pre>Wrapper" + html + "</pre>")
                        ]
                        // do renderCount <- renderCount + 1
                        // Html.parse html
                    )
            ]

        app() |> mountTestApp

        Expect.queryText "span>*:nth-child(1)>*" "Loading.." 
        Expect.queryNumChildren "span" 1

        "Content" |> Store.set content

        Expect.queryText "span>*:nth-child(1)>*" "Content" 
        Expect.queryNumChildren "span" 1

        return ()
    }

let init() = ()
