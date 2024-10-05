module LogicElse

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Dsl
open Sutil.CoreElements
open Sutil.Bind


let view() =
    let user = Store.make {| loggedIn = false |}

    let toggle _ =
        user |> Store.modify (fun u -> {| u with loggedIn = not u.loggedIn |})

    Html.div [
        disposeOnUnmount [ user ]

        Bind.el(user, fun u ->
            Html.div [
                if u.loggedIn then
                    Html.button [
                        Ev.onClick toggle
                        text "Log out"
                    ]
                else
                    Html.button [
                        Ev.onClick toggle
                        text "Log in"
                    ]
            ])
    ]
