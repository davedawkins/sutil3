
module Bind

open Core
open CoreTypes
open Store
open Browser.Types
open Fable.Core.JS

type Bind =
    static member el( source : 'T observable, view : ('T -> SutilElement) ) =
        SutilElement.SideEffect <|
            fun context -> 

                let mutable currentNode : Node = null

                source.Subscribe( fun value ->
                    let se = view value
                    let ve = VirtualDom.fromSutil se

                    let actions =
                        Patch.calculatePatch currentNode ve

                    console.log( actions |> Seq.map (sprintf "%A") |> String.concat "\n:")
                    currentNode <- Patch.applyPatch context.ParentElement actions
                ) |> ignore
                ()
    
