
module Bind

open Core
open CoreTypes
open Store
open Browser.Types

// Simplest binding
type Bind =
    static member el( source : 'T observable, view : ('T -> SutilElement) ) =
        SutilElement.SideEffect <|
            fun context -> 

                let mutable currentNode : Node = null

                source.Subscribe( fun value ->

                    let patchAction =
                        value
                        |> view
                        |> VirtualDom.fromSutil
                        |> Patch.calculatePatch currentNode

                    //Fable.Core.JS.console.log( "Patch action: ", Fable.Core.JS.JSON.stringify( patchAction, space = 4) )
                    Fable.Core.JS.console.log( "Patch action: ", patchAction.ToString() )

                    currentNode <- Patch.applyPatch context currentNode patchAction |> snd

                ) |> ignore
                ()
    
