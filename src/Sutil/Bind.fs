
module Sutil.Bind

open Core
open CoreTypes
open Store
open Browser.Types

// Simplest binding
type Bind =
    static member el( source : 'T observable, view : ('T -> SutilElement) ) =
        SutilElement.SideEffect (
            "bindEl",
            fun context -> 

                let context = context.WithMount BuildContext.DefaultMount

                let mutable currentNode : Node =
                    SutilElement.HiddenDiv
                    |> mount context null

                source.Subscribe( fun value ->

                    currentNode <-
                        value
                        |> view 
                        |> mount context currentNode

                ) |> DomHelpers.Dispose.addDisposable (context.ParentElement) // FIXME: Put this in the disposals list

                EffectedNode (context.ParentElement)
        )    

    static member each<'T,'K>( source : ('T list) observable, view : ('T -> SutilElement) ) =
        Bind.el( source, (List.toArray >> Array.map view >>  SutilElement.Fragment) )

    static member each<'T,'K>( source : ('T list) observable, view : ('T -> SutilElement), key: 'T -> 'K) =
        Bind.el( source, (List.toArray >> Array.map view >>  SutilElement.Fragment) )

    static member each<'T,'K>( source : ('T list) observable, view : ('T observable -> SutilElement), key: 'T -> 'K) =
        SutilElement.SideEffect (
            "Bind.each",
            fun context -> 
                EffectedNode (context.ParentElement)
        )

type BindArray =
    static member each<'T,'K>( source : ('T[]) observable, view : ('T -> SutilElement), key: 'T -> 'K) =
        Bind.el( source, (Array.map view >>  SutilElement.Fragment) )

    static member each<'T,'K>( source : ('T[]) observable, view : ('T observable -> SutilElement), key: 'T -> 'K) =
        SutilElement.SideEffect (
            "BindArray.each",
            fun context -> 
                EffectedNode (context.ParentElement)
        )