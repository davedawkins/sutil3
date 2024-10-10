namespace Sutil

open System
open Sutil
open Sutil.CoreElements
open Sutil.Basic
open Sutil.Bind
open Fable.Core.JsInterop
open Feliz
open Browser
open Sutil.Internal

/// <summary>
/// Helpers for listening and reacting to media changes
/// </summary>
type Media =
    /// <summary>
    /// </summary>
    static member listenMedia(query: string, handler: bool -> unit) : Unsubscriber =
        let mql = window.matchMedia (query)
        handler (mql.matches)
        Sutil.Internal.EventListeners.add mql "change" (fun e -> e?matches |> handler)

    /// <summary>
    /// </summary>
    static member bindMediaQuery(query: string, view: bool -> SutilElement) =
        let s = Store.make false
        let u = Media.listenMedia (query, fun m -> s <~ m)

        fragment [
            unsubscribeOnUnmount [
                u
            ]
            disposeOnUnmount [
                s
            ]
            Bind.el (s, view)
        ]

    // /// <summary>
    // /// </summary>
    // static member showIfMedia (query:string, f:bool->bool, trans, view : SutilElement) =
    //     let s = Store.make false
    //     let u = Media.listenMedia(query, fun m -> s <~ m)
    //     Html.fragment [
    //         unsubscribeOnUnmount [ u ]
    //         disposeOnUnmount [ Helpers.disposable u; s ]
    //         transition trans (s .> f) view
    //     ]

    // static member showIfMedia (query:string, trans, view : SutilElement) =
    //     Media.showIfMedia(query,id,trans,view)

    /// <summary>
    /// </summary>
    static member media<'T>(query: string, map: bool -> 'T, app: IObservable<'T> -> SutilElement) =
        let s = Store.make false
        let u = Media.listenMedia (query, fun m -> s <~ m)

        fragment [
            unsubscribeOnUnmount [
                u
            ]
            disposeOnUnmount [
                s
            ]
            s .> map |> app
        ]

/// <summary>
/// Helpers for basic CSS media queries
/// </summary>
type CssMedia =
    /// <summary>
    /// Create a <c>@media</c> CSS rule for a custom condition and stylesheet
    /// </summary>
    static member custom(condition: string, rules) = Styling.makeMediaRule condition rules

    /// <summary>
    /// Create a <c>@media (min-width: &lt;nnn>)</c> CSS rule
    /// </summary>
    static member minWidth
        (minWidth: Styles.ICssUnit, rules: Sutil.Styling.Types.SutilStyleSheetDefinition list)
        =
        Styling.makeMediaRule (sprintf "(min-width: %s)" (string minWidth)) rules

    /// <summary>
    /// Create a <c>@media (max-width: &lt;nnn>)</c> CSS rule
    /// </summary>
    static member maxWidth
        (maxWidth: Styles.ICssUnit, rules: Sutil.Styling.Types.SutilStyleSheetDefinition list)
        =
        Styling.makeMediaRule (sprintf "(max-width: %s)" (string maxWidth)) rules
