module Sutil.Bind

open System
open Fable.Core
open Browser.Types

open Sutil.Core
open Sutil.Bindings

/// <summary>
/// Bindings for observables and the Core. For example, use an <c>IObservable&lt;bool></c> to toggle an element's <c>class</c> attribute
/// </summary>
[<Erase>]
type Bind =

    /// <summary>
    /// For <c>input[type='radio']</c>
    /// Only the checkbox with store's current value will be checked at any one time.
    /// </summary>
    static member radioValue<'T>(store: IStore<'T>) = Sutil.Bindings.bindRadioGroup store

    /// <summary>
    /// For multiple input elements. The input elements are grouped explicitly by name, or will be implicitly grouped by
    /// the (internal) name of the binding store.
    /// Each checkbox in the group is checked if its value is contained in the current <c>string list</c>
    /// </summary>
    static member checkboxGroup(store: IStore<string list>) = Sutil.Bindings.bindGroup store

    static member selectMultiple<'T when 'T: equality>(store: IStore<'T list>) =
        Sutil.Bindings.bindSelectMultiple store

    static member selectOptional<'T when 'T: equality>(store: IStore<'T option>) =
        Sutil.Bindings.bindSelectOptional store

    static member selectSingle<'T when 'T: equality>(store: IStore<'T>) =
        Sutil.Bindings.bindSelectSingle store

    static member selectSingle<'T when 'T: equality>
        (value: System.IObservable<'T>, dispatch: 'T -> unit)
        =
        bindSelected (value .> List.singleton) (List.exactlyOne >> dispatch)

    static member selectMultiple<'T when 'T: equality>
        (value: System.IObservable<'T list>, dispatch: 'T list -> unit)
        =
        bindSelected value dispatch

    ///<summary>
    /// Bind a scalar value to an element attribute. Listen for onchange events and dispatch the
    /// attribute's current value to the given function. This form is useful for view templates
    /// where v is invariant (for example, an each that already filters on the value of v, like Todo.Done)
    ///</summary>
    static member attrInit<'T>(attrName: string, initValue: 'T, dispatch: 'T -> unit) =
        attrNotify attrName initValue dispatch

    /// Two-way binding from value to attribute and from attribute to dispatch function
    static member attr<'T>(name: string, value: IObservable<'T>, dispatch: 'T -> unit) =
        bindAttrBoth name value dispatch

    /// Dual-binding for a given attribute. Changes to value are written to the attribute, while
    /// changes to the attribute are written back to the store. Note that an IStore is also
    /// an IObservable, for which a separate overload exists.
    static member attr<'T>(name: string, value: IStore<'T>) = bindAttrStoreBoth name value

    /// One-way binding from value to attribute. Note that passing store to this function will
    /// select the more specific `attr<'T>( string, IStore<'T>)` overload.
    /// If that looks to be a problem, we'll rename both of them to force a considered choice.
    static member attr<'T>(name: string, value: IObservable<'T>) = bindAttrIn name value

    /// One-way binding from attribute to dispatch function
    static member attr<'T>(name: string, dispatch: 'T -> unit) = bindAttrOut name dispatch

    /// One way binding from style values into style attribute
    static member style(attrs: IObservable<#seq<string * obj>>) =
        Bind.attr ("style", attrs |> Store.map cssAttrsToString)

    static member booleanAttr(name: string, isTrue: IObservable<bool>) = bindBoolAttr isTrue name

    /// One way binding from custom values to style updater function. This allows updating of the element's <c>style</c> property rather than the style attribute string.
    static member style<'T>(values: IObservable<'T>, updater: CSSStyleDeclaration -> 'T -> unit) =
        bindStyle values updater

    static member effect<'T, 'E when 'E :> HTMLElement>
        (values: IObservable<'T>, updater: 'E -> 'T -> unit)
        =
        bindElementEffect values updater

    static member leftTop(xy: IObservable<float * float>) = bindLeftTop xy

    static member widthHeight(xy: IObservable<float * float>) = bindWidthHeight xy

    static member toggleClass
        (toggle: IObservable<bool>, activeClass: string, inactiveClass: string)
        =
        bindClassToggle toggle activeClass inactiveClass

    static member toggleClass(toggle: IObservable<bool>, activeClass: string) =
        bindClassToggle toggle activeClass ""

    static member className(name: IObservable<string>) = bindClassName name

    static member classNames(name: IObservable<#seq<string>>) = bindClassNames name

    /// Binding from value to a DOM fragment. Each change in value replaces the current DOM fragment
    /// with a new one.
    static member el<'T>(value: IObservable<'T>, element: 'T -> SutilElement) : SutilElement =
        bindElement value element (fun _ _ -> false)

    static member el<'T>
        (name: string, value: IObservable<'T>, element: 'T -> SutilElement)
        : SutilElement
        =
        bindElementWithName name value element (fun _ _ -> false)

    static member el<'T, 'K when 'K: equality>
        (name: string, value: IObservable<'T>, key: 'T -> 'K, element: 'T -> SutilElement)
        : SutilElement
        =
        bindElementK name value element key

    static member el<'T, 'K when 'K: equality>
        (value: IObservable<'T>, key: 'T -> 'K, element: 'T -> SutilElement)
        : SutilElement
        =
        bindElementK "bindElementK" value element key

    /// Binding from two values to a DOM fragment. See fragment<'T>
    static member el2<'A, 'B>
        (valueA: IObservable<'A>)
        (valueB: IObservable<'B>)
        (element: 'A * 'B -> SutilElement)
        =
        bindElement2 valueA valueB element

    /// Deprecated naming, use Bind.el
    static member fragment2<'A, 'B>
        (valueA: IObservable<'A>)
        (valueB: IObservable<'B>)
        (element: 'A * 'B -> SutilElement)
        =
        bindElement2 valueA valueB element

    static member selected<'T when 'T: equality>
        (value: IObservable<'T list>, dispatch: 'T list -> unit)
        =
        bindSelected value dispatch

    static member selected<'T when 'T: equality>(store: IStore<'T list>) = bindSelectMultiple store

    static member selected<'T when 'T: equality>(store: IStore<'T option>) =
        bindSelectOptional store

    static member selected<'T when 'T: equality>(store: IStore<'T>) = bindSelectSingle store

    // -- Simple cases: 'T -> view ---------------------------

    /// Bind lists to a simple template
    static member each(items: IObservable<list<'T>>, view: 'T -> SutilElement) =
        each (listWrapO items) view

    // -- Keyed ----------------------------------------------

    /// Bind keyed lists to a simple template
    /// Deprecated: Use a view template that takes IObservable<'T>
    static member each
        (items: IObservable<list<'T>>, view: 'T -> SutilElement, key: 'T -> 'K)
        : SutilElement
        =
        eachk (listWrapO items) view key

    static member eachi
        (items: IObservable<list<'T>>, view: (int * 'T) -> SutilElement)
        : SutilElement
        =
        eachi (listWrapO items) view

    static member promises
        (
            name: string,
            items: IObservable<JS.Promise<'T>>,
            view: 'T -> SutilElement,
            waiting: SutilElement,
            error: Exception -> SutilElement
        )
        =
        Bind.el (name, items, fun p -> Bind.promise (name, p, view, waiting, error))

    static member promises
        (
            items: IObservable<JS.Promise<'T>>,
            view: 'T -> SutilElement,
            waiting: SutilElement,
            error: Exception -> SutilElement
        )
        =
        Bind.el (items, fun p -> Bind.promise (p, view, waiting, error))

    static member promise
        (
            name: string,
            p: JS.Promise<'T>,
            view: 'T -> SutilElement,
            waiting: SutilElement,
            error: Exception -> SutilElement
        )
        =
        let pob = p.ToObservable()

        // Wow. Even if I do say so myself. I was wondering how to dispose of pob, and then I realized I could just
        // wrap the Bind.el in a fragment, include a disposeOnUmount and let Sutil deal with it.

        SutilElement.Fragment(
            [|
                Bind.el (
                    name,
                    pob,
                    fun state ->
                        match state with
                        | PromiseState.Waiting -> waiting
                        | PromiseState.Error x -> error x
                        | PromiseState.Result r -> view r
                )
                CoreElements.disposeOnUnmount [
                    pob
                ]
            |]
        )

    static member promise
        (
            p: JS.Promise<'T>,
            view: 'T -> SutilElement,
            waiting: SutilElement,
            error: Exception -> SutilElement
        )
        =
        Bind.promise ("promise", p, view, waiting, error)

    static member promise(name: string, p: JS.Promise<'T>, view: 'T -> SutilElement) =
        let w =
            Basic.el "div" [
                Basic.attr "class" "promise-waiting"
                Basic.text "waiting..."
            ]

        let e (x: Exception) =
            Basic.el "div" [
                Basic.attr "class" "promise-error"
                Basic.text x.Message
            ]

        Bind.promise (name, p, view, w, e)

    static member promise(p: JS.Promise<'T>, view: 'T -> SutilElement) =
        Bind.promise ("promise", p, view)

/// <summary>
/// Bindings for array observables and the Core. For example, <c>IObservable&lt;int[]></c>. The <c>Bind</c> class already handles lists. <c>BindArray</c> exists to eliminate compilation errors related to overloading of the <c>each</c> method.
/// </summary>
type BindArray =

    /// Bind arrays to a simple template
    static member each(items: IObservable<'T[]>, view: 'T -> SutilElement) =
        each (arrayWrapO items) view

    /// Bind keyed arrays to a simple template
    /// Deprecated: Use a view template that takes IObservable<'T>
    static member each
        (items: IObservable<array<'T>>, view: 'T -> SutilElement, key: 'T -> 'K)
        : SutilElement
        =
        eachk (arrayWrapO items) view key

    // -- Indexed Arrays --------------------------------------------

    static member eachi
        (items: IObservable<array<'T>>, view: (int * 'T) -> SutilElement)
        : SutilElement
        =
        eachi (arrayWrapO items) view

///<summary>
/// Operators to help with bindings
/// </summary>
[<AutoOpen>]
module BindOperators =

    /// <summary>
    /// An alias for <c>Bind.el</c>
    /// </summary>
    /// <example>
    /// Consider this binding:
    /// <code>
    ///     Bind.el( model .> errorMessage, fun msg -> Html.div [ text msg ] )
    /// </code>
    /// This operator would allow this to be rewritten as
    /// <code>
    ///     model .> errorMessage >/ fun msg -> Html.div [ text msg ]
    /// </code>
    /// In fact, for this particular message, you can just write
    /// <code>
    ///     model .> errorMessage >/ Html.div
    /// </code>
    /// </example>
    let (>/) a b = Bind.el (a, b)
