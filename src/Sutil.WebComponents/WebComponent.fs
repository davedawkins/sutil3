namespace Sutil

open Fable.Core
open Browser.Types
open Sutil.Internal
open Core

module AdoptStyleSheet =
    open Browser
    open Sutil.Styling.Types

    type internal Node with
        /// returns this DocumentOrShadow adopted stylesheets or sets them.
        /// https://wicg.github.io/construct-stylesheets/#using-constructed-stylesheets
        [<Emit("$0.adoptedStyleSheets{{=$1}}")>]
        member __.adoptedStyleSheets with get(): CSSStyleSheet array = jsNative and set(v: CSSStyleSheet array) = jsNative

        [<Emit("$0.getRootNode()")>]
        member __.getRootNode() : Node = jsNative


    let adoptStyleSheet (styleSheet : Sutil.Styling.Types.SutilStyleRule list) =
        CoreElements.onElementMounted( fun el ->
            //"adoptStyleSheet",
            let run() =
                let sheet = CSSStyleSheet.Create()
                sheet.replaceSync (styleSheet |> SutilStyleSheet.Of |> Sutil.Styling.Renderer.styleSheetAsText)

                let rootNode : Node = el.getRootNode()

                rootNode.adoptedStyleSheets <- Array.concat [ rootNode.adoptedStyleSheets; [| sheet |] ]

            if Sutil.Internal.Node.isConnected el then
                run()
            else
                Timers.rafu run
        )

/// <summary>
/// Support for defining Web Components in Sutil
/// </summary>
module private WebComponent =
    type Callbacks<'T> = {
        OnDisconnected : (unit -> unit)
        GetModel : unit -> 'T
        SetModel : 'T -> unit
        OnConnected : unit -> unit
    }

    [<Import("makeWebComponent", "./webcomponentinterop.js")>]
    let makeWebComponent<'T> name (ctor : Node -> Callbacks<'T>) (init : 'T) : unit = jsNative

open Fable.Core.JsInterop

/// <exclude/>
[<Global>]
type ShadowRoot() =
    member internal this.appendChild(el: Browser.Types.Node) = jsNative

    static member mount (app : SutilElement) (host: Node) : (unit -> unit) =
        // TODO
        // let el = Core.buildWith

        // match el with
        // | DomNode node ->
        //     let shadowRoot: ShadowRoot = host?shadowRoot
        //     shadowRoot.appendChild (node)
        // let dispose () =
        //     el.Dispose()

        ignore


/// <summary>
/// Support for defining Web Components in Sutil
/// </summary>
type WebComponent =

    static member Register<'T>(name:string, view : IStore<'T> -> Node -> SutilElement, initValue : 'T, initModel: unit -> IStore<'T>, dispose : IStore<'T> -> unit) =

        // If model is instantiated here, then it doesn't get captured correctly within 'wrapper', with multiple calls
        // to Register() (such as the Counter example)
        //let model = initModel()

        let wrapper (host:Node) : WebComponent.Callbacks<'T> =
            let model = initModel()

            let sutilElement = view model host
            let disposeElement = ShadowRoot.mount sutilElement host

            let disposeWrapper() =
                dispose(model)
                disposeElement()

            {   OnDisconnected = disposeWrapper
                GetModel = (fun () -> model |> Store.current)
                SetModel = Store.set model
                OnConnected = fun _ -> CustomEvents.CustomDispatch<_>.dispatch( (host?shadowRoot?firstChild) :> EventTarget, CustomEvents.CONNECTED ) //"sutil-connected"
                }

        WebComponent.makeWebComponent name wrapper initValue

    static member Register<'T>(name:string, view : IStore<'T> -> Node -> SutilElement, init : 'T ) =
        WebComponent.Register( name, view, init, (fun () -> Store.make init), (fun s -> s.Dispose()))

    static member Register<'T>(name:string, view : IStore<'T> -> SutilElement, init : 'T ) =
        WebComponent.Register( name, (fun store _ -> view store), init, (fun () -> Store.make init), (fun s -> s.Dispose()))
