Experimental rewrite of the Sutil core to support a diffing engine

New in Sutil 3

- DOM updates are optimised using a patching mechanism, very much like React
- withStyle uses nested stylesheets, rather than recoding all rule selectors to insert ".__sutil_xx" selectors
- Ev.input understands that it belongs to an HTMLInputElement, and so `e.targeElement.value` will work without casts
- Better handling of `fragment`. This removes a large amount of ugly code. If a Bind's view function returns a fragment
  this will be wrapped in a `div`. If the fragment is empty then the `div` will be set to `display: none`Bind.fs:module Sutil.Bind

# Modules and Namespaces

Top level namespace is Sutil

Namespaces:

Sutil
Sutil.Dom
Sutil.Elmish
Sutil.Dsl

Modules are:

Namespace : Sutil.Dom

module Types =
module TypeHelpers =
module Logging = 
module JsHelpers =
module JsMap =
module NodeKey =
module EventListeners =
module Id =
module CustomEvents =
module DomHelpers =
module Dispose =
module DomEdit =
module ClassHelpers =
module Extensions =

Bind.fs:module BindOperators =


Internal

Bindings.fs:module internal Sutil.Bindings

namespace Sutil.Elmish

Cmd.fs:module Sutil.Elmish.Cmd
Cmd.fs:// TODO: Refactor this into Sutil.Elmish module
Cmd.fs:module internal Timer =
Cmd.fs:module OfFunc =
Cmd.fs:module OfAsyncWith =
Cmd.fs:module OfAsync =
Cmd.fs:module OfAsyncImmediate =
Cmd.fs:module OfPromise =
Cmd.fs:module OfTask =
Core.fs:module Sutil.Core
Core.fs:module Sutil2 =
Core.fs:    module Interop =
Core.fs:    module Logging =
Core.fs:    module Ext =
CoreTypes.fs:module Sutil.CoreTypes
CoreTypes.fs:module CollectionWrapperExt =
CoreTypes.fs:module internal CollectionWrapper =
Dsl.fs:module Sutil.Dsl
Dsl.fs:module Html =
Dsl.fs:module EngineHelpers =
Dsl.fs:module PseudoCss =
Elmish.fs:module Sutil.Elmish
Elmish.fs:module internal Helpers =
EventHelpers.fs:module Sutil.EventHelpers
Helpers.fs:module Sutil.Helpers
Log.fs:module Sutil.Log
Patch.fs:module Sutil.Patch 
ResizeObserver.fs:module Sutil.ResizeObserver

Store.fs:module Sutil.Store 
Store.fs:module Observable =
Store.fs:module StoreOperators =
Store.fs:module StoreExtensions =
Store.fs:module ObservablePromise =
Style.fs:module Sutil.Style 
StyleDomHelpers.js.fs:module Sutil.StyleDomHelpers 
VirtualDom.fs:module Sutil.VirtualDom 

DomHelpers.fs:namespace Sutil.Dom
Dsl.fs:    /// own namespace. For example
Program.fs:namespace Sutil
