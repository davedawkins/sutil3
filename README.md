New in Sutil 3

- DOM updates are optimised using a patching mechanism. Amongst other things, this means that an Html.input element
  can be re-rendered without losing focus

- Ev.input understands that it belongs to an HTMLInputElement, and so `e.targeElement.value` will work without casts

- Better handling of `fragment`. This removes a large amount of ugly code. If a Bind's view function returns a fragment
  this will be wrapped in a `div`. If the fragment is empty then the `div` will be set to `display: none`Bind.fs:module Sutil.Bind

- Separation of Feliz.Engine DSL into Sutil.Html. This allows for other DSLs to be used with the core of Sutil

- Separation of Bulma components into Sutil.Bulma. This allows for a smaller core for those who don't need or want Bulma.

- Re-organization of modules and API so that it should be easier to find the functionality you're looking for with Intellisense

- Optimizations to Fable code generation, so that bundle sizes will be smaller.


