Experimental rewrite of the Sutil core to support a diffing engine

New in Sutil 3

- DOM updates are optimised using a patching mechanism, very much like React
- withStyle uses nested stylesheets, rather than recoding all rule selectors to insert ".__sutil_xx" selectors
- Ev.input understands that it belongs to an HTMLInputElement, and so `e.targeElement.value` will work without casts
- Better handling of `fragment`. This removes a large amount of ugly code. If a Bind's view function returns a fragment
  this will be wrapped in a `div`. If the fragment is empty then the `div` will be set to `display: none`