Experimental rewrite of the Sutil core to support a diffing engine

New in Sutil 3

- DOM updates are optimised using a patching mechanism, very much like React
- withStyle uses nested stylesheets, rather than recoding all rule selectors to insert ".__sutil_xx" selectors
- Ev.input understands that it belongs to an HTMLInputElement, and so `e.targeElement.value` will work without casts
- Better handling of `fragment`. This removes a large amount of ugly code. If a Bind's view function returns a fragment
  this will be wrapped in a `div`. If the fragment is empty then the `div` will be set to `display: none`Bind.fs:module Sutil.Bind

# Modules and Namespaces

Notes from Maxime meeting Oct-5

- Docs are detailed but hard to parse. Comapre with Fable.Form docs
- Open in REPL is good!
- Try not to mix use of namespace / module
- What I Want to Work with . subgroup . helpers function
- SideEffect -> SutilSideEffect
- Dsl.fs into own package
- Upper case for Literals
- Prefix with SUTIL_
- <| only in tests
- Fable.Form fantomas settings
- [<Erase>] can remove reflection *eg Bind - use on static types eg - when no instance needed
- Use inline for 'rename' helpers
- EasyBuild

