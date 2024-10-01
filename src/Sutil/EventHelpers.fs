module EventHelpers

open Browser.Types

open Fable.Core

type TypedEvent<'T when 'T :> HTMLElement> =
    inherit Event

    // Tried this as an extension method, which compiled, but would only report
    // type of TypedEvent<HTMLInputElement>.targetElement (in editor) as literally 'T

    [<Emit("$0.currentTarget")>]
    abstract targetElement : 'T 
