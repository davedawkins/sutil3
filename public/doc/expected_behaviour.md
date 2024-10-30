
## Events

### onMount

Called when an element is mounted into the DOM

### onUnmount

Called when an element is removed permanently from the DOM

# Fragments

## Top-level Fragments

For example:

    let view() =
        Html.fragment [
            Html.h1 "A heading"
            Html.p "A paragraph"
        ]

    view() |> Program.mount 

Assume that index.html looks like this:

    <body>
        <div id='sutil-app'></div>
    </body>

The resulting DOM will be:

    <body>
        <div id='sutil-app'>
         ????
        </div>
    </body>

## Contained Fragments

For example:

    Html.div [
        Html.h4 "Section 1"
        Html.fragment [
            Html.p "Details about section 1"
            Ev.onClick (fun _ -> console.log("Clicked"))
        ]
    ]

Fragments are flattened so that their children become children of the fragment's parent, and the
fragment construct itself disappears. The example above is equivalent to:

    Html.div [
        Html.h4 "Section 1"
        Html.p "Details about section 1"
        Ev.onClick (fun _ -> console.log("Clicked"))
    ]

The following example also flattens in the same way to same result:

    Html.div [
        Html.h4 "Section 1"
        Html.fragment [
            Html.fragment [
                Html.p "Details about section 1"
            ]
            Ev.onClick (fun _ -> console.log("Clicked"))
        ]
    ]

Flattening cannot cross a `Bind.el` boundary though. For example:

    Html.div [
        Html.h4 "Section 1"

        Bind.el( details, fun deets -> 
            Html.fragment [
                Html.p deets
            ])
    ]

No flattening can occur here. See the section below on `Bind With Fragments`

## Bind with Fragments

For example, when a Bind uses Html.fragment as a top-level container:

    let details = Store.make "Details on section 1"

    Html.div [
        Html.h4 "Section 1"

        Bind.el( details, fun deets -> 
            Html.fragment [
                Html.p deets
            ])
        
        Html.footer [ text "Please see T&Cs" ]
    ]

## Mount and Unmount Events With Binding

Sutil's `Bind.el` method will replace existing nodes with new ones (though it may produce and empty fragment - see the notes on Binding With Fragments), and this will invoke the patcher to integrate the new element into the DOM.

1. An element can be patched (edited in-situ) if and only if the new node is an element with the same tag.
2. A text node can be patched if and only if the new node is also a text node.

Every other combination will result in either a straightforward edit by unmounting any existing node, and inserting the new one. For these cases, the Mount and Unmount events are called as expected.

For patching operations, the Mount and Unmount events are not invoked.

For example.

Element #1

    Html.div [
        Ev.onMount (fun _ -> console.log("Mounted: Alice"))
        text "Alice"
        Ev.onUnmount (fun _ -> console.log("Unmounted: Alice"))
    ]

After a binding fires, the following the element is rendered in its place

Element #2

    Html.div [
        Ev.onMount (fun _ -> console.log("Mounted: Bob"))
        text "Bob"
        Ev.onUnmount (fun _ -> console.log("Unmounted: Bob"))
    ]

After the same binding fires again, we then produce

    Html.p [
        Ev.onMount (fun _ -> console.log("Mounted: Carol"))
        text "Carol"
        Ev.onUnmount (fun _ -> console.log("Unmounted: Carol"))
    ]

Up this point we will see the following messages logged:

```
Mounted: Alice
Unmounted: Bob
Mounted: Carol
```

Line 1 `Mounted: Alice` is written because the new `div`  for "Alice" has been created

Line 2 `Unmounted: Bob` is written because we have to remove the `div`

Line 3 `Mounted: Carol` is written because a new `p` element is created

Note that we do not get a message to say `Unmounted: Alice` - we changed the behaviour 
of Unmount event to say "Unmounted: Bob" instead.

Note also that there is no message printed when we patch element #1 into #2. Element #1 becomes #2, and neither is being mounted or unmounted - just changing in situ.

Here is an example that shows why it works like this

    let nameS = Store.make "Alice"

    Bind.el( nameS, fun name -> 
        Html.div [
            Ev.onUnmount (fun _ -> nameS.Dispose() )
            text name
        ]
    )

Now, update `nameS`:

    "Bob" |> Store.set nameS

We do not want `nameS` to be disposed!






