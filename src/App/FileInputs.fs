module FileInputs

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Html
open Sutil.CoreElements
open Sutil.Bind

module Helpers =
    open Browser.Types

    let fileListToSeq (files: FileList) : File seq =
        if not (isNull files) then
            seq {
                for i in
                    [
                        0 .. files.length - 1
                    ] do
                    yield files.[i]
            }
        else
            Seq.empty

let view () =
    let files = Store.make Unchecked.defaultof<Browser.Types.FileList>
    let fileSeq = files |> Store.map (Helpers.fileListToSeq >> Seq.toList)

    let unsub =
        fileSeq
        |> (Store.iter (fun fileSeq ->
            // Note that `files` is of type `FileList`, not an Array:
            // https://developer.mozilla.org/en-US/docs/Web/API/FileList
            //console.log(files);

            for file in fileSeq do
                Browser.Dom.console.log ($"{file.name}: {file.size} bytes")
        ))

    Html.div [
        disposeOnUnmount [
            files
            unsub
        ]

        Html.divc "block" [
            Html.labelc "file-label" [
                Attr.for' "avatar"
                text "Upload a picture:"
            ]
            Bulma.inputFile [
                Attr.accept "image/png, image/jpeg"
                Bind.attr ("files", files)
                Attr.id "avatar"
                Attr.name "avatar"
            ]
        ]

        Html.divc "block" [
            Html.labelc "file-label" [
                Attr.for' "many"
                text "Upload multiple files of any type:"
            ]
            Bulma.inputFile [
                Bind.attr ("files", files)
                Attr.id "many"
                Attr.multiple true
                Attr.typeFile
            ]
        ]

        Bind.el (
            fileSeq,
            fun _files ->
                Html.divc "control" [
                    Bulma.h3 [
                        text "Selected files"
                    ]
                    for file in _files do
                        Html.p [
                            text $"{file.name} ({file.size} bytes)"
                        ]
                ]
        )
    ]
