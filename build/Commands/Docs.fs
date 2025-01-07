module EasyBuild.Commands.Docs

open Spectre.Console.Cli
open SimpleExec
open EasyBuild.Workspace
open EasyBuild.Tools.Fable
open EasyBuild.Tools.Vite

type DocsSettings() =
    inherit CommandSettings()

    [<CommandOption("-w|--watch")>]
    member val IsWatch = false with get, set

type DocsCommand() =
    inherit Command<DocsSettings>()
    interface ICommandLimiter<DocsSettings>

    override __.Execute(context, settings) =

        if settings.IsWatch then
            Async.Parallel [
                Fable.watch (
                    projFileOrDir = Workspace.web.Docs.src.``Docs.fsproj``,
                    verbose = true,
                    sourceMaps = true
                )
                |> Async.AwaitTask

                Vite.watch (workingDirectory = Workspace.web.Docs.``.``) |> Async.AwaitTask
            ]
            |> Async.RunSynchronously
            |> ignore

        else

            Fable.build (projFileOrDir = Workspace.web.Docs.src.``Docs.fsproj``)

            Vite.build (workingDirectory = Workspace.web.Docs.``.``)

            printfn
                """Build completed successfully

If you want to test your code compiled in production mode, run:

    npx http-server deploy

This will start a local server to serve your files."""

        0
