module EasyBuild.Commands.QuickTest

open EasyBuild.Workspace
open Spectre.Console.Cli
open EasyBuild.Tools.Fable
open EasyBuild.Tools.Vite

type QuickTestSettings() =
    inherit CommandSettings()

    [<CommandOption("-w|--watch")>]
    member val IsWatch = false with get, set

type QuickTestCommand() =
    inherit Command<QuickTestSettings>()
    interface ICommandLimiter<QuickTestSettings>

    override __.Execute(context, settings) =

        if settings.IsWatch then
            Async.Parallel [
                Fable.watch (
                    projFileOrDir = Workspace.web.QuickTest.``QuickTest.fsproj``,
                    verbose = true,
                    sourceMaps = true
                )
                |> Async.AwaitTask

                Vite.watch (workingDirectory = Workspace.web.QuickTest.``.``) |> Async.AwaitTask
            ]
            |> Async.RunSynchronously
            |> ignore

        else

            Fable.build (projFileOrDir = Workspace.web.QuickTest.``QuickTest.fsproj``)

            Vite.build (workingDirectory = Workspace.web.QuickTest.``.``)

            printfn
                """Build completed successfully

If you want to test your code in production mode, run:

    npx http-server web/Small/dist

This will start a local server to serve your files."""

        0
