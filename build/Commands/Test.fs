module EasyBuild.Commands.Test

open Spectre.Console.Cli
open SimpleExec
open BlackFox.CommandLine
open EasyBuild.Workspace
open EasyBuild.Tools.Fable
open EasyBuild.Tools.Vite
open EasyBuild.Tools.Npm
open Fake.IO

type TestSettings() =
    inherit CommandSettings()

type TestHeadlessSettings() =
    inherit TestSettings()

    [<CommandOption("-w|--watch")>]
    member val IsWatch = false with get, set

type TestHeadlessCommand() =
    inherit Command<TestHeadlessSettings>()
    interface ICommandLimiter<TestSettings>

    override __.Execute(context, settings) =

        Npm.install ()

        // Make sure we start from a clean slate
        Directory.delete VirtualWorkspace.tests.fableBuild.``.``

        let webTestRunnerCmd =
            CmdLine.empty
            |> CmdLine.appendRaw "web-test-runner"
            |> CmdLine.appendRaw (Workspace.tests.src.``.`` + "/*Test.fs.js")
            |> CmdLine.appendRaw "--node-resolve"
            |> CmdLine.toString

        if settings.IsWatch then
            Fable.watch (
                projFileOrDir = Workspace.tests.src.``Tests.fsproj``,
                define =
                    [
                        "HEADLESS"
                    ],
                runWatch = webTestRunnerCmd
            )
            |> Async.AwaitTask
            |> Async.RunSynchronously
            |> ignore

        else

            Fable.build (
                projFileOrDir = Workspace.tests.src.``Tests.fsproj``,
                define =
                    [
                        "HEADLESS"
                    ]
            )

            Command.Run("npx", webTestRunnerCmd)

        0

type TestInBrowserSettings() =
    inherit CommandSettings()

type TestInBrowserCommand() =
    inherit Command<TestInBrowserSettings>()
    interface ICommandLimiter<TestSettings>

    override __.Execute(context, settings) =

        Npm.install ()

        // Make sure we start from a clean slate
        Directory.delete VirtualWorkspace.tests.fableBuild.``.``

        // We don't offer a non watch mode for this command
        // as we don't know how to automatically run the tests in the browser

        Async.Parallel [
            Fable.watch (projFileOrDir = Workspace.tests.src.``Tests.fsproj``, verbose = true)
            |> Async.AwaitTask

            Vite.watch (workingDirectory = Workspace.tests.``.``) |> Async.AwaitTask
        ]
        |> Async.RunSynchronously
        |> ignore

        0
