module EasyBuild.Main

open Spectre.Console.Cli
open EasyBuild.Commands.QuickTest
open EasyBuild.Commands.Docs
open EasyBuild.Commands.Deploy
open EasyBuild.Commands.Release
open EasyBuild.Commands.Test
open SimpleExec
open System.Runtime.InteropServices

[<EntryPoint>]
let main args =

    Command.Run("dotnet", "husky install")

    let app = CommandApp()

    app.Configure(fun config ->
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            config.Settings.ApplicationName <- "./build.cmd"
        else
            config.Settings.ApplicationName <- "./build.sh"

        config.AddBranch<TestSettings>(
            "test",
            fun test ->
                test
                    .AddCommand<TestHeadlessCommand>("headless")
                    .WithDescription("Run tests in headless mode")
                |> ignore

                test
                    .AddCommand<TestInBrowserCommand>("browser")
                    .WithDescription("Run tests in browser")
                |> ignore
        )
        |> ignore

        config.AddCommand<DocsCommand>("docs").WithDescription("Run the docs project")
        |> ignore

        config
            .AddCommand<QuickTestCommand>("quicktest")
            .WithDescription("Run the QuickTest project")
        |> ignore

        config
            .AddCommand<DeployCommand>("deploy")
            .WithDescription("Deploy the docs project")
        |> ignore

        config
            .AddCommand<ReleaseCommand>("release")
            .WithDescription(
                """Package a new version of the libraries and publish it to NuGet.
This will also deploy the documentation to GitHub Pages.

You can pass additional arguments to EasyBuild.ChangelogGen tool by appending them after `--`.
For example:
    ./build.sh release --project Sutil -- --force-version 1.2.3

Run `dotnet changelog-gen --help` for more information"""
            )
        |> ignore
    )

    app.Run(args)
