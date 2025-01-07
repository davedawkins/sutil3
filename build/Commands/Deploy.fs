module EasyBuild.Commands.Deploy

open Spectre.Console.Cli
open SimpleExec
open BlackFox.CommandLine
open EasyBuild.Workspace
open EasyBuild.Commands.Docs
open System.ComponentModel
open Fake.IO

type DeploySettings() =
    inherit CommandSettings()

    [<CommandOption("-l|--local")>]
    [<Description("Deploy the docs locally, this is useful for
testing the website before deploying it.")>]
    member val IsLocal = false with get, set

type DeployCommand() =
    inherit Command<DeploySettings>()
    interface ICommandLimiter<DeploySettings>

    override __.Execute(context, settings) =

        // Generate the doc website
        DocsCommand().Execute(context, DocsSettings()) |> ignore

        Directory.delete VirtualWorkspace.``.fsdocs``.``.``

        // Generate all the projects DLLs, for generating the XML Docs
        Command.Run("dotnet", "build")

        // Generate the F# API Docs
        Command.Run(
            "dotnet",
            CmdLine.empty
            |> CmdLine.append "fsdocs"
            |> CmdLine.append "build"
            |> CmdLine.appendPrefix "--input" Workspace.apiDocs.``.``
            |> CmdLine.appendPrefix "--output" VirtualWorkspace.``output-fsdocs``.``.``
            // If we are running locally, we need to adjust the root path
            |> fun cmdLine ->
                if settings.IsLocal then
                    cmdLine |> CmdLine.appendRaw "--parameters root ../"
                else
                    cmdLine
            |> CmdLine.toString
        )

        // Combine the F# API Docs with the rest of the website

        // Make sure we start from a clean slate
        Directory.delete VirtualWorkspace.deploy.``.``

        Shell.copyDir
            VirtualWorkspace.deploy.``.``
            VirtualWorkspace.``output-fsdocs``.``.``
            FileFilter.allFiles

        Shell.copyDir
            VirtualWorkspace.deploy.``.``
            VirtualWorkspace.web.Docs.dist.``.``
            FileFilter.allFiles

        // Copy source files to the deploy folder, so we can load them in the examples
        Shell.copyDir
            VirtualWorkspace.deploy.sources.``.``
            Workspace.web.Docs.src.``.``
            FileFilter.allFiles

        if settings.IsLocal then
            printfn
                """Website ready for deployment.

To test the website locally, run:

    npx http-server deploy"""
        else
            // TODO: Deploy the website to the server
            // Can be easily be deployed to GitHub Pages with `npx gh-pages -d deploy`
            // But I think this project is using linode?
            // Check if we want to keep linode, or switch to GitHub Pages
            ()

        0
