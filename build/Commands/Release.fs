module EasyBuild.Commands.Release

open System
open Spectre.Console.Cli
open EasyBuild.Workspace
open EasyBuild.Commands.Test
open EasyBuild.Commands.Deploy
open EasyBuild.Tools.ChangelogGen
open EasyBuild.Tools.DotNet
open EasyBuild.Tools.Git
open System.ComponentModel
open System.Collections.Generic

[<RequireQualifiedAccess>]
type Project =
    | Sutil
    | SutilBulma
    | SutilBulmaEngine
    | SutilHtml
    | SutilTransition
    | SutilWebComponents
    | All

type ProjectTypeConverter() =
    inherit TypeConverter()

    override _.ConvertFrom(_: ITypeDescriptorContext, _, value: obj) =
        match value with
        | :? string as text ->
            match text.ToLowerInvariant() with
            | "sutil" -> Project.Sutil
            | "sutil.bulma" -> Project.SutilBulma
            | "sutil.bulma.engine" -> Project.SutilBulmaEngine
            | "sutil.html" -> Project.SutilHtml
            | "sutil.transition" -> Project.SutilTransition
            | "sutil.webcomponents" -> Project.SutilWebComponents
            | "all" -> Project.All
            | _ -> raise <| InvalidOperationException("Invalid project name")

        | _ -> raise <| InvalidOperationException("Invalid project name")

type ReleaseSettings() =
    inherit CommandSettings()

    [<CommandOption("--project")>]
    [<TypeConverter(typeof<ProjectTypeConverter>)>]
    [<Description("""Name of the project to release

Possible values:
- Sutil
- Sutil.Bulma
- Sutil.Bulma.Engine
- Sutil.Html
- Sutil.Transition
- Sutil.WebComponents

If not specified, all projects will be released.
    """)>]
    member val Project = Project.All with get, set

let private releaseNuGetPackage (changelogPath: string) (forwardArguments: IReadOnlyList<string>) =

    ChangelogGen.run (
        changelogPath,
        // We allow dirty because we check for it before
        // and when releasing multiple projects, the repository
        // will be dirty after the first release
        allowDirty = true,
        preRelease = "beta",
        forwardArguments = (forwardArguments |> Seq.toList)
    )
    |> ignore

    let nupkgPath = DotNet.pack Workspace.src.``.``

    DotNet.nugetPush nupkgPath

let private releaseSutil (context: CommandContext) =
    releaseNuGetPackage Workspace.src.Sutil.``CHANGELOG.md`` context.Remaining.Raw

let private releaseSutilBulma (context: CommandContext) =
    releaseNuGetPackage Workspace.src.``Sutil.Bulma``.``CHANGELOG.md`` context.Remaining.Raw

let private releaseSutilBulmaEngine (context: CommandContext) =
    releaseNuGetPackage Workspace.src.``Sutil.BulmaEngine``.``CHANGELOG.md`` context.Remaining.Raw

let private releaseSutilHtml (context: CommandContext) =
    releaseNuGetPackage Workspace.src.``Sutil.Html``.``CHANGELOG.md`` context.Remaining.Raw

let private releaseSutilTransition (context: CommandContext) =
    releaseNuGetPackage Workspace.src.``Sutil.Transition``.``CHANGELOG.md`` context.Remaining.Raw

let private releaseSutilWebComponents (context: CommandContext) =
    releaseNuGetPackage Workspace.src.``Sutil.WebComponents``.``CHANGELOG.md`` context.Remaining.Raw

type ReleaseCommand() =
    inherit Command<ReleaseSettings>()
    interface ICommandLimiter<ReleaseSettings>

    override __.Execute(context, settings) =
        TestHeadlessCommand().Execute(context, TestHeadlessSettings()) |> ignore

        if Git.isDirty () then
            failwith
                "There are uncommitted changes in the repository. Please commit or stash them before releasing."

        match settings.Project with
        | Project.Sutil -> releaseSutil context
        | Project.SutilBulma -> releaseSutilBulma context
        | Project.SutilBulmaEngine -> releaseSutilBulmaEngine context
        | Project.SutilHtml -> releaseSutilHtml context
        | Project.SutilTransition -> releaseSutilTransition context
        | Project.SutilWebComponents -> releaseSutilWebComponents context
        | Project.All ->
            // Order matters !!!
            releaseSutil context
            releaseSutilBulmaEngine context
            releaseSutilBulma context
            releaseSutilHtml context
            releaseSutilTransition context
            releaseSutilWebComponents context

        Git.commit "chore: release new version"
        Git.push ()

        DeployCommand().Execute(context, DeploySettings()) |> ignore

        0
