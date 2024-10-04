module Sutil.Log

open Fable.Core

type ILog =
    abstract log: ?message: obj * [<System.ParamArray>] optionalParams: obj[] -> unit
    abstract trace: ?message: obj * [<System.ParamArray>] optionalParams: obj[] -> unit
    abstract error: ?message: obj * [<System.ParamArray>] optionalParams: obj[] -> unit
    abstract warning: ?message: obj * [<System.ParamArray>] optionalParams: obj[] -> unit
    abstract info: ?message: obj * [<System.ParamArray>] optionalParams: obj[] -> unit

[<Emit("window.console.log")>]
let private _console_log : obj = jsNative

// 
let Console = 
    {|
        log = _console_log
    |} :> obj :?> ILog

let mutable private sources : Map<string,bool> = 
    [
    ] |> Map

let mutable private categories : Map<string,bool> = 
    [
        "Info"   , true
        "Warning", true
        "Error"  , true
        "Debug"  , false
        "Trace"  , false
    ] |> Map

let private sourceIsEnabled src = not (sources.ContainsKey src) || sources[src]
let private categoryIsEnabled src = not (categories.ContainsKey src) || categories[src]

type LogCategory =
    | Info 
    | Warning
    | Error 
    | Debug 
    | Trace
    | Custom of string
    with    
        override __.ToString() =
            match __ with
            | Info -> "Info"
            | Debug -> "Debug"
            | Error -> "Error"
            | Trace -> "Trace"
            | Warning -> "Warning"
            | Custom c -> c

let private nextId =Sutil.Helpers.createIdGenerator()

type LogMessage = 
    {
        Id : int
        Time: System.DateTime
        ModelTime : int
        Source : string
        Category : string
        Message : string
        Context : obj option
    }
    static member Create(msg : string) = { Source = ""; Id = nextId(); ModelTime = 0; Category = "-"; Message = msg; Context = None; Time = System.DateTime.Now }
    static member Create(cat : string, msg : string) = { Source = ""; Id = nextId(); ModelTime = 0; Category = cat; Message = msg; Context = None; Time = System.DateTime.Now }
    static member Create(src : string, cat : string, msg : string, ctx : obj) = 
        { 
            Id = nextId()
            Source = src
            ModelTime = 0
            Category = cat
            Message = msg
            Context = if ctx <> null then Some ctx else None
            Time = System.DateTime.Now
        }

let enableSource (source : string) (enabled : bool) =
    sources <- sources.Add(source, enabled)

let enableCategory (cat : string) (enabled : bool) =
    categories <- categories.Add(cat, enabled)

let logmessage (m : LogMessage) =
    if categoryIsEnabled m.Category && sourceIsEnabled m.Source then
        if (m.Message.StartsWith("Error")) then
            Fable.Core.JS.debugger()
        Console.log(m.Category,m.Source,m.Message)

/// Log message with a given source, category, message and context 
let logm (src : string) (cat : string) (msg : string) (ctx : obj) =
    LogMessage.Create(src, cat, msg, ctx) |> logmessage

/// Log a simple string message
let log(s : string) =
    LogMessage.Create(s) |> logmessage
    
/// Log a simple string message with a category (Error, Warning, 
let logc (cat : LogCategory) (s : string) =
    LogMessage.Create(string cat,s) |> logmessage

let inline private  fmt msg args =
    ( args |> Array.append [| msg :> obj |] |> Array.map string |> String.concat " " )

let createWith logm (source : string) =
    {
        new ILog with
            member _.log( arg0, argsN ) =
                logm source "Trace" (fmt arg0 argsN) null
            member _.trace( arg0, argsN ) =
                logm source "Trace" (fmt arg0 argsN) null
            member _.error( arg0, argsN ) =
                logm source "Error" (fmt arg0 argsN) null
            member _.warning( arg0, argsN ) =
                logm source "Warning" (fmt arg0 argsN) null
            member _.info( arg0, argsN ) =
                logm source "Info" (fmt arg0 argsN) null
    }
    
/// Create a logger for a given source
let create (source : string) =
    createWith (fun src cat msg ctx -> if sourceIsEnabled src then logm source cat msg ctx) source
    
let getEnabled() =
    sources |> Map.toArray |> Array.filter snd |> Array.map fst

let getSources() = 
    sources |> Map.toArray |> Array.map fst

let Trace = create "App"

let command (args : string[]) =
    ()