module CoreTypes

type disposable = System.IDisposable

open Browser.Types

type BuildContext = 
    {
        MakeId : (unit -> int)
        Parent : Node 
        //CreateElement : string -> HTMLElement
    }
    static member Create() : BuildContext = { Parent = null; MakeId = Helpers.createIdGenerator() } 
    member __.ParentElement = __.Parent :?> HTMLElement
    member __.WithParent( node : Node ) = { __ with Parent = node }

type DomEventHandler = Event -> unit

/// New version of SutilElement
/// This DSL can be inspected before committing to real DOM elements

type SutilElement =
    | Text of string
    | Element of (string * SutilElement[])
    | Attribute of (string * string)
    | Event of (string * DomEventHandler)
    | SideEffect of (BuildContext -> unit)
    | MapElement of ( (Node -> Node) * SutilElement )
    | Fragment of (SutilElement[])
