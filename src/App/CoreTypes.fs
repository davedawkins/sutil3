module CoreTypes

open Browser.Types

type BuildContext = 
    {
        Parent : Node 
        Existing : Node
    }
    static member Create() : BuildContext = { Existing = null; Parent = null } 
    member __.ParentElement = __.Parent :?> HTMLElement
    member __.WithExisting( node : Node ) = { __ with Existing = null }
    member __.WithParent( node : Node ) = { __ with Parent = node }

type DomEventHandler = Event -> unit

/// New version of SutilElement
/// This DSL can be inspected before committing to real DOM elements
/// 
type SutilElement =
    | Text of string
    | Element of (string * SutilElement[])
    | Attribute of (string * string)
    | Event of (string * DomEventHandler)
    | SideEffect of (BuildContext -> unit)
//      | Fragment of (SutilElement[])

type SutilBuildResult =
    | DomNode of Node
    | DomElement of (HTMLElement * SutilBuildResult[])
    | PatchedNode of Node
    | PatchedElement of (HTMLElement * SutilBuildResult[])
//        | DomFragment of Node[]
    | DomSideEffect