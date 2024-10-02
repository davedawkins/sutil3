module Sutil.CoreTypes

type disposable = System.IDisposable

open Browser.Types

type BuildContext = 
    {
        MakeId : (unit -> int)
        Parent : Node 
        Mount : Node -> Node -> unit
    }
    static member Create() : BuildContext = 
        { 
            Parent = null
            MakeId = Helpers.createIdGenerator()
            Mount = BuildContext.DefaultMount 
        } 

    static member DefaultMount = DomHelpers.append
    
    member __.WithParentId( id : string ) = 
        __.WithParent( Browser.Dom.document.getElementById(id) )

    member __.ParentElement = __.Parent :?> HTMLElement

    member __.WithParent( node : Node ) = { __ with Parent = node }
    member __.WithMount( mount ) = { __ with Mount = mount }

type DomEventHandler = Event -> unit

/// New version of SutilElement
/// This DSL can be inspected before committing to real DOM elements

type SutilElement =
    | Text of string
    | Element of (string * SutilElement[])
    | Attribute of (string * string)
    | Event of (string * DomEventHandler)
    | SideEffect of (string * (BuildContext -> unit))
    | MapElement of (string * (Node -> Node) * SutilElement )
    | Fragment of (SutilElement[])
