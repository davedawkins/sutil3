module Sutil.CoreTypes

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

type EffectResult =
    | CreatedNode of Node
    | EffectedNode of Node
    with 
        member __.Node = 
            match __ with CreatedNode node -> node | EffectedNode node -> node

type SideEffect = string * (BuildContext -> EffectResult)

/// New version of SutilElement
/// This DSL can be inspected before committing to real DOM elements

type SutilElement =
    | Text of string
    | Element of (string * SutilElement[])
    | Attribute of (string * string)
    | Event of (string * DomEventHandler)
    | SideEffect of SideEffect
    | Fragment of (SutilElement[])

    with    
        static member HiddenDiv =
            SutilElement.Element( "div", [|
                SutilElement.Attribute( "style", "display:none" )
            |])