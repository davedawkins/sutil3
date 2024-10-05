namespace Sutil

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

    static member DefaultMount = Dom.DomEdit.appendLabel "DefaultMount"

    member __.ParentNode = __.Parent
    
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
        static member Define( name : string, f : BuildContext -> EffectResult ) = 
            SutilElement.SideEffect( name, f )

        static member HiddenDiv =
            SutilElement.Element( "div", [|
                SutilElement.Attribute( "style", "display:none" )
            |])

/// <exclude/>
type ICollectionWrapper<'T> =
    abstract member ToList : unit -> List<'T>
    abstract member ToArray : unit -> 'T array
    abstract member Length : int
    abstract member Mapi : (int -> 'T -> 'R) -> ICollectionWrapper<'R>
    abstract member Map : ('T -> 'R) -> ICollectionWrapper<'R>
    abstract member Exists : ('T -> bool) -> bool
    abstract member TryFind : ('T -> bool) -> 'T option
    inherit System.Collections.Generic.IEnumerable<'T>

///  <exclude />
[<AutoOpen>]
module CollectionWrapperExt =

    type private ListW<'T>(list: 'T list) =
        interface ICollectionWrapper<'T> with
            member _.ToList() = list
            member _.ToArray() = list |> Array.ofList
            member _.Length = list.Length
            member _.Mapi(f: (int -> 'T -> 'R)) = upcast ListW((list |> List.mapi f))
            member _.Map(f: ('T -> 'R)) = upcast ListW(list |> List.map f)
            member _.Exists(p: 'T -> bool) = list |> List.exists p
            member _.TryFind(p: 'T -> bool) = list |> List.tryFind p

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                upcast (list |> Seq.ofList).GetEnumerator()

        interface System.Collections.Generic.IEnumerable<'T> with
            member _.GetEnumerator() =
                upcast (list |> Seq.ofList).GetEnumerator()

    type private ArrayW<'T>(a: 'T array) =
        interface ICollectionWrapper<'T> with
            member _.ToList() = a |> List.ofArray
            member _.ToArray() = a
            member _.Length = a.Length
            member _.Mapi(f: (int -> 'T -> 'R)) = upcast ArrayW((a |> Array.mapi f))
            member _.Map(f: ('T -> 'R)) = upcast ArrayW(a |> Array.map f)
            member _.Exists(p: 'T -> bool) = a |> Array.exists p
            member _.TryFind(p: 'T -> bool) = a |> Array.tryFind p

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                upcast (a |> Seq.ofArray).GetEnumerator()

        interface System.Collections.Generic.IEnumerable<'T> with
            member _.GetEnumerator() =
                upcast (a |> Seq.ofArray).GetEnumerator()

    type List<'T> with
        member internal __.ToCollectionWrapper() : ICollectionWrapper<'T> = upcast ListW(__)

    type 'T ``[]`` with
        member internal __.ToCollectionWrapper() : ICollectionWrapper<'T> = upcast ArrayW(__)

///  <exclude />
module internal CollectionWrapper =
    let length (c: ICollectionWrapper<'T>) = c.Length
    let mapi (f: (int -> 'T -> 'R)) (c: ICollectionWrapper<'T>) = c.Mapi f
    let map (f: ('T -> 'R)) (c: ICollectionWrapper<'T>) = c.Map f
    let exists f (c: ICollectionWrapper<'T>) = c.Exists(f)
    let tryFind f (c: ICollectionWrapper<'T>) = c.TryFind(f)
