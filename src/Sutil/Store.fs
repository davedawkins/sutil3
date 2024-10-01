module Store 

type 'T observable = System.IObservable<'T>

type Store<'T>( init : 'T ) =
    let mutable value = init
    let mutable clientId = 0
    let mutable clients : Map<int, ('T -> unit)> = Map.empty

    let unsubscribe cid =
        clients <- clients.Remove(cid)

    let notify() = 
        clients.Values |> Seq.iter (fun f -> f value)

    let update( v ) =
        value <- v
        notify()

    member __.Value with get() = value and set(v) = update v

    member __.Subscribe( f ) : System.IDisposable =
        clientId <- clientId + 1
        let cid = clientId
        clients <- clients.Add( cid, f )
        f value
        { new System.IDisposable with member _.Dispose() = unsubscribe cid }

    interface System.IObservable<'T> with
        member __.Subscribe (observer: System.IObserver<'T>): System.IDisposable = 
            __.Subscribe( observer.OnNext )

let make (v : 't) = new Store<'t>(v)
