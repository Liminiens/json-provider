namespace FSharp.Data.JsonProvider

open System.Collections.Concurrent
open System.Timers
open System

type CacheItem = 
    { Value: string; Added: DateTime; Duration: TimeSpan }
    member __.IsValid() =
        if __.Added.Add(__.Duration) > DateTime.Now then
                true
            else
                false

[<AbstractClass; Sealed>]
type Cache private () = 
    static do 
       Cache.Timer <- new Timer(2000.0)
       Cache.Timer.AutoReset <- true
       Cache.Timer.Enabled <- true
       Cache.Timer.Elapsed |> Event.add (fun args -> Cache.Check())
       Logging.log "Setting up timer"

    static member val private Timer: Timer = null with get, set
        
    static member private Store = new ConcurrentDictionary<unit -> string, CacheItem>()

    static member private Invalidate(item: string): unit =
        Logging.log <| sprintf "Invalidating item %A" item
        Cache.Store.TryRemove(item) |> ignore

    static member private Check() = 
        Logging.log "Checking cache" 
        Cache.Store.Values 
        |> Seq.where (fun value -> value.IsValid())
        |> Seq.iter (fun value -> Cache.Invalidate(value.Value))

    static member Add(source: unit -> string, duration: TimeSpan) : string =
        let (success, value) = Cache.Store.TryGetValue(source)
        if success then
            Logging.log <| sprintf "Returning cached value %A" value.Value
            value.Value
        else
            let result = source()
            let isAdded = Cache.Store.TryAdd(source, { Value = result; Added = DateTime.Now; Duration = duration} )
            if not isAdded then
                Logging.log <| sprintf "Failed to add %A" result
                Cache.Add(source, duration)
            else    
                Logging.log <| sprintf "Added to cache %A" result
                result


