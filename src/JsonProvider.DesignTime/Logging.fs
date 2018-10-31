namespace FSharp.Liminiens.JsonProvider

module Logging = 
    open System
    open System.IO
    open Newtonsoft.Json.Linq

    let log =
        #if DEBUG
        let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        let log = Path.Combine(home, "jsoninference_log.txt")
        fun (msg: string) ->
            let message =  
                let time = DateTime.Now.ToString("dd/MM/yyyy HH:mm:ss.fff")
                sprintf "[%s]: %s" time msg
            File.AppendAllLines(log, [message])
        #else
        fun (msg: string) -> ()
        #endif
    
    let logProperties = 
        #if DEBUG
        fun (properties: JProperty seq) ->
            properties 
            |> Seq.map 
                (fun prop -> sprintf """Property name: "%s" type: "%s" """ prop.Name (prop.Type.ToString()))
            |> Seq.iter log
        #else
        fun (properties: JProperty list) -> ()
        #endif

