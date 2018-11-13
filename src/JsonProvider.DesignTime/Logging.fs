namespace FSharp.Data.JsonProvider

module Logging = 
    open System
    open System.IO

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

