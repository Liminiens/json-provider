namespace FSharp.Data.JsonProvider

module internal Logging = 
    open System
    open System.IO
    open System.Threading

    let log =
        #if DEBUG
        let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        let log = Path.Combine(home, "jsoninference_log.txt")
        fun (msg: string) ->
            let message =  
                let time = DateTime.Now.ToString("dd/MM/yyyy HH:mm:ss.fff")
                sprintf "[%s]: %s" time msg

            use mutex = new Mutex(false, "jsoninference_log.txt")
            mutex.WaitOne(TimeSpan.FromSeconds(3.0)) |> ignore
            File.AppendAllLines(log, [message])
            mutex.ReleaseMutex()
        
        #else
        fun (msg: string) -> ()
        #endif

