module FSharp.Data.JsonProvider.Helpers

open System
open System.Text
open System.IO
open System.Globalization

let readStream (encoding: Encoding) (stream: Stream) =    
    use reader = new StreamReader(stream, encoding)
    reader.ReadToEnd()

let tryFirst (parameter: 'T) (actions: list<'T -> 'TResult>) =
    let rec tryExecute actionsToTry lastErrorMsg =
        match actionsToTry with
        | action :: tail ->
            try
                Ok(action parameter)
            with
            | :? Exception as e -> 
                tryExecute tail e.Message
        | [] ->
            Error(lastErrorMsg)
    tryExecute actions String.Empty
    
type BoolParseResult =
    | BoolValue of bool
    | NotBoolValue
    | NullValue

let stringToBool (str: string) = 
    let str = str.ToLower(CultureInfo.InvariantCulture).Trim()
    if String.IsNullOrWhiteSpace(str) then NullValue
    elif str = "false" then BoolValue(false)
    elif str = "true" then BoolValue(true)
    else NotBoolValue