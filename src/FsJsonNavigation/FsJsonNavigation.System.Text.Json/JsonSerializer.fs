module FsJsonNavigation.System.Text.Json.JsonSerializer

open FsJsonNavigation.JsonDom

open System.Text.Json

module Details =
  let inline isEmpty (l : ResizeArray<_>) = l.Count = 0
  let inline pop (l : ResizeArray<_>) = 
    let i = l.Count - 1
    let c = l.[i]
    l.RemoveAt i
    c
  let inline push (l : ResizeArray<_>) v = 
    l.Add v
open Details

let read (reader : Utf8JsonReader) = 

  let keyBuilders   = ResizeArray ()
  let valueBuilders = ResizeArray ()
  let keyFrames     = ResizeArray ()
  let valueFrames   = ResizeArray ()

  let mutable keyFrame    = ResizeArray ()
  let mutable valueFrame  = ResizeArray ()

  while reader.Read () do
    let tkt = reader.TokenType

    match tkt with
    | JsonTokenType.StartObject
    | JsonTokenType.StartArray  ->
      push keyFrames keyFrame
      push valueFrames valueFrame
      keyFrame <-
        if isEmpty keyBuilders then
          ResizeArray ()
        else
          pop keyBuilders
      valueFrame <-
        if isEmpty valueBuilders then
          ResizeArray ()
        else
          pop valueBuilders
    | JsonTokenType.EndObject ->
      if keyFrame.Count <> valueFrame.Count then
        failwithf 
          "Mismatch between propery names and values count when building an object: %d (keys) <> %d (values)" 
          keyFrame.Count 
          valueFrame.Count
      else
        let c = valueFrame.Count
        let kvs = Array.zeroCreate c
        for i = 0 to c - 1 do
          kvs.[i] <- struct (keyFrame.[i], valueFrame.[i])
        keyFrame.Clear ()
        valueFrame.Clear ()
        push keyBuilders keyFrame
        push valueBuilders valueFrame
        keyFrame <- pop keyFrames
        valueFrame <- pop valueFrames
        push valueFrame (Json.Object kvs)
    | JsonTokenType.EndArray -> 
      if keyFrame.Count <> 0 then
        failwithf 
          "No property names expected when building an array: %d (keys) <> 0" 
          keyFrame.Count 
      else
        let vs = valueFrame.ToArray ()
        // Not needed, it is of 0 size
        // keyFrame.Clear () 
        valueFrame.Clear ()
        push keyBuilders keyFrame
        push valueBuilders valueFrame
        keyFrame <- pop keyFrames
        valueFrame <- pop valueFrames
        push valueFrame (Json.Array vs)
    | JsonTokenType.PropertyName ->
      let v = reader.GetString ()
      let v = if isNull v then "" else v
      push keyFrame v
    | JsonTokenType.Number ->
      let v = reader.GetDouble ()
      push valueFrame (v |> Json.Number)
    | JsonTokenType.String ->
      let v = reader.GetString ()
      let v = if isNull v then "" else v
      push valueFrame (v |> Json.String)
    | JsonTokenType.True -> 
      push valueFrame (true |> Json.Bool)
    | JsonTokenType.False -> 
      push valueFrame (false |> Json.Bool)
    | JsonTokenType.Null ->
      push valueFrame Json.Null
    | JsonTokenType.Comment ->
      // Skip
      ()
    | _ ->
      failwithf "Encountered unsupported token type: %A" tkt

  if keyFrames.Count = 0 && valueFrames.Count = 0 && keyFrame.Count = 0 && valueFrame.Count = 1 then
    valueFrame.[0]
  else
    failwithf
      "Failed to construct Json object due to failed invariant: keyFrames (%d) = 0 && valueFrames (%d) = 0 && keyFrame (%d) = 0 && valueFrame (%d) = 1"
      keyFrames.Count
      valueFrames.Count
      keyFrame.Count
      valueFrame.Count

let rec write (writer : Utf8JsonWriter) (json : Json) =
  match json with
  | Json.Bool   v   -> writer.WriteBooleanValue v
  | Json.Null       -> writer.WriteNullValue ()
  | Json.Number v   -> writer.WriteNumberValue v
  | Json.String v   -> writer.WriteStringValue v
  | Json.Object kvs ->
    writer.WriteStartObject ()
    for struct (k, v) in kvs do 
      writer.WritePropertyName k
      write writer v
    writer.WriteEndObject ()
  | Json.Array vs ->
    writer.WriteStartArray ()
    for v in vs do write writer v
    writer.WriteEndArray ()

