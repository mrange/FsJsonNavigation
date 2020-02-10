module FsJsonNavigation.JsonDom

open System

type [<RequireQualifiedAccess>] JsonType = 
  | Bool    = 0
  | Null    = 1
  | Number  = 2
  | String  = 3
  | Object  = 4
  | Array   = 5

type [<RequireQualifiedAccess>] Json =
  | Bool    of bool
  | Null
  | Number  of float
  | String  of string
  | Object  of struct (string*Json) array
  | Array   of Json array

  member x.Type = 
    match x with
    | Bool    _ -> JsonType.Bool  
    | Null      -> JsonType.Null  
    | Number  _ -> JsonType.Number
    | String  _ -> JsonType.String
    | Object  _ -> JsonType.Object
    | Array   _ -> JsonType.Array 

  member x.IsTruthy = 
    match x with
    | Bool    v -> v
    | Null      -> false
    | Number  v -> not (v = 0. || Double.IsNaN v) 
    | String  s -> s.Length > 0
    | Object  _ 
    | Array   _ -> true

  member x.IsFalsy = not x.IsTruthy

  member x.IsNotNull = 
    match x with
    | Bool    _ -> true
    | Null      -> false
    | Number  _ 
    | String  _ 
    | Object  _ 
    | Array   _ -> true

