module FsJsonNavigation.JsonNavigation

open JsonDom

type [<RequireQualifiedAccess>] JsonPathElement = 
  | ObjectKey   of string
  | ArrayIndex  of int

type JsonPath = JsonPathElement list

type [<RequireQualifiedAccess>] JsonNavigationFailure =
  | NotConvertibleTo        of JsonPath*JsonType*JsonType
  | InvalidNumberValue      of JsonPath
  | MissingProperty         of JsonPath*string
  | IndexOutOfRange         of JsonPath*int*int
  | NotAnArray              of JsonPath*JsonType
  | NotAnObject             of JsonPath*JsonType
  | Group                   of JsonPath*JsonNavigationFailure array

type [<RequireQualifiedAccess>] JsonNavigationResult =
  | Success of JsonPath*Json
  | Failure of JsonNavigationFailure

type JsonNavigateContext = JNC of unit

type JsonNavigator = 
  | JN of (JsonNavigateContext -> JsonPath -> Json -> JsonNavigationResult)

module JsonNavigate  =
  module Details =
    let inline adapt3  (JN f)   = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f
    let inline invoke3 f jc j p = (f : OptimizedClosures.FSharpFunc<_, _, _, _>).Invoke (jc, j, p)
    let inline success jp je    = JsonNavigationResult.Success (jp, je)
    let inline failure f        = JsonNavigationResult.Failure f
  open Details

  let jnroot = JN <| fun jc jp je -> success jp je

  let jnrun je t = 
    let t = adapt3 t
    let jc = JNC ()
    invoke3 t jc [] je

  let jnelement je t =
    match jnrun je t with
    | JsonNavigationResult.Success (_, je)  -> Ok     je
    | JsonNavigationResult.Failure f        -> Error  f

  let jnvalue je m t : Result<'T, _> =
    jnelement je t
    |> Result.map m

  let jnstring    je t = jnvalue je (function Json.String v -> Ok v    | j -> Error (JsonNavigationFailure.NotConvertibleTo ([], j.Type, JsonType.String))) t
  let jnnumber    je t = jnvalue je (function Json.Number v -> Ok v    | j -> Error (JsonNavigationFailure.NotConvertibleTo ([], j.Type, JsonType.Number))) t
  let jnbool      je t = jnvalue je (function Json.Bool   v -> Ok v    | j -> Error (JsonNavigationFailure.NotConvertibleTo ([], j.Type, JsonType.Bool  ))) t

  let jnisNull    je t = jnvalue je (fun j -> not j.IsNotNull ) t
  let jnisNotNull je t = jnvalue je (fun j -> j.IsNotNull     ) t
  let jnisTruthy  je t = jnvalue je (fun j -> j.IsTruthy      ) t
  let jnisFalsy   je t = jnvalue je (fun j -> j.IsFalsy       ) t

  let jnindex idx t = 
    let t = adapt3 t
    JN <| fun jc jp je ->
      match invoke3 t jc jp je with
      | JsonNavigationResult.Failure _ as f -> f
      | JsonNavigationResult.Success (tjp, tje) ->
        match tje with
        | Json.Array vs ->
          let c = vs.Length
          if idx < c then
            let ijp = (JsonPathElement.ArrayIndex idx)::tjp
            let ije = vs.[idx]
            success ijp ije
          else
            JsonNavigationFailure.IndexOutOfRange (tjp, idx, c) |> failure
        | _ ->
          JsonNavigationFailure.NotAnArray (tjp, tje.Type) |> failure

  let jnkey key t = 
    let t = adapt3 t
    JN <| fun jc jp je ->
      match invoke3 t jc jp je with
      | JsonNavigationResult.Failure _ as f -> f
      | JsonNavigationResult.Success (tjp, tje) ->
        match tje with
        | Json.Object kvs ->
          let rec loop i =
            if i < kvs.Length then
              let struct (k, ije) = kvs.[i]
              if k = key then
                let ijp = (JsonPathElement.ObjectKey key)::tjp
                success ijp ije
              else
                loop (i + 1)
            else
              JsonNavigationFailure.MissingProperty (tjp, key) |> failure
          loop 0
        | _ ->
          JsonNavigationFailure.NotAnObject (tjp, tje.Type) |> failure

  let jnpick p t = 
    let p = adapt3 p
    let t = adapt3 t
    JN <| fun jc jp je ->
      match invoke3 t jc jp je with
      | JsonNavigationResult.Failure _ as f -> f
      | JsonNavigationResult.Success (tjp, tje) ->
        match tje with
        | Json.Array vs ->
          let c = vs.Length
          let ra = ResizeArray c
          for i = 0 to c - 1 do
            let ijp = (JsonPathElement.ArrayIndex i)::tjp
            let ije = vs.[i]
            match invoke3 p jc ijp ije with
            | JsonNavigationResult.Success (_, sjt) -> ra.Add sjt
            | JsonNavigationResult.Failure _        -> ()
          success tjp (Json.Array (ra.ToArray ()))
        | _ ->
          JsonNavigationFailure.NotAnArray (tjp, tje.Type) |> failure

  let jnfilter f t = 
    let f = adapt3 f
    let t = adapt3 t
    JN <| fun jc jp je ->
      match invoke3 t jc jp je with
      | JsonNavigationResult.Failure _ as f -> f
      | JsonNavigationResult.Success (tjp, tje) ->
        match tje with
        | Json.Array vs ->
          let c = vs.Length
          let ra = ResizeArray c
          for i = 0 to c - 1 do
            let ijp = (JsonPathElement.ArrayIndex i)::tjp
            let ije = vs.[i]
            match invoke3 f jc ijp ije with
            | JsonNavigationResult.Success (_, sjt) -> if sjt.IsTruthy then ra.Add ije
            | JsonNavigationResult.Failure _        -> ()
          success tjp (Json.Array (ra.ToArray ()))
        | _ ->
          JsonNavigationFailure.NotAnArray (tjp, tje.Type) |> failure

type JsonNavigator with
  member x.Item   idx = JsonNavigate.jnindex  idx x
  member x.Filter f   = JsonNavigate.jnfilter f x
  member x.Pick   p   = JsonNavigate.jnpick   p x
  static member (?) (jn, key) = JsonNavigate.jnkey key jn
