import Browser
import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)

type alias VarId = String

type Term
  = Abs VarId Term
  | App Term Term
  | Var VarId

type alias CacheId = Int

type Value
  = Closure VarId Term Env
  | Expression Term Env CacheId

type alias Env = Dict VarId Value
type alias Cache = Array (Maybe Value)

registerInCache cache =
  ( Array.push Nothing cache
  , Array.length cache
  )

type Error
  = Undefined VarId
  | AppToNonClosure

strict : ( Value, Cache ) -> Result Error ( Value, Cache )
strict ( value, cache ) =
  case value of
    Expression term env cacheId ->
      case Array.get cacheId cache |> Maybe.andThen identity of
        Just cachedValue ->
          Ok ( cachedValue, cache )
        Nothing ->
          interpret term env cache
            |> Result.andThen strict
            |> Result.map
                 (\ ( strictValue, strictCache ) ->
                   ( strictValue, Array.set cacheId (Just strictValue) strictCache )
                 )
    _ -> Ok ( value, cache )


interpret : Term -> Env -> Cache -> Result Error ( Value, Cache )
interpret term env cache =
  let _ = Debug.log "interpret" (term, env)
  in
  case term of
    Var v ->
      Dict.get v env
        |> Result.fromMaybe (Undefined v)
        |> Result.map (\ value -> ( value, cache ))
    Abs v t ->
      Ok ( Closure v t env, cache )
    App funTerm argTerm ->
      case interpret funTerm env cache |> Debug.log "interpret gives" |> Result.andThen strict of
        Ok ( Closure funParam funBody funEnv, cache1 ) ->
          let ( cache2, newCacheId ) = registerInCache cache1
              argValue = Expression argTerm env newCacheId
          in interpret funBody (Dict.insert funParam argValue env) cache2
        _ -> Err AppToNonClosure

identityTerm =
  Abs "x" (Var "x")

abs vars term =
  List.foldr Abs term vars

app term args =
  List.foldl (\ arg acc -> App acc arg) term args

nil =
  abs ["c", "n"] (Var "n")

cons =
  abs ["x", "xs", "c", "n"] (app (Var "c") [Var "x", Var "xs"])

zero =
  abs ["s", "z"] (Var "z")

succ =
  abs ["x", "s", "z"] (App (Var "s") (Var "x"))

zeroes =
  App yCurry (Abs "zeroes" (app cons [zero, Var "zeroes"]))

yCurry =
  let part = Abs "x" (App (Var "f") (App (Var "x") (Var "x")))
  in Abs "f" (App part part)

takeNonrec =
  abs ["take", "n", "l"] <|
    app (Var "n") -- case n of
      [ Abs "n'" <| -- succ(n') ->
          app (Var "l") -- case l of
            [ abs ["x", "xs"] <| -- cons(x, xs) ->
                app cons [Var "x", app (Var "take") [Var "n'", Var "xs"]]
            , nil
            ]
      , nil
      ]

take = App yCurry takeNonrec

takeTest = app take [zero, nil]


init =
  ()

view model =
  interpret (App succ (App succ zero)) Dict.empty Array.empty
    |> Debug.toString
    |> Html.text

update msg model =
  model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
