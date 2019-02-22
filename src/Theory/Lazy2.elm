-- Based on: <http://dev.stephendiehl.com/fun/005_evaluation.html>

import Browser
import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)

type alias VarId = String

type Expr
  = EAbs VarId Expr
  | EApp Expr Expr
  | EVar VarId

type alias ThunkId = Int

type Value =
  VClosure (Thunk -> Cache -> Maybe ( Value, Cache ))

type alias Env = Dict VarId ThunkId
type alias Cache = Array Thunk

type Thunk =
  Thunk (Cache -> Maybe ( Value, Cache ))


updateThunk : ThunkId -> Value -> Cache -> Cache
updateThunk ref v =
  Array.set ref (Thunk (\cache -> Just ( v, cache )))

force : ThunkId -> Cache -> Maybe ( Value, Cache )
force ref cache =
  Array.get ref cache
    |> Maybe.andThen (\(Thunk th) -> th cache)
    |> Maybe.map (\( v, newCache ) -> ( v, updateThunk ref v newCache ))

envInsert x a env =
  Dict.insert x a env

mkClosure : Env -> VarId -> Expr -> (Thunk -> Cache -> Maybe ( Value, Cache ))
mkClosure env var body =
  \thunk cache ->
    let
      thunkId = Array.length cache
      newCache = Array.push thunk cache
    in
      eval (envInsert var thunkId env) body newCache

eval : Env -> Expr -> Cache -> Maybe ( Value, Cache )
eval env ex cache =
  case ex of
    EVar n ->
      Dict.get n env
        |> Maybe.andThen (\th -> force th cache)
  
    EAbs x e -> Just (VClosure (mkClosure env x e), cache)
  
    EApp a b -> 
      eval env a cache
        |> Maybe.andThen
             (\(VClosure c, cache1) ->
               c (Thunk (eval env b)) cache1
             )

init =
  ()

view model =
  1
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
