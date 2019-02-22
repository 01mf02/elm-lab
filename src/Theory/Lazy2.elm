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
  | EFix Expr
  | EInt Int

type alias ThunkId = Int

type Value
  = VClosure Env VarId Expr
  | VInt Int

valueToString value =
  case value of
    VInt n -> String.fromInt n
    VClosure _ _ _ -> "<<closure>>"

type alias Env = Dict VarId ThunkId
type alias Cache = Array Thunk

type Thunk
  = TValue Value
  | TPending Env Expr

updateThunk : ThunkId -> ( Value, Cache ) -> ( Value, Cache )
updateThunk ref ( v, cache ) =
  ( v, Array.set ref (TValue v) cache )

force : ThunkId -> Cache -> Maybe ( Value, Cache )
force ref cache =
  Array.get ref cache
    |> Maybe.andThen
      (\thunk ->
        case thunk of
          TValue v -> Just ( v, cache )
          TPending env expr ->
            eval env expr cache
              |> Maybe.map (updateThunk ref)
      )

envInsert x a env =
  Dict.insert x a env

eval : Env -> Expr -> Cache -> Maybe ( Value, Cache )
eval env ex cache =
  case ex of
    EVar n ->
      Dict.get n env
        |> Maybe.andThen (\th -> force th cache)
  
    EAbs x e -> Just (VClosure env x e, cache)
  
    EApp a b -> 
      case eval env a cache of
        Just (VClosure closEnv closVar closExpr, cache1) ->
          let
            thunk = TPending env b
            thunkId = Array.length cache1
            newCache = Array.push thunk cache1
          in
            eval (envInsert closVar thunkId closEnv) closExpr newCache

        _ -> Nothing

    EFix e -> eval env (EApp e (EFix e)) cache
    EInt n -> Just (VInt n, cache)


const = EAbs "x" (EAbs "y" (EVar "x"))

diverge = EFix (EAbs "x" (EApp (EVar "x") (EVar "x")))

-- omega = (\x -> x x) (\x -> x x)
omega = EApp (EAbs "x" (EApp (EVar "x") (EVar "x")))
             (EAbs "x" (EApp (EVar "x") (EVar "x")))

-- test1 = (\y -> 42) omega
test1 = EApp (EAbs "y" (EInt 42)) omega

test2 = EApp (EApp const (EInt 42)) omega

init =
  ()

view model =
  eval Dict.empty test2 Array.empty
    |> Maybe.map (Tuple.first >> valueToString)
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
