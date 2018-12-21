module Machines exposing (..)

import Browser
import Html exposing (Html)
import Test

type Arity = Arity Int

type alias Value = Int

fun2 f =
  ( Arity 2
  , \ args ->
      case args of
        [x, y] -> Just (f x y)
        _ -> Nothing
  )

evalBuiltin : String -> Maybe (Arity, List Value -> Maybe Value)
evalBuiltin s =
  case s of
    "add" -> Just (fun2 (\ x y -> x + y))
    "mul" -> Just (fun2 (\ x y -> x * y))
    _ -> Nothing

type Machine
  = Const Value
  | App (List (Maybe Machine)) Machine
  | Abs Arity Machine
  | Reference String
  | Ghost (Maybe Arity) Machine


allJust : List (Maybe a) -> Maybe (List a)
allJust = List.foldr (Maybe.map2 (::)) (Just [])


combArgs : List (Maybe a) -> List (Maybe a) -> Maybe (List (Maybe a))
combArgs newer older =
  List.foldl (\ maybeX accRem -> accRem |> Maybe.andThen
    (\ (acc, rem) ->
      case maybeX of
        Nothing ->
          case rem of
            r :: rs -> Just (r :: acc, rs)
            [] -> Nothing
        Just x -> Just (Just x :: acc, rem)
    )) (Just ([], newer)) older
  |> Maybe.andThen (\ (acc, rem) ->
    if List.isEmpty rem then Just (List.reverse acc) else Nothing
    )


evalMachine machine =
  case machine of
    Const v -> Just v
    App args (Reference n) ->
      evalBuiltin n |> Maybe.andThen
        (\ (Arity arity, fun) -> allJust args |> Maybe.andThen
          (\ args_ ->
            if List.length args_ == arity
            then allJust (List.map evalMachine args_) |> Maybe.andThen fun
            else Nothing
          )
        )
    App args (Ghost maybeArity m) ->
      evalMachine (App args m)
    App argsNewer (App argsOlder m) ->
      combArgs argsNewer argsOlder |> Maybe.andThen (\ comb ->
      evalMachine (App comb m))
    _ -> Debug.todo "evalM"
      {-
      case evalBuiltin n of
        Nothing -> Nothing
        Just (Arity arity, fun) ->
          case allJust args of
            Nothing -> Nothing
            Just args_ ->
              if List.length args_ == arity
              then
                case allJust (List.map evalMachine args_) of
                  Nothing -> Nothing
                  Just argsE -> fun argsE
              else Nothing
      -}

testMachine1 =
  App
    [ Just
        (App
          [Just (Const 1), Just (Const 1)]
          (Reference "add")
        )
    , Just (Const 2)
    ]
    (Reference "mul")

testMachine2 =
  App [Just (Const 2)]
    <| Ghost Nothing
      <| App [Just (Const 1), Nothing]
        <| Reference "add"

  

type alias Model = ()

type alias Msg = ()

init = ()

view model =
  Html.text (Maybe.withDefault "<error>" (Maybe.map String.fromInt (evalMachine testMachine2)))

update msg model = model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
