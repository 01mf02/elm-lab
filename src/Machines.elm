module Machines exposing (..)

import Browser
import Html exposing (Html)
import Test

type Arity = Arity Int

type Value
  = IntValue Int
  | BoolValue Bool

stringFromValue v =
  case v of
    IntValue i -> String.fromInt i
    BoolValue True -> "true"
    BoolValue False -> "false"

boolfun2 f =
  ( Arity 2
  , \ args ->
      case args of
        [BoolValue x, BoolValue y] -> Just (BoolValue (f x y))
        _ -> Nothing
  )

intfun2 f =
  ( Arity 2
  , \ args ->
      case args of
        [IntValue x, IntValue y] -> Just (IntValue (f x y))
        _ -> Nothing
  )

evalBuiltin : String -> Maybe (Arity, List Value -> Maybe Value)
evalBuiltin s =
  case s of
    "add" -> Just (intfun2 (+))
    "mul" -> Just (intfun2 (*))
    "and" -> Just (boolfun2 (&&))
    "or" -> Just (boolfun2 (||))
    _ -> Nothing

type Machine
  = Const Value
  | App (List (Maybe Machine)) Machine
  | Abs Arity Machine
  | Reference String
  | Ghost (Maybe Arity) Machine
  | Var Int
  | Case


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


listNth : Int -> List a -> Maybe a
listNth n l =
  if n == 0 then List.head l
  else List.tail l |> Maybe.andThen (listNth (n-1))

substituteArgs : List (Maybe Machine) -> List (Maybe Machine) -> List (Maybe Machine)
substituteArgs args =
  List.map (Maybe.andThen (substituteMachine args))

substituteMachine : List (Maybe Machine) -> Machine -> Maybe Machine
substituteMachine args machine =
  case machine of
    Var v -> listNth v args |> Maybe.andThen identity
    App appArgs m ->
      substituteMachine args m
        |> Maybe.map (App (substituteArgs args appArgs))
    Ghost maybeArity m -> substituteMachine args m
    Const v -> Just (Const v)
    Reference s -> Just (Reference s)
    Abs arity m -> Just (Abs arity m)
    Case -> Just Case


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
    App args (Abs arity m) ->
      substituteMachine args m |> Maybe.andThen evalMachine
    App args Case ->
      case args of
        [Just bool, Just trueCase, Just falseCase] ->
          evalMachine bool |> Maybe.andThen (\ v -> case v of
            BoolValue True -> evalMachine trueCase
            BoolValue False -> evalMachine falseCase
            _ -> Nothing
            )
        _ -> Debug.todo "case"

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

zero  = Const (IntValue 0)
one   = Const (IntValue 1)
two   = Const (IntValue 2)
true  = Const (BoolValue True)
false = Const (BoolValue False)

testMachine1 =
  App
    [ Just
        (App
          [Just one, Just one]
          (Reference "add")
        )
    , Just two
    ]
    (Reference "mul")

testMachine2 =
  App [Just two]
    <| Ghost Nothing
      <| App [Just one, Nothing]
        <| Reference "add"

idMachine =
  Abs (Arity 1) (Var 0)

testMachine3 =
  App [Just two] idMachine

testMachine4 =
  App [Just true, Just false] (Reference "and")

testMachine5 =
  App [Just testMachine4, Just testMachine1, Just testMachine2] Case


  

type alias Model = ()

type alias Msg = ()

init = ()

view model =
  Html.text (Maybe.withDefault "<error>" (Maybe.map stringFromValue (evalMachine testMachine5)))

update msg model = model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
