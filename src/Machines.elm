module Machines exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Test

type Arity = Arity Int

type Value
  = IntValue Int

stringFromValue v =
  case v of
    IntValue i -> String.fromInt i

intfun2 f =
  ( Arity 2
  , \ args ->
      case args of
        [IntValue x, IntValue y] -> Just (IntValue (f x y))
        _ -> Nothing
  )

builtinFunctions =
  List.foldl (\ (name, fun) -> Dict.insert name fun) Dict.empty
  [ ("add", intfun2 (+))
  , ("mul", intfun2 (*))
  ]

type Machine
  = Const Value
  | App (List Machine) Machine
  | Abs Arity Machine
  | Reference String
  | Ghost (Maybe Arity) Machine
  | Var Int
  | Case
  | Constr String

getConst machine =
  case machine of
    Const v -> Just v
    _ -> Nothing

stringFromMachine : Machine -> Maybe String
stringFromMachine m =
  case m of
    Const v -> Just (stringFromValue v)
    App args (Constr c) ->
      let
        stringArgs =
          List.map (stringFromMachine >> Maybe.withDefault "<unhandled>") args
      in Just (c ++ "(" ++ String.join ", " stringArgs ++ ")")
    Constr c -> Just c
    _ -> Debug.todo "stringFromMachine"


allJust : List (Maybe a) -> Maybe (List a)
allJust = List.foldr (Maybe.map2 (::)) (Just [])


substituteMachine : Machine -> Machine -> Machine
substituteMachine arg machine =
  case machine of
    Var v ->
      if v == 0
      then arg
      else Var (v - 1)
    App args m ->
      substituteMachine arg m |> App (List.map (substituteMachine arg) args)
    Ghost maybeArity m -> substituteMachine arg m
    Const v -> Const v
    Reference s -> Reference s
    Abs arity m -> Abs arity m
    Case -> Case
    Constr c -> Constr c

type alias Context =
  { machines : Dict String Machine
  }

idMachine =
  Abs (Arity 1) (Var 0)

notMachine =
  Abs (Arity 1)
    <| App [Var 0, Constr "false", Constr "true"]
      <| Case

andMachine =
  Abs (Arity 2)
    <| App [Var 0, App [Var 1, true, false] Case, false]
      <| Case

yCombinator =
  Abs (Arity 1)
    <| App
         [App [Var 0] (Reference "Y")]
         (Ghost Nothing (Var 0))

defaultCtx =
  { machines = defaultMachines
  }

defaultMachines =
  List.foldl (\ (name, machine) -> Dict.insert name machine) Dict.empty
  [ ("not", notMachine)
  , ("and", andMachine)
  , ("id", idMachine)
  , ("Y", yCombinator)
  ]


evalBuiltin : String -> List Value -> Maybe Value
evalBuiltin name args =
  Dict.get name builtinFunctions
    |> Maybe.andThen
      (\ (Arity arity, fun) ->
        if List.length args == arity
        then fun args
        else Nothing
      )

evalMachine : Context -> Machine -> Maybe Machine
evalMachine ctx machine =
  case machine of
    Const v -> Just (Const v)
    App args (Reference n) ->
      case Dict.get n ctx.machines of
        Just m -> evalMachine ctx (App args m)
        Nothing ->
          args
            |> List.map (evalMachine ctx >> Maybe.andThen getConst)
            |> allJust
            |> Maybe.andThen (evalBuiltin n)
            |> Maybe.map Const
    App args (Ghost maybeArity m) ->
      evalMachine ctx (App args m)
    App argsNewer (App argsOlder m) ->
      evalMachine ctx (App (argsOlder ++ argsNewer) m)
    App (arg :: args) (Abs (Arity arity) m) ->
      if arity == 0 then
        evalMachine ctx (App (arg :: args) m)
      else
        substituteMachine arg m |> Abs (Arity (arity - 1)) |> App args |> evalMachine ctx
    App [] m -> evalMachine ctx m
    Abs (Arity 0) m -> evalMachine ctx m
    App args Case ->
      case args of
        [bool, trueCase, falseCase] ->
          evalMachine ctx bool |> Maybe.andThen (\ v -> case v of
            Constr "true" -> evalMachine ctx trueCase
            Constr "false" -> evalMachine ctx falseCase
            _ -> Nothing
            )
        _ -> Debug.todo ("case: " ++ Debug.toString args)
    Case -> Nothing
    Constr c -> Just (Constr c)
    App args (Constr c) ->
      args
        |> List.map (evalMachine ctx)
        |> allJust
        |> Maybe.map (\ evalArgs -> App evalArgs (Constr c))

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
true  = Constr "true"
false = Constr "false"

testMachine1 =
  App
    [
        (App
          [one, one]
          (Reference "add")
        )
    , two
    ]
    (Reference "mul")

testMachine2 =
  App [two]
    <| Ghost Nothing
      <| App [one]
        <| Reference "add"

testMachine3 =
  App [two] idMachine

testMachine4 =
  App [true, false] (Reference "and")

testMachine5 =
  App [testMachine4, testMachine1, testMachine2] Case

testMachine6 = App [one, (Constr "nil")] (Constr "cons")

testMachine7 = App [testMachine4] (Reference "not")

alwaysTrueMachineY =
  Abs (Arity 2)
    <| App
      [ (Var 1)
      , (Constr "true")
      , (App [(Constr "true")] (Var 0))
      ]
      Case

testMachine8 =
  App [alwaysTrueMachineY, Constr "false"] (Reference "Y")


  

type alias Model = ()

type alias Msg = ()

init = ()

view model =
  testMachine8
    |> evalMachine defaultCtx
    |> Maybe.andThen stringFromMachine
    |> Maybe.withDefault "<error>"
    |> Html.text

update msg model = model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
