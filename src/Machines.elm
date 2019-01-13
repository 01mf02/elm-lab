module Machines exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Test
import Graph exposing (Graph)

import Typecheck exposing (..)
import Machine exposing (..)
import Builtin exposing (..)
import Type exposing (..)




graphFromDependencies : List (comparable, List comparable) -> Graph comparable ()
graphFromDependencies l =
  let
    nodes = List.map Tuple.first l
    dict  = List.indexedMap (\ i x -> (x, i)) nodes |> Dict.fromList
    edges = List.concatMap
      (\ (node, refs) ->
        List.filterMap
          (\ ref ->
            let n = Dict.get node dict
                r = Dict.get ref  dict
            in Maybe.map2 Tuple.pair n r
          ) refs
      ) l
  in Graph.fromNodeLabelsAndEdgePairs nodes edges

graphSCCs : Graph n e -> List (List n)
graphSCCs graph =
  case Graph.stronglyConnectedComponents graph of
    Ok acyclic ->
      Graph.nodes graph |> List.map (.label >> List.singleton)
    Err components ->
      List.map (Graph.nodes >> List.map .label) components

machineSCCs : Dict MachineName Machine -> List (List (MachineName, Machine))
machineSCCs machines =
  machineGraph machines
  |> graphFromDependencies
  |> graphSCCs
  |> List.map (List.filterMap (\ name -> Dict.get name machines |> Maybe.map (Tuple.pair name)))


idMachine =
  Abs (Arity 1) (Var 0)

constMachine =
  Abs (Arity 2) (Var 0)

notMachine =
  Abs (Arity 1)
    <| App [Var 0, Constr "false", Constr "true"]
      <| Case "bool"

andMachine =
  Abs (Arity 2)
    <| App [Var 0, App [Var 1, true, false] (Case "bool"), false]
      <| Case "bool"

yCombinator =
  Abs (Arity 1)
    <| App
         [App [Var 0] (Reference "Y")]
         (Var 0)

repeatMachine =
  Abs (Arity 1)
    <| App [Var 0, App [Var 0] (Reference "repeat")]
     <| Constr "cons"

takeMachine =
  Abs (Arity 2)
    <| App
      [ App [zero, Var 0] <| Reference "="
      , Constr "nil"
      , App
          [ Var 1
          , Constr "nil"
          , App [Var 0]
              <| Abs (Arity 3)
                <| App
                  [ Var 1
                  , App
                      [ App
                          [ Var 0
                          , one
                          ]
                          <| Reference "sub"
                      , Var 2
                      ]
                      <| Reference "take"
                  ]
                  <| Constr "cons"
          ]
          <| Case "list"
      ]
      <| Case "bool"

defaultCtx =
  { machines = Dict.fromList defaultMachines
  , types = Dict.fromList defaultTypes
  }

defaultMachines =
  [ ("not", notMachine)
  , ("and", andMachine)
  , ("id", idMachine)
  , ("Y", yCombinator)
  , ("repeat", repeatMachine)
  , ("take", takeMachine)
  ]


zero  = Const (IntValue 0)
one   = Const (IntValue 1)
two   = Const (IntValue 2)
true  = Constr "true"
false = Constr "false"

intTy = TyConst "int" []

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
    <| App [one]
      <| Reference "add"

testMachine3 =
  App [two] idMachine

testMachine4 =
  App [true, false] (Reference "and")

testMachine5 =
  App [testMachine4, testMachine1, testMachine2] <| Case "bool"

testMachine6 = App [one, (Constr "nil")] (Constr "cons")

testMachine7 = App [testMachine4] (Reference "not")

alwaysTrueMachineY =
  Abs (Arity 2)
    <| App
      [ (Var 1)
      , (Constr "true")
      , (App [(Constr "true")] (Var 0))
      ]
      <| Case "bool"

testMachine8 =
  App [alwaysTrueMachineY, Constr "false"] (Reference "Y")

testMachine9 =
  App [two, App [one] <| Reference "repeat"] (Reference "take")

  

type alias Model = ()

type alias Msg = ()

init = ()

view model =
  testMachine9
    |> evaluateNF defaultCtx
    |> Result.map stringFromMachine
    |> Debug.toString
    |> Html.text

compactAndMachine =
  Abs (Arity 2)
    <| App [Var 0, Var 1, false]
      <| Case "bool"


fullTypecheckContext =
  defaultCtx.machines
  |> machineSCCs
  |> List.foldl (\ machines -> Result.andThen (algWRecursive machines)) (Ok defaultTypecheckContext)


{-
view model =
  fullTypecheckContext
    |> Result.andThen
      (\ ctx -> algW ctx emptyVarMap initFreshGen testMachine9)
    |> Result.map (\ (acc, typ) -> typeToString typ)
    |> Debug.toString
    |> Html.text
-}

update msg model = model

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
