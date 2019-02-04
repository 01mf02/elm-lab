module Connection exposing (..)

import Dict exposing (Dict)
import Set

import Maybe.Extra as MaybeE

import Entity exposing (EntityId)
import Input exposing (Inputs)
import Machine exposing (Machines, EMachine)
import Transform exposing (Transforms)

type Type = Input | Output

{-
if from is Input, then it is the input of the outer machine
if from is Output, it is the output of an inner machine
if to is Input, then it is the input of an inner machine
if to is Output, then it is the output of the outer machine
-}

type alias Endpoint =
  { id : EntityId
  , typ : Type
  }

type alias Connection =
  { from : Endpoint
  , to : Endpoint
  , machine : EntityId
  }

type alias Connections a =
  { a | connections : Dict EntityId Connection }

getConnection : Connections a -> EntityId -> Maybe Connection
getConnection components id =
  Dict.get id components.connections

isValidEndpoint : Machines (Inputs a) -> EntityId -> Bool
isValidEndpoint components id =
  (MaybeE.isJust (Machine.getMachine components id))
    || (MaybeE.isJust (Input.getInput components id))

machineHasConnectionTo : Connections a -> EntityId -> EMachine -> Bool
machineHasConnectionTo components sinkId =
  .connections
    >> Set.toList
    >> List.filterMap (getConnection components)
    >> List.any (.to >> .id >> (==) sinkId)

-- get machine potentially containing connections to given endpoint
getMachineWithConnectionsTo : Inputs (Transforms (Machines a)) -> Endpoint -> Maybe EMachine
getMachineWithConnectionsTo components to =
  case to.typ of
    Input ->
      to.id
        |> Input.getParent components
        |> Maybe.andThen (Transform.getParent components)
        |> Maybe.andThen (Machine.getMachine components)
    Output ->
      to.id |> Machine.getMachine components

hasUnconnectedSink : Machines (Transforms (Inputs (Connections a))) -> Connection -> Bool
hasUnconnectedSink components connection =
  getMachineWithConnectionsTo components connection.to
    |> Maybe.map (not << machineHasConnectionTo components connection.to.id)
    |> Maybe.withDefault True

from : Connections (Machines (Transforms (Inputs a))) -> EntityId -> EntityId -> Maybe Connection
from components id1 id2 =
  orientSourceSink components id1 id2
    |> MaybeE.filter (hasUnconnectedSink components)

orientSourceSink : Machines (Transforms (Inputs a)) -> EntityId -> EntityId -> Maybe Connection
orientSourceSink components id1 id2 =
  let
    isMachine = Machine.getMachine components >> MaybeE.isJust
    contains = Transform.isParentOf components
  in
  -- TODO: refactor!
  case ( isMachine id1, isMachine id2 ) of
    -- both machine outputs
    ( True, True ) ->
      if contains id1 id2
      then Just { from = { id = id1, typ = Output }
                , to = { id = id2, typ = Output }
                , machine = id2
                }
      else
        if contains id2 id1
        then Just { from = { id = id2, typ = Output }
                  , to = { id = id1, typ = Output }
                  , machine = id1
                  }
        else Nothing
    -- both machine inputs
    ( False, False ) ->
      Maybe.map2
        (\machine1 machine2 ->
          if contains machine1 machine2
          then Just { from = { id = id2, typ = Input }
                    , to = { id = id1, typ = Input }
                    , machine = machine2
                    }
          else
            if contains machine2 machine1
            then Just { from = { id = id1, typ = Input }
                      , to = { id = id2, typ = Input }
                      , machine = machine1
                      }
            else Nothing
        )
        (Input.getParent components id1)
        (Input.getParent components id2)
        |> MaybeE.join
    -- first is machine output, second is input
    ( True, False ) ->
      Maybe.map2
        (\parent1 parent2 ->
          if parent2 == id1
          then Just { from = { id = id2, typ = Input }
                    , to = { id = id1, typ = Output }
                    , machine = parent2
                    }
          else
            if contains parent2 parent1
            then Just { from = { id = id1, typ = Output }
                      , to = { id = id2, typ = Input }
                      , machine = parent1
                      }
            else Nothing
        )
        (Transform.getParent components id1)
        (Input.getParent components id2)
        |> MaybeE.join
    ( False, True ) -> orientSourceSink components id2 id1
