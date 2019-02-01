module Machine exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Rectangle2d exposing (Rectangle2d)

import Entity exposing (EntityId)

type alias ConstrName = String

type MachineType
  = TConstr ConstrName
  | TAbs
  | TReference String

type alias EMachine =
  { inputs : List EntityId
  , connections : Set EntityId
  , machineType : MachineType
  , rectangle : Rectangle2d
  }

emptyMachine : Rectangle2d -> EMachine
emptyMachine rectangle =
  { inputs = []
  , connections = Set.empty
  , machineType = TAbs
  , rectangle = rectangle
  }

type alias Machines a =
  { a | machines : Dict EntityId EMachine }


getMachine : Machines a -> EntityId -> Maybe EMachine
getMachine components id =
  Dict.get id components.machines
