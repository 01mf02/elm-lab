module Machine exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Rectangle2d exposing (Rectangle2d)

import Entity exposing (EntityId)

type alias ConstrName = String

type MachineType
  = TConstr ConstrName
  | TAbs
  | Ghost
  | TReference String

type alias EMachine =
  { inputs : List EntityId
  , connections : Set EntityId
  , machineType : MachineType
  , rectangle : Rectangle2d
  }

emptyMachine : MachineType -> Rectangle2d -> EMachine
emptyMachine typ rectangle =
  { inputs = []
  , connections = Set.empty
  , machineType = typ
  , rectangle = rectangle
  }

type alias Machines a =
  { a | machines : Dict EntityId EMachine }


getMachine : Machines a -> EntityId -> Maybe EMachine
getMachine components id =
  Dict.get id components.machines
