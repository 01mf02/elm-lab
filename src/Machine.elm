module Machine exposing (..)

import Rectangle2d exposing (Rectangle2d)

import Entity exposing (EntityId)

type alias ConstrName = String

type MachineType
  = TConstr ConstrName
  | TAbs
  | TReference String

type alias EMachine =
  { inputs : List (Maybe EntityId)
  , machineType : MachineType
  , rectangle : Rectangle2d
  }

emptyMachine : Rectangle2d -> EMachine
emptyMachine rectangle =
  { inputs = []
  , machineType = TAbs
  , rectangle = rectangle
  }
