module Machine exposing (..)

import Entity exposing (EntityId)
import Rect exposing (SVGSize)

type alias ConstrName = String

type MachineType
  = TConstr ConstrName
  | TAbs
  | TReference String

type alias EMachine =
  { inputs : List (Maybe EntityId)
  , machineType : MachineType
  , size : SVGSize
  }

emptyMachine : SVGSize -> EMachine
emptyMachine size =
  { inputs = []
  , machineType = TAbs
  , size = size
  }
