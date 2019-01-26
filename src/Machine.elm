module Machine exposing (..)

import Rectangle2d exposing (Rectangle2d)

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
  , rectangle : Rectangle2d
  , size : SVGSize
  }

emptyMachine : ( Float, Float ) -> EMachine
emptyMachine ( width, height ) =
  let size = Rect.sizeFromTuple ( width, height )
  in
  { inputs = []
  , machineType = TAbs
  , rectangle = Rect.toRectangle2d { position = { x = 0, y = 0 }, size = size }
  , size = size
  }
