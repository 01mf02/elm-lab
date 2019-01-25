module Pointer exposing (..)

import Coord
import Entity exposing (EntityId)

type Msg
  = MouseMoved EntityId Coord.ClientCoord
  | Clicked EntityId Coord.ClientCoord
