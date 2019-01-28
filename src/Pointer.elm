module Pointer exposing (..)

import Json.Decode as JD exposing (Decoder)

import Coord exposing (ClientCoord)
import Entity exposing (EntityId)

type Msg
  = MouseMoved EntityId Coord.ClientCoord
  | Clicked EntityId Coord.ClientCoord

clientCoordDecoder : Decoder ClientCoord
clientCoordDecoder =
  JD.map2 ClientCoord
    (JD.at [ "clientX" ] JD.int)
    (JD.at [ "clientY" ] JD.int)

pageCoordDecoder : Decoder ClientCoord
pageCoordDecoder =
  JD.map2 ClientCoord
    (JD.at [ "pageX" ] JD.int)
    (JD.at [ "pageY" ] JD.int)

svgOfClientCoord : Coord.ClientCoord -> ( Float, Float )
svgOfClientCoord { x, y } =
  ( toFloat x, toFloat y )
