module Pointer exposing (..)

import Json.Decode as JD exposing (Decoder)

import Entity exposing (EntityId)

type alias Coord = ( Int, Int )

type Msg
  = MouseMoved EntityId Coord
  | Clicked EntityId Coord

clientCoordDecoder : Decoder Coord
clientCoordDecoder =
  JD.map2 Tuple.pair
    (JD.at [ "clientX" ] JD.int)
    (JD.at [ "clientY" ] JD.int)

pageCoordDecoder : Decoder Coord
pageCoordDecoder =
  JD.map2 Tuple.pair
    (JD.at [ "pageX" ] JD.int)
    (JD.at [ "pageY" ] JD.int)

svgOfClientCoord : Coord -> ( Float, Float )
svgOfClientCoord ( x, y ) =
  ( toFloat x, toFloat y )
