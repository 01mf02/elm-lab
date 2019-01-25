module Coord exposing (..)

import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)

import Json.Decode as JD exposing (Decoder)

type alias ClientCoord =
  { x : Int
  , y : Int
  }

toPoint2d { x, y } =
  Point2d.fromCoordinates ( x, y )

toVector2d { x, y } =
  Vector2d.fromComponents ( x, y )

fromPoint2d point =
  let ( x, y ) = Point2d.coordinates point
  in { x = x, y = y }

{-
type alias SVGCoord = Point2d
-}
type alias SVGCoord =
  { x : Float
  , y : Float
  }

svgOfClientCoord : ClientCoord -> SVGCoord
svgOfClientCoord { x, y } =
  { x = toFloat x
  , y = toFloat y
  }

combine : (Float -> Float -> Float) -> SVGCoord -> SVGCoord -> SVGCoord
combine fn c1 c2 =
  { x = fn c1.x c2.x
  , y = fn c1.y c2.y
  }

add : SVGCoord -> SVGCoord -> SVGCoord
add = combine (+)

subtract : SVGCoord -> SVGCoord -> SVGCoord
subtract = combine (-)


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

toString : SVGCoord -> String
toString { x, y } =
  "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"

