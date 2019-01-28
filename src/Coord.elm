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


fromPoint2d point =
  let ( x, y ) = Point2d.coordinates point
  in { x = x, y = y }

toVector2d { x, y } =
  Vector2d.fromComponents ( x, y )

fromVector2d vec =
  let ( x, y ) = Vector2d.components vec
  in { x = x, y = y }

{-
type alias SVGCoord = Point2d
-}
type alias SVGCoord =
  { x : Float
  , y : Float
  }
