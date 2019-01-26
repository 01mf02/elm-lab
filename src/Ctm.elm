module Ctm exposing (..)

import Json.Decode as JD exposing (Decoder)

import Point2d

import Coord exposing (SVGCoord)

-- Thanks to Markus Laire for his advice on transforming
-- client coordinates to SVG coordinates!
-- <https://discourse.elm-lang.org/t/dispatching-custom-events-only-if-needed/2740>

-- See <https://developer.mozilla.org/en-US/docs/Web/API/SVGGraphicsElement#Methods>
type alias Ctm =
  { a : Float
  , b : Float
  , c : Float
  , d : Float
  , e : Float
  , f : Float
  }

ctmDecoder : Decoder Ctm
ctmDecoder =
  JD.map6 (\ a b c d e f -> { a = a, b = b, c = c, d = d, e = e, f = f })
    (JD.field "a" JD.float)
    (JD.field "b" JD.float)
    (JD.field "c" JD.float)
    (JD.field "d" JD.float)
    (JD.field "e" JD.float)
    (JD.field "f" JD.float)

unit : Ctm
unit =
  { a = 1, b = 0, c = 0
  , d = 1, e = 0, f = 0
  }

{-| See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
-}
matrixTransform : Ctm -> SVGCoord -> SVGCoord
matrixTransform c p =
  let ( x, y ) = Point2d.coordinates (Coord.toPoint2d p)
  in
  { x = c.a * x + c.c * y + c.e
  , y = c.b * x + c.d * y + c.f
  }
