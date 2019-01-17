module Ctm exposing (..)

import Json.Decode as JD exposing (Decoder)

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

type alias SVGCoord =
  { x : Float
  , y : Float
  }

type alias ClientCoord =
  { x : Int
  , y : Int
  }

svgOfClientCoord : ClientCoord -> SVGCoord
svgOfClientCoord {x, y} = {x = toFloat x, y = toFloat y}

clientCoordDecoder : Decoder ClientCoord
clientCoordDecoder =
  JD.map2 ClientCoord
    (JD.at [ "clientX" ] JD.int)
    (JD.at [ "clientY" ] JD.int)

ctmDecoder : Decoder Ctm
ctmDecoder =
  JD.map6 (\ a b c d e f -> { a = a, b = b, c = c, d = d, e = e, f = f })
    (JD.field "a" JD.float)
    (JD.field "b" JD.float)
    (JD.field "c" JD.float)
    (JD.field "d" JD.float)
    (JD.field "e" JD.float)
    (JD.field "f" JD.float)


{-| See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
-}
matrixTransform : Ctm -> SVGCoord -> SVGCoord
matrixTransform c p =
  { x = c.a * p.x + c.c * p.y + c.e
  , y = c.b * p.x + c.d * p.y + c.f
  }

