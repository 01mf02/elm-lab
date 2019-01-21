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

pageCoordDecoder : Decoder ClientCoord
pageCoordDecoder =
  JD.map2 ClientCoord
    (JD.at [ "pageX" ] JD.int)
    (JD.at [ "pageY" ] JD.int)

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
  { x = c.a * p.x + c.c * p.y + c.e
  , y = c.b * p.x + c.d * p.y + c.f
  }

translationMatrix : SVGCoord -> Ctm
translationMatrix { x, y } =
  { a = 1, b = 0, c = 0
  , d = 1, e = x, f = y
  }

-- See https://www.alanzucconi.com/2016/02/10/tranfsormation-matrix/
multiply m1 m2 =
  { a = m1.a * m2.a + m1.b * m2.d
  , b = m1.a * m2.b + m1.b * m2.e
  , c = m1.a * m2.c + m1.b * m2.f + m1.c
  , d = m1.d * m2.a + m1.e * m2.d
  , e = m1.d * m2.b + m1.e * m2.e
  , f = m1.d * m2.c + m1.e * m2.f + m1.f
  }
