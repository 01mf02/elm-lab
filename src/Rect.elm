module Rect exposing (..)

import Svg
import Svg.Attributes as SA

import Coord exposing (SVGCoord)

type alias SVGRect =
  { position : SVGCoord
  , size : SVGSize
  }

type alias SVGSize =
  { width : Float
  , height : Float
  }

type alias Rectangular a
  = { a | position : SVGCoord, size : SVGSize }

centerAt : SVGCoord -> SVGSize -> SVGRect
centerAt { x, y } size =
  { position =
      { x = x - size.width/2
      , y = y - size.height/2
      }
  , size = size
  }

coordIn : SVGRect -> SVGCoord -> Bool
coordIn rect coord =
  coord.x > rect.position.x &&
  coord.x < rect.position.x + rect.size.width &&
  coord.y > rect.position.y &&
  coord.y < rect.position.y + rect.size.height

fromCoords : SVGCoord -> SVGCoord -> SVGRect
fromCoords c1 c2 =
  let
    minMax x y =
      if x < y then (x, y) else (y, x)
    (xmin, xmax) = minMax c1.x c2.x
    (ymin, ymax) = minMax c1.y c2.y
  in
  { position = { x = xmin, y = ymin }
  , size = { width = xmax - xmin, height = ymax - ymin }
  }

noOverlap : Rectangular a -> Rectangular b -> Bool
noOverlap bb1 bb2 =
     bb1.position.x + bb1.size.width  < bb2.position.x
  || bb2.position.x + bb2.size.width  < bb1.position.x
  || bb1.position.y + bb1.size.height < bb2.position.y
  || bb2.position.y + bb2.size.height < bb1.position.y

inside : Rectangular a -> Rectangular b -> Bool
inside bbOut bbIn =
  bbOut.position.x < bbIn.position.x &&
  bbOut.position.y < bbIn.position.y &&
  bbOut.position.x + bbOut.size.width  > bbIn.position.x + bbIn.size.width &&
  bbOut.position.y + bbOut.size.height > bbIn.position.y + bbIn.size.height

svgAttributes : Rectangular a -> List (Svg.Attribute msg)
svgAttributes { position, size } =
  [ SA.x (String.fromFloat position.x)
  , SA.y (String.fromFloat position.y)
  , SA.width (String.fromFloat size.width)
  , SA.height (String.fromFloat size.height)
  ]

