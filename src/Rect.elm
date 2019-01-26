module Rect exposing (..)

import Svg
import Svg.Attributes as SA
import Geometry.Svg as Svg

import Point2d
import Vector2d
import Rectangle2d
import BoundingBox2d

import Coord exposing (SVGCoord)

type alias SVGRect =
  { position : SVGCoord
  , size : SVGSize
  }

type alias SVGSize =
  { width : Float
  , height : Float
  }

sizeToTuple { width, height } = ( width, height )

sizeFromTuple ( width, height ) = { width = width, height = height }

svgSizeOfDimensions ( width, height ) = SVGSize width height

type alias Rectangular a
  = { a | position : SVGCoord, size : SVGSize }

fromRectangle2d rect =
  { position = Coord.fromPoint2d (Rectangle2d.bottomLeftVertex rect)
  , size = svgSizeOfDimensions (Rectangle2d.dimensions rect)
  }

toRectangle2d { position, size } =
  let pos = Coord.toPoint2d position
      off = Vector2d.fromComponents ( size.width, size.height )
  in Rectangle2d.from pos (Point2d.translateBy off pos)

fromCoords : SVGCoord -> SVGCoord -> SVGRect
fromCoords c1 c2 =
  Rectangle2d.from (Coord.toPoint2d c1) (Coord.toPoint2d c2) |> fromRectangle2d

render : List (Svg.Attribute msg) -> Rectangular a -> Svg.Svg msg
render attributes =
  toRectangle2d >> Rectangle2d.toPolygon >> Svg.polygon2d attributes
