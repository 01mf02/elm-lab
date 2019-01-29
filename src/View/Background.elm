module View.Background exposing (draw)

import Json.Decode as JD
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import BoundingBox2d
import Point2d
import Geometry.Svg as Svg

import Entity exposing (EntityId)
import Pointer


draw : EntityId -> Svg Pointer.Msg
draw id =
  let
    attributes =
      [ SE.on "click" <| JD.map (Pointer.Clicked id) <| Pointer.pageCoordDecoder
      , SE.on "mousemove" <| JD.map (Pointer.MouseMoved id) <| Pointer.pageCoordDecoder
      , SA.id "background"
      ]
    box = BoundingBox2d.from Point2d.origin (Point2d.fromCoordinates (800, 600))
  in
  Svg.boundingBox2d attributes box
