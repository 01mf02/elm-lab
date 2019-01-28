module View.Background exposing (draw)

import Json.Decode as JD
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import Geometry.Svg as Svg
import Point2d
import Rectangle2d

import Entity exposing (EntityId)
import Pointer exposing (Msg(..))

draw : EntityId -> Svg Pointer.Msg
draw id =
  let
    rect = Rectangle2d.from Point2d.origin (Point2d.fromCoordinates ( 800, 600 ))
    attributes =
     [ SE.on "click" <| JD.map (Pointer.Clicked id) <| Pointer.pageCoordDecoder
     , SE.on "mousemove" <| JD.map (Pointer.MouseMoved id) <| Pointer.pageCoordDecoder
     , SA.id "background"
     ]
  in
  Rectangle2d.toPolygon rect |> Svg.polygon2d attributes
