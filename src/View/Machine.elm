module View.Machine exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import Dict.Extra as DictE
import Direction2d
import Geometry.Svg as Svg
import LineSegment2d
import Rectangle2d

import Components exposing (..)
import Entity exposing (..)
import Machine exposing (..)
import Pointer exposing (Msg(..))
import Transform exposing (Transform)

drawContour : EMachine -> List (Svg msg)
drawContour machine =
  let
    attributes = [ SA.class "contour" ]
    edges = Rectangle2d.edges machine.rectangle
    ( bottomLeftPoint, bottomRightPoint ) = LineSegment2d.endpoints edges.bottom
    bottomDirection = LineSegment2d.direction edges.bottom |> Maybe.withDefault Direction2d.x
    bottomMidPoint = LineSegment2d.midpoint edges.bottom
    bottomLeftLine = LineSegment2d.from bottomLeftPoint bottomMidPoint
    bottomRightLine = LineSegment2d.from bottomLeftPoint bottomMidPoint
  in
  List.map (Svg.lineSegment2d attributes)
    [edges.bottom, edges.right, edges.top, edges.left, bottomLeftLine, bottomRightLine]


drawBackground machine =
  Rectangle2d.toPolygon machine.rectangle |>
    Svg.polygon2d [ SA.class "background" ]

drawMachine : EntityId -> EMachine -> Svg Msg
drawMachine id machine =
  let
    events =
      [ SE.on "click" <| JD.map (Clicked id) <| Pointer.pageCoordDecoder
      , SE.on "mousemove" <| JD.map (MouseMoved id) <| Pointer.pageCoordDecoder
      , SA.class "machine"
      ]
  in
  Svg.g events (drawBackground machine :: drawContour machine)


drawStrikethrough : EMachine -> Svg msg
drawStrikethrough machine =
  let v = Rectangle2d.vertices machine.rectangle
  in
  Svg.g [ SA.class "strikethrough" ]
    [ Svg.lineSegment2d [] (LineSegment2d.from v.bottomLeft v.topRight)
    , Svg.lineSegment2d [] (LineSegment2d.from v.bottomRight v.topLeft)
    ]


drawEMachine : Components -> EntityId -> EMachine -> Transform -> Svg Msg
drawEMachine components id machine transform =
  let
    groupAttributes =
      (Dict.get id components.svgClasses |> Maybe.map (SA.class >> List.singleton) |> Maybe.withDefault [])
  in
  Svg.placeIn transform.frame
  <| Svg.g groupAttributes
    <| (::) (drawMachine id machine)
    <| (if Set.member id components.invalids then (::) (drawStrikethrough machine) else identity)
    <| drawEMachines components
         (DictE.keepOnly transform.children components.machines)
         components.transforms

drawEMachines : Components -> Dict EntityId EMachine -> Dict EntityId Transform -> List (Svg Msg)
drawEMachines components =
  foldl2
    (\childId childMachine childTransform ->
      (::) (drawEMachine components childId childMachine childTransform)
    ) [ ]

