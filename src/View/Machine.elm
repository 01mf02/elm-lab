module View.Machine exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import Circle2d
import Dict.Extra as DictE
import Direction2d
import Geometry.Svg as Svg
import LineSegment2d
import Point2d
import Polyline2d
import Rectangle2d
import Vector2d

import Components exposing (..)
import Entity exposing (..)
import Machine exposing (..)
import Pointer exposing (Msg(..))
import Transform exposing (Transform)

untuple : List (List a, List a) -> List (List a)
untuple list =
  let
    aux prev l =
      case l of
        [] -> [prev]
        ( x1, x2 ) :: xs ->
          (prev ++ x1) :: aux x2 xs
  in
  if List.isEmpty list then [] else aux [] list

mapLast : (a -> List a) -> List a -> List a
mapLast fn list =
  case list of
    x :: y :: l -> x :: mapLast fn (y :: l)
    x :: [] -> fn x
    [] -> []

appendHeadLast : List a -> List a -> List (List a) -> List (List a)
appendHeadLast start end list =
  case list of
    [] -> [start ++ end]
    head :: xs -> ((start ++ head) :: xs) |> mapLast (\last -> [last ++ end])

doorVertices mid =
  let
    doorLength = 15
    left = Point2d.translateBy (Vector2d.withLength (-doorLength) Direction2d.x) mid
    right = Point2d.translateBy (Vector2d.withLength (doorLength) Direction2d.x) mid
    leftUp = Point2d.rotateAround left (45) mid
    rightUp = Point2d.rotateAround right (-45) mid
  in
    ( [ left, leftUp ], [ rightUp, right ] )

drawContour : EMachine -> List (Svg msg)
drawContour machine =
  let
    attributes = [ SA.class "contour" ]
    edges = Rectangle2d.edges machine.rectangle
    vertices = Rectangle2d.vertices machine.rectangle

    inputVertices (x, _) =
      LineSegment2d.midpoint edges.bottom
        |> Point2d.translateBy (Vector2d.withLength x Direction2d.x)
        |> doorVertices
    outputVertices =
      LineSegment2d.midpoint edges.top
        |> doorVertices
        |> Tuple.mapBoth List.reverse List.reverse

    left = Tuple.first outputVertices ++ [ vertices.topLeft, vertices.bottomLeft ]
    right = [ vertices.bottomRight, vertices.topRight ] ++ Tuple.second outputVertices
  in
  List.map inputVertices machine.inputs
    |> untuple
    |> appendHeadLast left right
    |> List.map (Polyline2d.fromVertices >> Svg.polyline2d attributes)


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
  Svg.g events (drawBackground machine :: (drawContour machine))


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

