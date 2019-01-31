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
import Point2d exposing (Point2d)
import Polygon2d
import Polyline2d
import Rectangle2d
import Vector2d

import Components exposing (..)
import Entity exposing (..)
import Input exposing (Input)
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

type alias InputSegment = (List Point2d, List Point2d)
type alias OutputSegment = InputSegment

inputOutputSegments : EMachine -> ( Input -> InputSegment, OutputSegment )
inputOutputSegments machine =
  let
    edges = Rectangle2d.edges machine.rectangle
    inputVertices { position } =
      LineSegment2d.midpoint edges.bottom
        |> Point2d.translateBy (Vector2d.withLength position Direction2d.x)
        |> doorVertices
    outputVertices =
      LineSegment2d.midpoint edges.top
        |> doorVertices
        |> Tuple.mapBoth List.reverse List.reverse
  in
  ( inputVertices, outputVertices )

contourSegments : Components -> EMachine -> List (List Point2d)
contourSegments components machine =
  let
    vertices = Rectangle2d.vertices machine.rectangle
    ( inputVertices, outputVertices ) = inputOutputSegments machine

    left = Tuple.first outputVertices ++ [ vertices.topLeft, vertices.bottomLeft ]
    right = [ vertices.bottomRight, vertices.topRight ] ++ Tuple.second outputVertices
  in
  machine.inputs
    |> List.filterMap (\inputId -> Dict.get inputId components.inputs)
    |> List.sortBy .position
    |> List.map inputVertices
    |> untuple
    |> appendHeadLast left right

drawContour : Components -> EMachine -> List (Svg msg)
drawContour components machine =
  contourSegments components machine
    |> List.map (Polyline2d.fromVertices >> Svg.polyline2d [ SA.class "contour" ])

drawBackground : Components -> EMachine -> Svg msg
drawBackground components machine =
  contourSegments components machine
    |> List.concat
    |> Polygon2d.singleLoop
    |> Svg.polygon2d [ SA.class "background" ]

drawInput : EntityId -> InputSegment -> Svg Msg
drawInput id ( left, right ) =
  let
    attributes =
      [ SA.class "input"
      , SE.on "click" <| JD.map (Clicked id) <| Pointer.pageCoordDecoder
      ]
    vertices = left ++ right
  in
  Polygon2d.singleLoop vertices |> Svg.polygon2d attributes

drawInputs : Components -> EMachine -> List (Svg Msg)
drawInputs components machine =
  let ( inputVertices, _ ) = inputOutputSegments machine
  in
  machine.inputs
    |> List.filterMap
         (\inputId -> Dict.get inputId components.inputs
           |> Maybe.map (Tuple.pair inputId)
         )
    |> List.map (\( inputId, input ) -> drawInput inputId (inputVertices input))

drawMachine : Components -> EntityId -> EMachine -> Svg Msg
drawMachine components id machine =
  let
    events =
      [ SE.on "click" <| JD.map (Clicked id) <| Pointer.pageCoordDecoder
      , SE.on "mousemove" <| JD.map (MouseMoved id) <| Pointer.pageCoordDecoder
      , SA.class "machine"
      ]
  in
  Svg.g events ((drawContour components machine) ++ (drawBackground components machine :: drawInputs components machine))


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
    <| (::) (drawMachine components id machine)
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

