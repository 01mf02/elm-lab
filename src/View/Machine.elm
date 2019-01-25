module View.Machine exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import Dict.Extra as DictE

import Components exposing (..)
import Coord
import Entity exposing (..)
import Machine exposing (..)
import Pointer exposing (Msg(..))
import Rect exposing (SVGRect, Rectangular, SVGSize)
import Transform exposing (Transform, rootTransformId, setParentChild, reorientParentChild)


drawMachineContour : EntityId -> EMachine -> Svg Msg
drawMachineContour id machine =
  let
    attributes =
      [ SA.class "machine-contour"
      , SE.on "click" <| JD.map (Clicked id) <| Coord.pageCoordDecoder
      , SE.on "mousemove" <| JD.map (MouseMoved id) <| Coord.pageCoordDecoder
      ]
      ++ Rect.svgAttributes { position = { x = 0, y = 0 }, size = machine.size }
  in
  Svg.rect attributes [ ]

drawMachineStrikethrough machine =
  drawStrikethrough
    [ SA.class "strikethrough" ]
    { position = { x = 0, y = 0 }, size = machine.size }

drawStrikethrough : List (Svg.Attribute msg) -> Rectangular a -> Svg msg
drawStrikethrough attrs { position, size } =
  let
    x = position.x
    y = position.x
    w = size.width
    h = size.height
  in
  Svg.g attrs
    [ Svg.line
        [ SA.x1 (String.fromFloat x)
        , SA.y1 (String.fromFloat y)
        , SA.x2 (String.fromFloat (x + w))
        , SA.y2 (String.fromFloat (y + h))
        ]
        []
    , Svg.line
        [ SA.x1 (String.fromFloat (x + w))
        , SA.y1 (String.fromFloat y)
        , SA.x2 (String.fromFloat x)
        , SA.y2 (String.fromFloat (y + h))
        ]
        []
    ]

drawEMachine : Components -> EntityId -> EMachine -> Transform -> Svg Msg
drawEMachine components id machine transform =
  let
    groupAttributes =
      [ SA.transform ("translate" ++ Coord.toString transform.translate) ]
      ++
      (Dict.get id components.svgClasses |> Maybe.map (SA.class >> List.singleton) |> Maybe.withDefault [])
  in
  Svg.g groupAttributes
    <| (::) (drawMachineContour id machine)
    <| (if Set.member id components.invalids then (::) (drawMachineStrikethrough machine) else identity)
    <| drawEMachines components
         (DictE.keepOnly transform.children components.machines)
         components.transforms

drawEMachines : Components -> Dict EntityId EMachine -> Dict EntityId Transform -> List (Svg Msg)
drawEMachines components =
  foldl2
    (\childId childMachine childTransform ->
      (::) (drawEMachine components childId childMachine childTransform)
    ) [ ]

