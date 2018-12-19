-- TODO:
-- Positioning a Tooltip on a SVG:
-- <https://codepen.io/billdwhite/pen/rgEbc>
-- Relative positioning:
-- <https://www.w3schools.com/css/css_positioning.asp>

-- Amazing inspiration:
-- <https://sketch.io/sketchpad/>

port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, Attribute, aside, main_, button, div, footer, input, text, node, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Svg
import Svg.Attributes as SA
import Svg.Events
import Json.Decode as Json exposing (map2, int, at)
import Json.Decode as Decode exposing (map2, int, at)
import Json.Encode as JE
import Json.Decode as JD




-- MAIN

svgElementId = "svg"

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- PORTS

-- Thanks to Markus Laire for his advice on transforming
-- screen coordinates to SVG coordinates!
-- https://discourse.elm-lang.org/t/dispatching-custom-events-only-if-needed/2740

port requestScreenCtm : JE.Value -> Cmd msg

port receiveScreenCtm : (JE.Value -> msg) -> Sub msg

-- Thanks to harrysarson for the CSS custom variable workaround in
-- <https://github.com/elm/html/issues/177>

port setCssProp : JE.Value -> Cmd msg

setCssPropE selector prop value =
  setCssProp (JE.list JE.string [selector, prop, value])


-- MODEL

type alias ScreenCtm =
  { a : Float
  , b : Float
  , c : Float
  , d : Float
  , e : Float
  , f : Float
  }

type alias SVGPoint =
  { x : Float
  , y : Float
  }

type alias ClientXY =
  { x : Int
  , y : Int
  }

type MoveMode
  = HandleMove
  | ObjectMove

type alias Id = Int

type alias MoveInfo =
  { moveMode : MoveMode
  , object : Obj
  , clickPos : SVGPoint
  , pointerPos : SVGPoint
  }

initMove mode o pos =
  { moveMode = mode
  , object = o
  , clickPos = pos
  , pointerPos = pos
  }

type alias RectInfo
  = { x : Float, y : Float, width : Float, height : Float, objects : List Obj }

type Object =
    Circle { x : Float, y : Float }
  | Rect RectInfo

type alias Obj = (Id, Object)

mapObj f (id, object) = (id, f object)

type alias Model =
  { content : List String
  , screenCtm : Maybe ScreenCtm
  , mode : Mode
  , prevClick : Maybe SVGPoint
  , objects : List Obj
  , moving : Maybe MoveInfo
  , msElapsed : Float
  , hovered : Maybe Obj
  }

initialModel =
  { content = ["c"]
  , screenCtm = Nothing
  , mode = CircleMode
  , prevClick = Nothing
  , objects = []
  , moving = Nothing
  , msElapsed = 0
  , hovered = Nothing
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , requestScreenCtm (JE.string svgElementId)
  )



-- UPDATE

type Mode
  = CircleMode
  | RectMode

type Msg
  = Click SVGPoint
  | Move SVGPoint
  | ModeChange Mode
  | MouseOver Obj
  | MouseOut
  | Clicked MoveMode Obj SVGPoint
  | GotScreenCtm JE.Value
  | OnResize Int Int
  | OnAnimationFrameDelta Float
  | NoOp

moveObject : MoveInfo -> Obj
moveObject mi =
  let {object, clickPos, pointerPos} = mi in
  mapObj (\ obj ->
  case obj of
    Rect r -> Rect
      { r
      | x = r.x + pointerPos.x - clickPos.x
      , y = r.y + pointerPos.y - clickPos.y
      , objects = List.map (\ c -> moveObject {mi | object = c}) r.objects
      }
    Circle c -> Circle c
  ) object

moveHandleObject : MoveInfo -> Obj
moveHandleObject {object, clickPos, pointerPos} =
  mapObj (\ obj ->
  case obj of
    Rect r -> Rect
      { r
      | x = r.x + pointerPos.x - clickPos.x
      , y = r.y + pointerPos.y - clickPos.y
      , width = r.width - (pointerPos.x - clickPos.x)
      , height = r.height - (pointerPos.y - clickPos.y)
      }
    Circle c -> Circle c
  ) object

moveFunction : MoveInfo -> Obj
moveFunction mi =
  case mi.moveMode of
    HandleMove -> moveHandleObject mi
    ObjectMove -> moveObject mi

createRect c1 c2 =
  let
    minMax x y =
      if x < y then (x, y) else (y, x)
    (x1, x2) = minMax c1.x c2.x
    (y1, y2) = minMax c1.y c2.y
  in
    { x = x1
    , y = y1
    , width = x2 - x1
    , height = y2 - y1
    , objects = []
    }

listPartitionFirst p =
  let
    aux acc rem =
      case rem of
        [] -> (acc, Nothing, [])
        x :: xs ->
          if p x
          then (acc, Just x, xs)
          else aux (x :: acc) xs
  in aux []

boundingBox o =
  case o of
    Rect r -> {x = r.x, y = r.y, width = r.width, height = r.height }
    Circle c -> {x = c.x, y = c.y, width = 0, height = 0 }

insideBB bbOut bbIn =
  bbOut.x < bbIn.x &&
  bbOut.y < bbIn.y &&
  bbOut.x + bbOut.width  > bbIn.x + bbIn.width &&
  bbOut.y + bbOut.height > bbIn.y + bbIn.height

-- with a little help from:
-- https://gamedev.stackexchange.com/questions/586/what-is-the-fastest-way-to-work-out-2d-bounding-box-intersection/913#913
noOverlap bb1 bb2 =
  (  bb1.x + bb1.width < bb2.x
  || bb2.x + bb2.width < bb1.x
  || bb1.y + bb1.height < bb2.y
  || bb2.y + bb2.height < bb1.y
  )

overlap o1 o2 = not (noOverlap (boundingBox o1) (boundingBox o2))

inside : Object -> Object -> Bool
inside outer inner = insideBB (boundingBox outer) (boundingBox inner)

anyObject : (Object -> Bool) -> List Obj -> Bool
anyObject p =
  List.any
    (\ (_, o) ->
      p o ||
      (case o of
        Rect r -> anyObject p r.objects
        Circle c -> False
      )
    )

invalidOverlap o1 o2 =
  not (inside o1 o2) &&
  not (inside o2 o1) &&
  overlap o1 o2

addObject : Obj -> List Obj -> List Obj
addObject (id, obj) objs =
  case obj of
    Rect r -> addRect id r objs
    Circle c -> (id, Circle c) :: objs

addRect : Id -> RectInfo -> List Obj -> List Obj
addRect id rect objs =
  let (p1, mo, p2) = listPartitionFirst (\ (_, o) -> inside o (Rect rect)) objs in
  case mo of
    Just (idmo, Rect o) ->
      (idmo, Rect {o | objects = addRect id rect o.objects}) :: p1 ++ p2
    _ ->
      let (objsInside, objsOutside) = List.partition (Tuple.second >> inside (Rect rect)) objs
      in (id, Rect {rect | objects = rect.objects ++ objsInside}) :: objsOutside

-- TODO: adapt for ID comparison
-- mapObjWithId
mapObject f obj objs =
  List.filterMap
    (\ (id, o) ->
      (if o == Tuple.second obj then f o
      else
        case o of
          Rect r ->
            Just (Rect {r | objects = mapObject f obj r.objects})
          Circle c -> Just (Circle c))
      |> Maybe.map (Tuple.pair id)
    ) objs

removeObject : Obj -> List Obj -> List Obj
removeObject obj objs = mapObject (always Nothing) obj objs


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Click c ->
      let
        newModel = case model.mode of
          CircleMode -> {model | objects = (0, Circle c) :: model.objects}
          RectMode ->
            case model.prevClick of
              Nothing -> { model | prevClick = Just c }
              Just pc ->
                { model | prevClick = Nothing,
                  objects = addRect 0 (createRect c pc) model.objects}
      in (newModel, Cmd.none)
    ModeChange m -> ({ model | mode = m, content = "m" :: model.content }, Cmd.none)
    MouseOver o -> ({ model | content = "over" :: model.content, hovered = Just o }, Cmd.none)
    MouseOut -> ({ model | content = "out" :: model.content }, Cmd.none)
    Clicked moveMode o p ->
      (case model.moving of
        Nothing ->
          { model |
            moving = Just (initMove moveMode o p)
          , objects = removeObject o model.objects
          }
        Just om ->
          let newObj = moveFunction om
          in
            if anyObject (invalidOverlap (Tuple.second newObj)) model.objects
            then model
            else
              { model
              | moving = Nothing
              , objects = addObject newObj model.objects
              }
      , Cmd.none)

    Move xy ->
      let
        newModel =
          { model |
            moving = case model.moving of
              Nothing -> Nothing
              Just mi -> Just { mi | pointerPos = xy }
          }
      in (newModel, Cmd.none)

    OnResize _ _ ->
      ( model, requestScreenCtm (JE.string svgElementId) )

    GotScreenCtm value ->
      case JD.decodeValue screenCtmDecoder value of
        Ok screenCtm ->
          ( { model | screenCtm = Just screenCtm }
          , Cmd.none
          )

        Err _ ->
          -- probably failed because svg element wasn't created yet,
          -- so request again
          ( model, requestScreenCtm (JE.string svgElementId) )

    OnAnimationFrameDelta d ->
      ( { model | msElapsed = model.msElapsed + d },
        setCssPropE ":root" "--ms-elapsed" (String.fromFloat model.msElapsed) )

    NoOp -> ({ model | content = "n" :: model.content }, Cmd.none)

drawObject : DrawType -> Model -> Obj -> Svg.Svg Msg
drawObject drawType model (id, o) =
  let
    dt =
      case model.hovered of
        Nothing -> drawType
        Just (hid, ho) -> if o == ho then Hovered else drawType
  in case o of
    Circle c -> drawCircle c
    Rect r -> drawRect dt model r

drawSimpleRect : Model -> RectInfo -> Svg.Svg Msg
drawSimpleRect model r =
  Svg.g []
    (Svg.rect
      [ SA.x (String.fromFloat r.x)
      , SA.y (String.fromFloat r.y)
      , SA.width (String.fromFloat r.width)
      , SA.height (String.fromFloat r.height)
      , Svg.Events.on "click" <| Json.map (Clicked ObjectMove (0, Rect r)) <| svgPointDecoder model
      , Svg.Events.onMouseOver (MouseOver (0, Rect r))
      , Svg.Events.onMouseOut MouseOut
      ]
      []
    :: List.map (drawObject Normal model) r.objects)

drawStrikethrough r =
  Svg.g [ SA.class "strikethrough" ]
    [ Svg.line
        [ SA.x1 (String.fromFloat r.x)
        , SA.y1 (String.fromFloat r.y)
        , SA.x2 (String.fromFloat (r.x + r.width))
        , SA.y2 (String.fromFloat (r.y + r.height))
        ]
        []
    , Svg.line
        [ SA.x1 (String.fromFloat (r.x + r.width))
        , SA.y1 (String.fromFloat r.y)
        , SA.x2 (String.fromFloat r.x)
        , SA.y2 (String.fromFloat (r.y + r.height))
        ]
        []
    ]

type DrawType
  = Normal
  | Hovered
  | Moving
  | MovingOverlap

drawRectHandle model r =
  Svg.circle
    [ SA.cx (String.fromFloat r.x)
    , SA.cy (String.fromFloat r.y)
    , SA.r "10"
    , Svg.Events.on "click" <| Json.map (Clicked HandleMove (0, Rect r)) <| svgPointDecoder model
    ]
    []

consIf b x xs =
  if b then x :: xs else xs

drawRect drawType model r =
  Svg.g [ ]
    ([drawSimpleRect model r]
     |> consIf (drawType == MovingOverlap) (drawStrikethrough r)
     |> consIf (drawType == Hovered) (drawRectHandle model r)
    )

-- TODO: remove id
drawMoving model (id, o) =
  if anyObject (invalidOverlap o) model.objects
  then drawObject MovingOverlap model (id, o)
  else drawObject Moving model (id, o)

drawCircle {x, y} =
  Svg.circle
    [ SA.cx (String.fromFloat x)
    , SA.cy (String.fromFloat y)
    , SA.r "3"
    ]
    []

type alias Position =
  {x : Int, y : Int}

-- VIEW


clientXYDecoder =
  Json.map2 Tuple.pair
    (at [ "clientX" ] int)
    (at [ "clientY" ] int)

rawKeyDecoder =
  Json.field "key" Json.string

keyHandler s =
  case s of
    "c" -> ModeChange CircleMode
    "r" -> ModeChange RectMode
    _ -> NoOp

css path =
  node "link" [ rel "stylesheet", href path ] []

listFromMaybe m =
  case m of
    Just x -> [x]
    Nothing -> []

clientToSVGDecoder ctm (x, y) =
  case ctm of
    Nothing -> Json.fail "No CTM"
    Just m -> Json.succeed (matrixTransform m (SVGPoint (toFloat x) (toFloat y)))

svgPointDecoder model =
  clientXYDecoder |> Json.andThen (clientToSVGDecoder model.screenCtm)

rectButton =
  Svg.svg
    [ SA.viewBox "0 0 10 10"
    ]
    [ Svg.rect
      [ SA.x "2.0"
      , SA.y "2.0"
      , SA.width "6.0"
      , SA.height "6.0"
      --, SA.rx "15"
      --, SA.ry "15"
      , SA.fillOpacity "0"
      , SA.stroke "black"
      ]
      []
    ]

circleButton =
  Svg.svg
    [ SA.viewBox "0 0 10 10"
    ]
    [ Svg.circle
        [ SA.cx "5"
        , SA.cy "5"
        , SA.r "3"
        ]
        []
    ]

view : Model -> Html Msg
view model =
  div [ SA.class "container" ]
    [ aside []
      [ label
        []
        [ input
          [ type_ "radio"
          , name "active-tool"
          , value "rect"
          , onClick (ModeChange RectMode)
          , checked (model.mode == RectMode)
          ]
          []
        , rectButton
        ]
      , label
        []
        [ input
          [ type_ "radio"
          , name "active-tool"
          , value "circle"
          , onClick (ModeChange CircleMode)
          , checked (model.mode == CircleMode)
          ]
          []
        , circleButton
        ]
      ]
    , main_ []
      [ Svg.svg
       [ SA.width "100%"
       , SA.height "100%"
       , SA.viewBox "0 0 800 600"
       , Svg.Events.on "click" <| Json.map Click <| svgPointDecoder model
       , Svg.Events.on "mousemove" <| Json.map Move <| svgPointDecoder model
       , SA.id svgElementId
       ]
       <| List.concat
         [ List.map (drawObject Normal model) model.objects
         , List.map (drawMoving model) <| listFromMaybe (Maybe.map moveFunction model.moving)
         ]
      ]
    , footer [] (List.map text model.content)
    ]

subscriptions model =
  Sub.batch
    [ receiveScreenCtm GotScreenCtm
    , Browser.Events.onResize OnResize
    , Browser.Events.onKeyDown (Json.map keyHandler rawKeyDecoder)
    , Browser.Events.onAnimationFrameDelta OnAnimationFrameDelta
    ]

screenCtmDecoder : JD.Decoder ScreenCtm
screenCtmDecoder =
  JD.map6 (\a b c d e f -> { a = a, b = b, c = c, d = d, e = e, f = f })
    (JD.field "a" JD.float)
    (JD.field "b" JD.float)
    (JD.field "c" JD.float)
    (JD.field "d" JD.float)
    (JD.field "e" JD.float)
    (JD.field "f" JD.float)

-- MATH


{-| See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
-}
matrixTransform : ScreenCtm -> SVGPoint -> SVGPoint
matrixTransform c p =
  { x = c.a * p.x + c.c * p.y + c.e
  , y = c.b * p.x + c.d * p.y + c.f
  }
