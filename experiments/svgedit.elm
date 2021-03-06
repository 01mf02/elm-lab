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
import Svg exposing (Svg)
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

type Id = Id Int

initId = Id 0

type alias MoveInfo =
  { moveMode : MoveMode
  , object : Object
  , clickPos : SVGPoint
  , pointerPos : SVGPoint
  }

initMove : MoveMode -> Object -> SVGPoint -> MoveInfo
initMove mode obj pos =
  { moveMode = mode
  , object = obj
  , clickPos = pos
  , pointerPos = pos
  }

type alias Rectangular a
  = { a | x : Float, y : Float, width : Float, height : Float }

type alias HasChildren c a
  = { a | children : List c }

type Shape
  = Circle SVGPoint
  | Rect (Rectangular (HasChildren Object {}))

type alias Identifiable a
  = { a | id : Id }

type alias Shaped a
  = { a | shape : Shape }

type alias Object
  = Identifiable (Shaped {})

mapShaped f r = {r | shape = f r}
mapMaybeShaped f r =
  case f r of
    Nothing -> Nothing
    Just s -> Just {r | shape = s}

mapObj f (id, object) = (id, f object)

type alias Model =
  { content : List String
  , screenCtm : Maybe ScreenCtm
  , mode : Mode
  , prevClick : Maybe SVGPoint
  , objects : List Object
  , moving : Maybe MoveInfo
  , msElapsed : Float
  , hovered : Maybe Id
  , freshId : Id
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
  , freshId = initId
  }

refreshId r =
  let (Id id) = r.freshId
  in { r | freshId = Id (id + 1) }

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
  | MouseOver Id
  | MouseOut
  | Clicked MoveMode Object SVGPoint
  | GotScreenCtm JE.Value
  | OnResize Int Int
  | OnAnimationFrameDelta Float
  | NoOp

moveObject : MoveInfo -> Object
moveObject mi =
  let {object, clickPos, pointerPos} = mi in
  mapShaped (\ {shape} ->
  case shape of
    Rect r -> Rect
      { r
      | x = r.x + pointerPos.x - clickPos.x
      , y = r.y + pointerPos.y - clickPos.y
      , children = List.map (\ c -> moveObject {mi | object = c}) r.children
      }
    Circle c -> Circle c
  ) object

moveHandleObject : MoveInfo -> Object
moveHandleObject {object, clickPos, pointerPos} =
  mapShaped (\ {shape} ->
  case shape of
    Rect r -> Rect
      { r
      | x = r.x + pointerPos.x - clickPos.x
      , y = r.y + pointerPos.y - clickPos.y
      , width = r.width - (pointerPos.x - clickPos.x)
      , height = r.height - (pointerPos.y - clickPos.y)
      }
    Circle c -> Circle c
  ) object

moveFunction : MoveInfo -> Object
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
    , children = []
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

boundingBox : Shape -> Rectangular {}
boundingBox o =
  case o of
    Rect r -> { x = r.x, y = r.y, width = r.width, height = r.height }
    Circle c -> { x = c.x, y = c.y, width = 0, height = 0 }

insideBB : Rectangular a -> Rectangular b -> Bool
insideBB bbOut bbIn =
  bbOut.x < bbIn.x &&
  bbOut.y < bbIn.y &&
  bbOut.x + bbOut.width  > bbIn.x + bbIn.width &&
  bbOut.y + bbOut.height > bbIn.y + bbIn.height

-- with a little help from:
-- https://gamedev.stackexchange.com/questions/586/what-is-the-fastest-way-to-work-out-2d-bounding-box-intersection/913#913
noOverlapBB : Rectangular a -> Rectangular b -> Bool
noOverlapBB bb1 bb2 =
  (  bb1.x + bb1.width < bb2.x
  || bb2.x + bb2.width < bb1.x
  || bb1.y + bb1.height < bb2.y
  || bb2.y + bb2.height < bb1.y
  )

invalidOverlapBB : Rectangular a -> Rectangular b -> Bool
invalidOverlapBB bb1 bb2 =
  not (insideBB bb1 bb2) &&
  not (insideBB bb2 bb1) &&
  not (noOverlapBB bb1 bb2)


inside : Shaped a -> Shaped b -> Bool
inside outer inner = insideBB (boundingBox outer.shape) (boundingBox inner.shape)

invalidOverlap : Shaped a -> Shaped b -> Bool
invalidOverlap o1 o2 =
  invalidOverlapBB (boundingBox o1.shape) (boundingBox o2.shape)


anyObject : (Object -> Bool) -> List Object -> Bool
anyObject p =
  List.any
    (\ o ->
      p o ||
      (case o.shape of
        Rect r -> anyObject p r.children
        Circle c -> False
      )
    )

addObject : Object -> List Object -> List Object
addObject o objs =
  case o.shape of
    Rect r -> addRect o.id r objs
    Circle c -> o :: objs

addRect : Id -> Rectangular (HasChildren Object {}) -> List Object -> List Object
addRect id rect objs =
  let
    rectObj = {shape = Rect rect}
    (p1, container, p2) = listPartitionFirst (\ o -> inside o rectObj) objs in
  case container of
    Just c ->
      case c.shape of
        Rect rc -> {c | shape = Rect {rc | children = addRect id rect rc.children}} :: p1 ++ p2
        Circle _ -> Debug.todo "impossible"
    Nothing ->
      let (objsInside, objsOutside) = List.partition (inside rectObj) objs
      in {id = id, shape = Rect {rect | children = rect.children ++ objsInside}} :: objsOutside

-- TODO: adapt for ID comparison
-- mapObjWithId
mapObject : (Shape -> Maybe Shape) -> Id -> List Object -> List Object
mapObject f id objs =
  List.filterMap (mapMaybeShaped
    (\ obj ->
      if obj.id == id then f obj.shape
      else
        case obj.shape of
          Rect r ->
            Just (Rect {r | children = mapObject f id r.children})
          Circle c -> Just (Circle c)
    )) objs

removeObject : Id -> List Object -> List Object
removeObject id objs = mapObject (always Nothing) id objs


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Click c ->
      let
        newModel = case model.mode of
          CircleMode ->
            { model
            | objects = {id = model.freshId, shape = Circle c} :: model.objects
            } |> refreshId
          RectMode ->
            case model.prevClick of
              Nothing -> { model | prevClick = Just c }
              Just pc ->
                { model | prevClick = Nothing,
                  objects = addRect model.freshId (createRect c pc) model.objects
                } |> refreshId
      in (newModel, Cmd.none)
    ModeChange m -> ({ model | mode = m, content = "m" :: model.content }, Cmd.none)
    MouseOver o -> ({ model | content = "over" :: model.content, hovered = Just o }, Cmd.none)
    MouseOut -> ({ model | content = "out" :: model.content }, Cmd.none)
    Clicked moveMode o p ->
      (case model.moving of
        Nothing ->
          { model |
            moving = Just (initMove moveMode o p)
          , objects = removeObject o.id model.objects
          , content = "start move" :: model.content
          }
        Just om ->
          let newObj = moveFunction om
          in
            if anyObject (invalidOverlap newObj) model.objects
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

drawObject : DrawType -> Model -> Object -> Svg.Svg Msg
drawObject drawType model ({id, shape} as object) =
  let
    dt =
      case model.hovered of
        Nothing -> drawType
        Just hov -> {-if hov == id then Hovered else -}drawType

    objEvents =
      [ Svg.Events.on "click" <| Json.map (Clicked ObjectMove object) <| svgPointDecoder model
      , Svg.Events.onMouseOver (MouseOver id)
      ]

    handleEvents =
      [ Svg.Events.on "click" <| Json.map (Clicked HandleMove object) <| svgPointDecoder model
      ]

  in
    case shape of
      Circle c -> drawCircle c
      Rect r ->
        let handle = (drawRectHandle handleEvents r)
            strikethrough = (drawStrikethrough [ SA.class "strikethrough" ] r)
        in
        Svg.g [ ]
          (drawRectangular objEvents r :: List.map (drawNormal model) r.children ++
            (case dt of
              Hovered -> [handle]
              MovingOverlap -> [handle, strikethrough]
              Moving -> [handle]
              Normal -> []
            )
          )

drawRectangular : List (Attribute msg) -> Rectangular a -> Svg msg
drawRectangular attrs r =
  let
    dimAttrs =
      [ SA.x (String.fromFloat r.x)
      , SA.y (String.fromFloat r.y)
      , SA.width (String.fromFloat r.width)
      , SA.height (String.fromFloat r.height)
      ]
  in
    Svg.rect (dimAttrs ++ attrs) []

drawStrikethrough : List (Attribute msg) -> Rectangular a -> Svg msg
drawStrikethrough attrs r =
  Svg.g attrs
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

drawRectHandle attrs r =
  let
    baseAttrs =
      [ SA.cx (String.fromFloat r.x)
      , SA.cy (String.fromFloat r.y)
      , SA.r "10"
      ]
  in
    Svg.circle (baseAttrs ++ attrs) []

consIf b x xs =
  if b then x :: xs else xs

normalDrawType model obj =
  case model.hovered of
    Nothing -> Normal
    Just hov -> if hov == obj.id then Hovered else Normal

drawNormal : Model -> Object -> Svg Msg
drawNormal model obj =
  drawObject (normalDrawType model obj) model obj

movingDrawType model obj =
  if anyObject (invalidOverlap obj) model.objects
  then MovingOverlap
  else Moving

drawMoving : Model -> Object -> Svg Msg
drawMoving model obj =
  drawObject (movingDrawType model obj) model obj

drawCircle {x, y} =
  Svg.circle
    [ SA.cx (String.fromFloat x)
    , SA.cy (String.fromFloat y)
    , SA.r "3"
    ]
    []


-- VIEW


clientXYDecoder =
  Json.map2 ClientXY
    (at [ "clientX" ] int)
    (at [ "clientY" ] int)

rawKeyDecoder =
  Json.field "key" Json.string

keyHandler s =
  case s of
    "c" -> ModeChange CircleMode
    "r" -> ModeChange RectMode
    _ -> NoOp

listFromMaybe m =
  case m of
    Just x -> [x]
    Nothing -> []

clientToSVGDecoder : Maybe ScreenCtm -> ClientXY -> JD.Decoder SVGPoint
clientToSVGDecoder ctm {x, y} =
  case ctm of
    Nothing -> Json.fail "No CTM"
    Just m -> Json.succeed (matrixTransform m (SVGPoint (toFloat x) (toFloat y)))

svgPointDecoder : {a | screenCtm : Maybe ScreenCtm} -> JD.Decoder SVGPoint
svgPointDecoder {screenCtm} =
  clientXYDecoder |> Json.andThen (clientToSVGDecoder screenCtm)

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
         [ List.map (drawNormal model) model.objects
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
