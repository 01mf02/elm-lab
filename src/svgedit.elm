-- TODO:
-- This suffers from the issue described in
-- <https://github.com/elm/svg/issues/23>.
-- Refs:
-- * <https://javascript.info/mouse-drag-and-drop>

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, Attribute, div, input, text, node)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg
import Svg.Attributes as SA
import Svg.Events
import Json.Decode as Json exposing (map2, int, at)
import Json.Decode as Decode exposing (map2, int, at)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL

type alias SVGPoint =
  { x : Float
  , y : Float
  }

type alias MoveInfo =
  { clickPos : SVGPoint
  , pointerPos : SVGPoint
  }

initMove pos =
  { clickPos = pos
  , pointerPos = pos
  }

type Object =
    Circle { x : Float, y : Float }
  | Rect { x : Float, y : Float, width : Float, height : Float }

type alias Model =
  { content : String
  , mode : Mode
  , prevClick : Maybe SVGPoint
  , objects : List Object
  , moving : Maybe (Object, MoveInfo)
  }

initialModel =
  { content = "c"
  , mode = CircleMode
  , prevClick = Nothing
  , objects = []
  , moving = Nothing
  }

init : () -> (Model, Cmd Msg)
init _ =
  (initialModel, Cmd.none)



-- UPDATE

type Mode
  = CircleMode
  | RectMode

type Msg
  = Click SVGPoint
  | ModeChange Mode
  | MouseOver
  | MouseOut
  | ObjClicked Object SVGPoint
  | Move SVGPoint
  | NoOp

moveObject (o, {clickPos, pointerPos}) =
  case o of
    Rect r -> Rect
      { r
      | x = r.x + pointerPos.x - clickPos.x
      , y = r.y + pointerPos.y - clickPos.y
      }
    Circle c -> Circle c

createRect c1 c2 =
  let
    minMax x y =
      if x < y then (x, y) else (y, x)
    (x1, x2) = minMax c1.x c2.x
    (y1, y2) = minMax c1.y c2.y
  in
    Rect
      { x = x1
      , y = y1
      , width = x2 - x1
      , height = y2 - y1
      }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel = case msg of
      Click c ->
        case model.mode of
          CircleMode -> {model | objects = Circle c :: model.objects}
          RectMode ->
            case model.prevClick of
              Nothing -> { model | prevClick = Just c }
              Just pc ->
                { model | prevClick = Nothing,
                  objects = createRect c pc :: model.objects}
      ModeChange m -> { model | mode = m, content = "m" ++ model.content }
      MouseOver -> { model | content = "over\n" ++ model.content }
      MouseOut -> { model | content = "out" ++ model.content }
      ObjClicked o p ->
        { model | content = "oc" ++ model.content, moving =
          case model.moving of
            Nothing -> Just (o, initMove p)
            Just _ -> Nothing
        , objects = case model.moving of
            Nothing -> List.filter ((/=) o) model.objects
            Just om -> moveObject om :: model.objects
        }
      Move xy ->
        case model.moving of
          Nothing -> model
          Just (o, m) ->
            { model
            | content = "move" ++ model.content
            , moving = Just (o, { m | pointerPos = xy })
            }
      NoOp -> { model | content = "n" ++ model.content }
  in (newModel, Cmd.none)

drawObject o =
  case o of
    Circle c -> drawCircle c
    Rect r -> drawRect r

detailXYDecoder =
  Decode.map2 SVGPoint
    (Decode.at ["detail", "x"] Decode.float)
    (Decode.at ["detail", "y"] Decode.float)


drawRect r =
  Svg.rect
    [ SA.x (String.fromFloat r.x)
    , SA.y (String.fromFloat r.y)
    , SA.width (String.fromFloat r.width)
    , SA.height (String.fromFloat r.height)
    , SA.rx "15"
    , SA.ry "15"
    , SA.fillOpacity "0"
    , SA.stroke "black"
    , Svg.Events.on "svgclick" <| Json.map (ObjClicked (Rect r)) <| detailXYDecoder
    , Svg.Events.onMouseOver MouseOver
    , Svg.Events.onMouseOut MouseOut
    ]
    []

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

view : Model -> Html Msg
view model =
  div []
    [ css "../style.css"
    , Svg.svg
       [ SA.width "100%"
       , SA.height "100%"
       , SA.viewBox "0 0 800 600"
       , Svg.Events.on "svgclick" <| Json.map Click <| detailXYDecoder
       , Svg.Events.on "svgmousemove" <| Json.map Move <| detailXYDecoder
       ]
       (List.map drawObject (listFromMaybe (Maybe.map moveObject model.moving) ++ model.objects))
    , div [] [ text model.content ]
    ]

subscriptions model =
  Sub.batch <| List.concat <|
  [ [ Browser.Events.onKeyDown (Json.map keyHandler rawKeyDecoder) ]
  , case model.moving of
      Just _ -> [] --[ Browser.Events.onMouseMove (Json.map Move clientXYDecoder) ]
      Nothing -> []
  ]
