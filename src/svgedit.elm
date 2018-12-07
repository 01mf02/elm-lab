-- TODO:
-- This suffers from the issue described in
-- <https://github.com/elm/svg/issues/23>.

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, Attribute, div, input, text, node)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg
import Svg.Attributes as SA
import Svg.Events
import Json.Decode as Json exposing (map2, int, at)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL

type Object =
    Circle { x : Float, y : Float }
  | Rect { x : Float, y : Float, width : Float, height : Float }

type alias Model =
  { content : String
  , mode : Mode
  , prevClick : Maybe (Int, Int)
  , objects : List Object
  }

initialModel =
  { content = "c"
  , mode = CircleMode
  , prevClick = Nothing
  , objects = [] }

init : () -> (Model, Cmd Msg)
init _ =
  (initialModel, Cmd.none)



-- UPDATE

type Mode
  = CircleMode
  | RectMode

type Msg
  = Click Position
  | ModeChange Mode
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel = case msg of
      --Click {x, y} -> {model | objects = Circle (toFloat x) (toFloat y) :: model.objects}
      Click {x, y} ->
        case model.mode of
          CircleMode -> {model | objects = Circle { x = toFloat x, y = toFloat y } :: model.objects}
          RectMode ->
            case model.prevClick of
              Nothing -> { model | prevClick = Just (x, y) }
              Just (xp, yp) ->
                { model | prevClick = Nothing,
                  objects = Rect { x = toFloat xp, y = toFloat yp, width = toFloat (x - xp), height = toFloat (y - yp) } :: model.objects}
      ModeChange m -> { model | mode = m, content = "m" ++ model.content }
      NoOp -> { model | content = "n" ++ model.content }
  in (newModel, Cmd.none)

drawObject o =
  case o of
    Circle c -> drawCircle c
    Rect r -> drawRect r

drawRect {x, y, width, height} =
  Svg.rect
    [ SA.x (String.fromFloat x)
    , SA.y (String.fromFloat y)
    , SA.width (String.fromFloat width)
    , SA.height (String.fromFloat height)
    , SA.rx "15"
    , SA.ry "15"
    , SA.fillOpacity "0"
    , SA.stroke "black"
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

clickDecoder = 
    (Json.map
      Click
      (map2
        Position
        --(map2 (-)
          (at [ "offsetX" ] int)
       --   (at [ "target", "offsetLeft" ] int)
        --)
        --(map2 (-)
          (at [ "offsetY" ] int)
        --  (at [ "target", "offsetTop" ] int)
        --)
      )
    )

rawKeyDecoder =
  Json.field "key" Json.string

keyHandler s =
  case s of
    "c" -> ModeChange CircleMode
    "r" -> ModeChange RectMode
    _ -> NoOp

css path =
  node "link" [ rel "stylesheet", href path ] []

view : Model -> Html Msg
view model =
  div []
    [ css "../style.css"
    , Svg.svg
       [ SA.width "800"
       , SA.height "600"
       , SA.viewBox "0 0 800 600"
       , Svg.Events.on "click" clickDecoder
       ]
       (List.map drawObject model.objects)
    , div [] [ text model.content ]
    ]

subscriptions model =
  Browser.Events.onKeyDown (Json.map keyHandler rawKeyDecoder)
