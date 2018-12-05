import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg
import Svg.Attributes as SA
import Svg.Events
import Json.Decode as Json exposing (map2, int, at)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Rect =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

type alias Circle =
  { x : Float
  , y : Float
  }

type alias Model =
  { content : String
  , clickLocation : Maybe (Int, Int)
  , objects : List Circle
  }


init : Model
init =
  { content = ""
  , clickLocation = Nothing
  , objects = [] }



-- UPDATE


type Msg
  = Click Position


update : Msg -> Model -> Model
update msg model =
  case msg of
    Click {x, y} -> {model | objects = Circle (toFloat x) (toFloat y) :: model.objects}



drawRect {x, y, width, height} =
  Svg.rect
    [ SA.x (String.fromFloat x)
    , SA.y (String.fromFloat y)
    , SA.width (String.fromFloat width)
    , SA.height (String.fromFloat height)
    , SA.rx "15"
    , SA.ry "15"
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

view : Model -> Html Msg
view model =
  div []
    [ Svg.svg
       [ SA.width "120"
       , SA.height "120"
       , SA.viewBox "0 0 120 120"
       , Svg.Events.on "click" clickDecoder
       ]
       (List.map drawCircle model.objects)
    , div [] [ text model.content ]
    ]
