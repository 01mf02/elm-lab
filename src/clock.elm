import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time
import Svg exposing (Svg, svg, rect, circle, line, text_)
import Svg.Attributes exposing (..)



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , shownTime : Time.Posix
  , running : Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) (Time.millisToPosix 0) True
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | Pause



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( updateShownTime { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    Pause ->
      ( updateShownTime { model | running = not model.running }
      , Cmd.none
      )

updateShownTime : Model -> Model
updateShownTime model =
  { model | shownTime = if model.running then model.time else model.shownTime }




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.zone model.shownTime)
    minute = String.fromInt (Time.toMinute model.zone model.shownTime)
    second = String.fromInt (Time.toSecond model.zone model.shownTime)
  in
  div []
    [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
    , button [ onClick Pause ] [ text "Pause" ]
    , drawClock model
    ]

pi = 3.1415

angle x max = (2.0 * pi * toFloat x / max - pi / 2)

drawHour hour =
  let
    radius = 100
    ang = angle hour 12
  in
  [ line
      [ x1 (String.fromFloat (0.8 * radius * cos ang))
      , x2 (String.fromFloat (1.0 * radius * cos ang))
      , y1 (String.fromFloat (0.8 * radius * sin ang))
      , y2 (String.fromFloat (1.0 * radius * sin ang))
      , stroke "black"
      , strokeWidth "1"
      ]
      []
  , text_
      [ x (String.fromFloat (0.65 * radius * cos ang)),
        y (String.fromFloat (0.65 * radius * sin ang)),
        dominantBaseline "middle",
        textAnchor "middle"]
      [ text (String.fromInt hour) ]
  ]

drawMinutes minute =
  let
    radius = 100
    ang = angle minute 60
  in
  line
    [ x1 (String.fromFloat (0.9 * radius * cos ang))
    , x2 (String.fromFloat (1.0 * radius * cos ang))
    , y1 (String.fromFloat (0.9 * radius * sin ang))
    , y2 (String.fromFloat (1.0 * radius * sin ang))
    , stroke "black"
    , strokeWidth "1"
    ]
    []


drawClock : Model -> Svg Msg
drawClock model =
  let
    radius = 100
    hour   = Time.toHour   model.zone model.shownTime
    minute = Time.toMinute model.zone model.shownTime
    second = Time.toSecond model.zone model.shownTime
  in
  svg
    [ width "200"
    , height "200"
    , viewBox "-100 -100 200 200"
    ]
    ([ line
        [ x1 "0"
        , x2 (String.fromFloat (0.62*radius * cos (angle second 60)))
        , y1 "0"
        , y2 (String.fromFloat (0.62*radius * sin (angle second 60)))
        , stroke "black"
        , strokeWidth "2"
        ]
        []
    , line
        [ x1 "0"
        , x2 (String.fromFloat (0.5*radius * cos (angle (hour*60 + minute) (12*60))))
        , y1 "0"
        , y2 (String.fromFloat (0.5*radius * sin (angle (hour*60 + minute) (12*60))))
        , stroke "black"
        , strokeWidth "5"
        ]
        []
    , line
        [ x1 "0"
        , x2 (String.fromFloat (0.62*radius * cos (angle (minute*60 + second) (60*60))))
        , y1 "0"
        , y2 (String.fromFloat (0.62*radius * sin (angle (minute*60 + second) (60*60))))
        , stroke "black"
        , strokeWidth "3"
        ]
        []
    , circle
        [ cx "0"
        , cy "0"
        , r (String.fromFloat radius)
        , fill "pink"
        , fillOpacity "0.7"
        , onClick Pause
        ]
        []
    ] ++ List.concatMap drawHour (List.range 1 12)
      ++ List.map drawMinutes (List.range 1 60 |> List.filter (modBy 5 >> (/=) 0)))
