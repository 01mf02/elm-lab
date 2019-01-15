port module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Json.Encode as JE
import Json.Decode as JD
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import Maybe.Extra as MaybeE

import Ctm exposing (Ctm)


-- PORTS

port requestScreenCtm : JE.Value -> Cmd msg

port receiveScreenCtm : (JE.Value -> msg) -> Sub msg



main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias SVGCoord =
  { x : Float
  , y : Float
  }

type alias SVGSize =
  { width : Float
  , height : Float
  }

type alias Rectangular a
  = { a | position : SVGCoord, size : SVGSize }


type alias SVGRect =
  { position : SVGCoord
  , size : SVGSize
  }

type Pipe = Pipe GMachine
type alias Socket = Maybe Pipe

type alias Arity = Int
type alias ConstrName = String

type Machine
  = Abs AbsInfo Socket
  | Constr ConstrName

type alias GMachine =
  Rectangular
    { machine : Machine
    , sockets : List Socket
    }

type alias AbsInfo =
  { floating : List GMachine
  }


type alias Model =
  { mode : Mode
  , machine : GMachine
  , screenCtm : Maybe Ctm
  }

type Mode
  = ConnectMode
  | MachineMode
  | InputMode
  | DeleteMode
  | TransformMode

type Msg
  = NoOp
  | ModeChanged Mode
  | ScreenCtmGot JE.Value
  | SvgClicked SVGCoord
  | SvgMouseMoved SVGCoord


svgElementId = "svg"

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , requestScreenCtm (JE.string svgElementId)
  )

initialMachine =
  { position = { x = 100, y = 100 }
  , size = { width = 100, height = 100 }
  , machine =
      Abs { floating = [] }
       <| Just <| Pipe
         { position = { x = 120, y = 120 }
         , size = { width = 60, height = 60 }
         , sockets = []
         , machine = Abs { floating = [] } Nothing
         }
  , sockets = []
  }

initialModel =
  { mode = TransformMode
  , machine = initialMachine
  , screenCtm = Nothing
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ScreenCtmGot value ->
      case JD.decodeValue Ctm.ctmDecoder value of
        Ok screenCtm ->
          ( { model | screenCtm = Just screenCtm }
          , Cmd.none
          )

        Err _ ->
          -- probably failed because svg element wasn't created yet,
          -- so request again
          (model, requestScreenCtm (JE.string svgElementId))

    _ -> (model, Cmd.none)

svgCoordDecoder : Model -> JD.Decoder SVGCoord
svgCoordDecoder { screenCtm } =
  case screenCtm of
    Nothing -> JD.fail "No CTM"
    Just ctm ->
      Ctm.clientCoordDecoder
        |> JD.map (Ctm.svgOfClientCoord >> Ctm.matrixTransform ctm)

drawSvg : Model -> List (Svg msg)
drawSvg model =
  [drawGMachine model.machine]

rectAttributes : Rectangular a -> List (Svg.Attribute msg)
rectAttributes { position, size } =
  [ SA.x (String.fromFloat position.x)
  , SA.y (String.fromFloat position.y)
  , SA.width (String.fromFloat size.width)
  , SA.height (String.fromFloat size.height)
  ]

machineOutputCoord : GMachine -> SVGCoord
machineOutputCoord { position, size } =
  { x = position.x + size.width/2
  , y = position.y + size.height
  }

centerRectAt : SVGCoord -> SVGSize -> SVGRect
centerRectAt {x, y} size =
  { position = { x = x - size.width/2, y = y - size.height/2 }
  , size = size
  }

drawSocket : Socket -> Svg msg
drawSocket socket =
  case socket of
    Nothing -> Svg.g [] []
    Just (Pipe machine) -> drawGMachine machine


type alias Polygon = List (String, String)

machineClipPolygon : Arity -> Polygon
machineClipPolygon arity =
  [ (   "0",    "0")
  , ("100%",    "0")
  , ("100%", "100%")
  , ( "55%", "100%")
  , ( "55%",  "80%")
  , ( "45%",  "80%")
  , ( "45%", "100%")
  , (   "0", "100%")
  ]

clipPathOfPolygon : Polygon -> String
clipPathOfPolygon polygon =
  "polygon(" ++ String.join ", " (List.map (\ (x, y) -> x ++ " " ++ y) polygon) ++ ")"

drawGMachine : GMachine -> Svg msg
drawGMachine machine =
  let
    arity = List.length machine.sockets
    clipPath = machineClipPolygon arity |> clipPathOfPolygon
  in
  case machine.machine of
    Abs { floating } socket ->

      Svg.g [ SA.class "gmachine" ]
        ([ drawSocket socket
        , Svg.rect (SA.class "machine-contour" :: SA.clipPath clipPath :: rectAttributes machine) []
        ] ++ List.map drawGMachine floating)
    _ -> Debug.todo "draw"


view : Model -> Html Msg
view model =
  H.div []
    [
      H.aside []
        []
    , H.main_ []
        [ Svg.svg
            [ SA.width "800px"
            , SA.height "600px"
            , SA.viewBox "0 0 800 600"
            , SE.on "click" <| JD.map SvgClicked <| svgCoordDecoder model
            , SE.on "mousemove" <| JD.map SvgMouseMoved <| svgCoordDecoder model
            , SA.id svgElementId
            ]
            (drawSvg model)
        ]
    , H.footer []
        [ H.text "Hello World!"
        ]
    ]

subscriptions model =
  Sub.batch
    [ receiveScreenCtm ScreenCtmGot
    ]

