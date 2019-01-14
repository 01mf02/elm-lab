port module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Json.Encode as JE
import Json.Decode as JD
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


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

type alias Pipe = GMachine
type alias Socket = Maybe Pipe

type alias Arity = Int
type alias ConstrName = String

type Machine
  = Abs AbsInfo Socket
  | App (List Socket) GMachine
  | Constr ConstrName

type alias GMachine =
  Rectangular { machine : Machine }

type alias AbsInfo =
  { arity : Arity
  , floating : List GMachine
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
  { position = { x = 0, y = 0 }
  , size = { height = 100, width = 100 }
  , machine = Abs { arity = 1, floating = []} Nothing
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

drawGMachine : GMachine -> Svg msg
drawGMachine machine =
  Svg.g [ SA.class "gmachine" ]
    [ Svg.defs []
        [ Svg.mask [ SA.id "output-hole" ]
            [
              Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "white" ] []
            ,
              Svg.rect ( SA.fill "black" :: (centerRectAt (machineOutputCoord machine) {width = 10, height = 10} |> rectAttributes)) []
            ]
        ]
    , Svg.rect (SA.class "machine-contour" :: SA.mask "url(#output-hole)" :: rectAttributes machine) []
    ]


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

