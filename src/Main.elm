port module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Json.Encode as JE
import Json.Decode as JD



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

type Pipe = Pipe GMachine

type alias Arity = Int
type alias ConstrName = String

type Machine
  = Abs AbsInfo Pipe
  | App (List Pipe) GMachine
  | Constr ConstrName

type alias GMachine =
  { position : SVGCoord
  , size : SVGSize
  , machine : Machine
  }

type alias AbsInfo =
  { arity : Arity
  , floating : List GMachine
  }


type alias Model =
  { mode : Mode
  }

type Mode
  = ConnectMode
  | MachineMode
  | InputMode
  | DeleteMode
  | TransformMode

type Msg
  = NoOp
  | ModeChange Mode

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none
  )

initialModel =
  { mode = TransformMode
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

view : Model -> Html Msg
view model =
  H.text "Hello World!"

subscriptions model =
  Sub.none

