port module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html as H exposing (Html)
import Json.Decode as JD
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import Dict.Extra as DictE
--import Maybe.Extra as MaybeE

import Ctm exposing (Ctm)
import CssPropPort
import ScreenCtmPort



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

type alias EntityId = Int

type ConnectionEndpoint
  = Output EntityId
  | Input EntityId Int

type alias Connection =
  { from : ConnectionEndpoint
  , to : ConnectionEndpoint
  }

type MachineType
  = TConstr ConstrName
  | TAbs
  | TReference String

type alias EMachine =
  { children : Set EntityId
  , inputs : List (Maybe EntityId)
  , machineType : MachineType
  }

addMachine : EMachine -> Transform -> Components -> (Components, EntityId)
addMachine machine transform components =
  ( { components
      | nextId = components.nextId + 1
      , machines = Dict.insert components.nextId machine components.machines
      , transforms = Dict.insert components.nextId transform components.transforms
    }
  , components.nextId
  )

addConnection connection components =
  ( { components
      | nextId = components.nextId + 1
      , connections = Dict.insert components.nextId connection components.connections
    }
  , components.nextId
  )

type alias Transform = SVGRect


type alias Components =
  { nextId : EntityId
  , names : Dict EntityId String
  , machines : Dict EntityId EMachine
  , connections : Dict EntityId Connection
  , transforms : Dict EntityId Transform
  }

foldl : (EntityId -> a -> b -> b) -> b -> Dict EntityId a -> b
foldl =
    Dict.foldl

stringFromSVGCoord { x, y } =
  "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"

drawMachineContour id transform =
  let
    attributes =
      [ SA.class "machine-contour"
      , SE.on "click" <| JD.map (ObjectClicked id) <| Ctm.clientCoordDecoder
      ]
      ++ rectAttributes { transform | position = { x = 0, y = 0 } }
  in
  Svg.rect attributes [ ]

drawEMachine : Components -> EntityId -> EMachine -> Transform -> Svg Msg
drawEMachine components id machine transform =
  drawEMachines components
    (DictE.keepOnly machine.children components.machines)
    components.transforms
    |> (::) (drawMachineContour id transform)
    |> Svg.g [ SA.transform ("translate" ++ stringFromSVGCoord transform.position) ]

drawEMachines : Components -> Dict EntityId EMachine -> Dict EntityId Transform -> List (Svg Msg)
drawEMachines components =
  foldl2
    (\childId childMachine childTransform ->
      (::) (drawEMachine components childId childMachine childTransform)
    ) [ ]

{-| Perform inner join on two component dicts
and aggregate the result. The order matters:
we get elements from the 1st component dict and
then inner join with the second component dict
-}
foldl2 : (EntityId -> a -> b -> c -> c) -> c -> Dict EntityId a -> Dict EntityId b -> c
foldl2 fn initial_ component1 component2 =
    Dict.foldl
        (\uid a ->
            Maybe.map (fn uid a)
                (Dict.get uid component2)
                |> Maybe.withDefault identity
        )
        initial_
        component1


foldl3 : (EntityId -> a -> b -> c -> d -> d) -> d -> Dict EntityId a -> Dict EntityId b -> Dict EntityId c -> d
foldl3 fn initial_ component1 component2 component3 =
    Dict.foldl
        (\uid a ->
            Maybe.map2 (fn uid a)
                (Dict.get uid component2)
                (Dict.get uid component3)
                |> Maybe.withDefault identity
        )
        initial_
        component1

initialComponents : Components
initialComponents =
  { nextId = 0
  , names = Dict.empty
  , machines = Dict.empty
  , connections = Dict.empty
  , transforms = Dict.empty
  }

nameEntity : EntityId -> String -> Components -> Components
nameEntity id name components =
  { components
    | names = Dict.insert id name components.names
  }

-- remove children?
deleteEntity : EntityId -> Components -> Components
deleteEntity id components =
  { nextId = components.nextId
  , names = Dict.remove id components.names
  , machines = Dict.remove id components.machines
  , connections = Dict.remove id components.connections
  , transforms = Dict.remove id components.transforms
  }

testComponents : Components
testComponents =
  initialComponents
    |> addMachine
         { children = Set.empty, inputs = [], machineType = TAbs }
         { position = { x = 20, y = 20 }
         , size = { width = 60, height = 60 }
         }
    |> (\( components, childId ) ->
       addMachine
         { children = Set.singleton childId, inputs = [], machineType = TAbs }
         { position = { x = 100, y = 100 }
         , size = { width = 100, height = 100 }
         }
         components
       )
    |> (\( components, parentId ) -> nameEntity parentId "test" components)


type alias Model =
  { mode : Mode
  , machine : GMachine
  , components : Components
  , screenCtm : Ctm
  , msElapsed : Float
  }

type Mode
  = ConnectMode
  | MachineMode
  | InputMode
  | DeleteMode
  | TransformMode { lastClick : Maybe (EntityId, Ctm.ClientCoord) }

initialTransformMode =
  TransformMode { lastClick = Nothing }

type Msg
  = NoOp
  | ModeChanged Mode
  | ScreenCtmGot (Maybe Ctm)
  | SvgClicked Ctm.ClientCoord
  | SvgMouseMoved Ctm.ClientCoord
  | ObjectClicked EntityId Ctm.ClientCoord
  | OnAnimationFrameDelta Float


svgElementId = "svg"

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , ScreenCtmPort.request svgElementId
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
  { mode = DeleteMode
  , machine = initialMachine
  , components = testComponents
  , screenCtm = Ctm.unit
  , msElapsed = 0
  }

noCmd model =
  ( model, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ModeChanged mode ->
      noCmd { model | mode = mode }

    ScreenCtmGot maybeCtm ->
      case maybeCtm of
        Just ctm -> noCmd { model | screenCtm = ctm }
        Nothing -> ( model, ScreenCtmPort.request svgElementId )

    OnAnimationFrameDelta d ->
      ( { model | msElapsed = model.msElapsed + d },
        CssPropPort.set ":root" "--ms-elapsed" (String.fromFloat model.msElapsed) )

    ObjectClicked id clientCoord ->
      let
        svgCoord = svgOfClientCoord model clientCoord
        _ = Debug.log "msg" (msg, svgCoord)
      in
      case model.mode of
        DeleteMode -> noCmd { model | components = deleteEntity id model.components }
        _ -> noCmd model

    _ -> (model, Cmd.none)

svgOfClientCoord { screenCtm } =
  Ctm.svgOfClientCoord >> Ctm.matrixTransform screenCtm

drawSvg : Model -> List (Svg Msg)
drawSvg model =
  let components = model.components
  in
 -- [drawGMachine model.machine]
  --++
  foldl3
    (\id machine transform name ->
      (::) (drawEMachine components id machine transform)
    ) [] components.machines components.transforms components.names

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
            , SE.on "click" <| JD.map SvgClicked <| Ctm.clientCoordDecoder
            , SE.on "mousemove" <| JD.map SvgMouseMoved <| Ctm.clientCoordDecoder
            , SA.id svgElementId
            ]
            (drawSvg model)
        ]
    , H.footer []
        [ H.text "Hello World!"
        ]
    ]

rawKeyDecoder : JD.Decoder String
rawKeyDecoder =
  JD.field "key" JD.string

keyHandler : String -> Msg
keyHandler s =
  case s of
    "c" -> ModeChanged ConnectMode
    "m" -> ModeChanged MachineMode
    "i" -> ModeChanged InputMode
    "d" -> ModeChanged DeleteMode
    "t" -> ModeChanged initialTransformMode
    _ -> NoOp

subscriptions model =
  Sub.batch
    [ ScreenCtmPort.receive ScreenCtmGot
    , Browser.Events.onKeyDown (JD.map keyHandler rawKeyDecoder)
    , Browser.Events.onAnimationFrameDelta OnAnimationFrameDelta
    ]

