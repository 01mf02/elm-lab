module Main exposing (main)

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
import Maybe.Extra as MaybeE
import Point2d
import Vector2d

import Components exposing (..)
import Coord exposing (SVGCoord)
import Ctm exposing (Ctm)
import CssPropPort
import Entity exposing (EntityId)
import Machine exposing (..)
import Pointer
import Rect exposing (SVGRect, Rectangular, SVGSize)
import ScreenCtmPort
import Transform exposing (Transform, rootTransformId, setParentChild, reorientParentChild)

import View.Machine exposing (drawEMachine)



main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


type Pipe = Pipe GMachine
type alias Socket = Maybe Pipe

type alias Arity = Int

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


{-
unsetParent : EntityId -> Components -> Components
unsetParent childId components =
  let
    maybeParentId = Dict.get childId components.transforms |> Maybe.andThen .parent
    removeChild parent = { parent | children = Set.remove childId parent.children }
    removeParent child = { child | parent = Nothing }
    updateTransforms =
      MaybeE.unwrap identity (\ parentId -> Dict.update parentId (Maybe.map removeChild)) maybeParentId
        >> Dict.update childId (Maybe.map removeParent)
  in
  { components | transforms = updateTransforms components.transforms }
-}



{-
transformCtm transform =
  Ctm.translationMatrix transform.position

getCtm : EntityId -> Components -> Ctm
getCtm id components =
  map2
    (\machine transform ->
      case machine.parent of
        Just parentId -> Ctm.multiply (getCtm parentId components) (Ctm.translationMatrix transform.position)
        Nothing -> Ctm.unit
    )
    id components.machines components.transforms
    |> Maybe.withDefault Ctm.unit
-}


{-
findSmallestMachineContaining : SVGCoord -> Set EntityId -> Components -> Maybe EntityId
findSmallestMachineContaining coord among components =
  foldl2
    (\id machine transform maybeAcc ->
      case maybeAcc of
        Just _ -> maybeAcc
        Nothing ->
          if coordInRect transform coord
          then
           findSmallestMachineContaining (transformCoord transform coord) machine.children components
             |> Maybe.withDefault id |> Just
          else Nothing
    )
    Nothing (DictE.keepOnly among components.machines) components.transforms
-}


testComponents : Components
testComponents =
  initialComponents
    |> addMachine
         (emptyMachine { width = 60, height = 60 })
         (Transform.empty { x = 20, y = 20 })
    |> (\( components, childId ) ->
       addMachine
         (emptyMachine { width = 300, height = 300 })
         (Transform.empty { x = 100, y = 100 })
         components
         |> (\( components_, parentId ) -> nameEntity parentId "test" components_ |> setParentChild parentId childId |> setParentChild rootTransformId parentId)
       )


type alias Model =
  { mode : Mode
  , machine : GMachine
  , components : Components
  , screenCtm : Ctm
  , msElapsed : Float
  }

type alias MouseEvent =
  { id : EntityId
  , coord : SVGCoord
  }

type alias ClickHover =
  { clicked : MouseEvent
  , hovering : MouseEvent
  }

initClickHover : MouseEvent -> ClickHover
initClickHover mouseEvent =
  { clicked = mouseEvent
  , hovering = mouseEvent
  }

type Mode
  = ConnectMode
  | MachineMode (Maybe ClickHover)
  | InputMode
  | DeleteMode
  | TransformMode (Maybe ClickHover)

initialTransformMode =
  TransformMode Nothing

initialMachineMode =
  MachineMode Nothing

type Msg
  = NoOp
  | ModeChanged Mode
  | ScreenCtmGot (Maybe Ctm)
  | PointerMsg Pointer.Msg
  | OnAnimationFrameDelta Float


svgElementId = "svg"

init : () -> ( Model, Cmd Msg )
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

update : Msg -> Model -> ( Model, Cmd Msg )
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
        CssPropPort.set ":root" "--ms-elapsed" (String.fromFloat model.msElapsed)
      )

    PointerMsg (Pointer.Clicked id clientCoord) ->
      let
        svgCoord = svgOfClientCoord model clientCoord
        mouseEvent = { id = id, coord = svgCoord }
        _ = Debug.log "msg" (msg, svgCoord)
      in
      case model.mode of
        DeleteMode -> noCmd { model | components = deleteEntity id model.components }

        MachineMode maybeClickHover ->
          case maybeClickHover of
            Nothing ->
              let clickHover = initClickHover mouseEvent
              in noCmd { model | mode = MachineMode (Just clickHover) }
            Just clickHover ->
              case isValidNewMachine clickHover model.components of
                Just inside ->
                  let
                    toLocal = Transform.toLocal model.components clickHover.hovering.id
                    rect = Rect.fromCoords (toLocal clickHover.clicked.coord) (toLocal clickHover.hovering.coord)
                    transform = Transform.empty rect.position
                    machine = emptyMachine rect.size
                    components =
                      addMachine machine transform model.components
                        |> (\(components_, newId) ->
                             setParentChild id newId components_
                             |> (\components__ -> Set.foldl (reorientParentChild newId) components__ inside)
                           )
                  in
                  noCmd { model | components = components, mode = initialMachineMode }
                Nothing ->
                  noCmd model

        TransformMode maybeClickHover ->
          case maybeClickHover of
            Nothing ->
              let move = initClickHover mouseEvent
              in noCmd { model | mode = TransformMode (Just move) }
            Just move ->
              if isValidMoveMachine move model.components
              then
                { model
                  | mode = initialTransformMode
                  , components =
                      applyMove move model.components
                        |> reorientParentChild move.hovering.id move.clicked.id
                } |> noCmd
              else
                noCmd model

        _ -> noCmd model

    PointerMsg (Pointer.MouseMoved id clientCoord) ->
      let mouseEvent = { id = id, coord = svgOfClientCoord model clientCoord }
      in
      case model.mode of
        TransformMode (Just clickHover) ->
          let clickHover_ = { clickHover | hovering = mouseEvent }
          in noCmd { model | mode = TransformMode (Just clickHover_) }

        MachineMode (Just clickHover) ->
          let clickHover_ = { clickHover | hovering = mouseEvent }
          in noCmd { model | mode = MachineMode (Just clickHover_) }

        _ -> noCmd model

    _ -> let _ = Debug.log "msg" msg in noCmd model

svgOfClientCoord { screenCtm } =
  Coord.svgOfClientCoord >> Ctm.matrixTransform screenCtm

applyMove : ClickHover -> Components -> Components
applyMove move components =
  let offset = Vector2d.from (Coord.toPoint2d move.clicked.coord) (Coord.toPoint2d move.hovering.coord) |> Coord.fromVector2d
  in Transform.map (Transform.translateBy offset) move.clicked.id components

-- TODO: detect when pointer is on root plane and
-- moving machine intersects another machine
isValidMoveMachine : ClickHover -> Components -> Bool
isValidMoveMachine { clicked, hovering } components =
  Maybe.map3
    (\clickedMachine clickedTransform hoveringMachine ->
      let
        hoveringRect =
          { position = Transform.toGlobal components hovering.id { x = 0, y = 0 }
          , size = hoveringMachine.size
          }
        offset = Vector2d.from (Coord.toPoint2d clicked.coord) (Coord.toPoint2d hovering.coord)
        clickedRect =
          { position = Transform.toGlobal components clicked.id { x = 0, y = 0 } |> Coord.toPoint2d |> Point2d.translateBy offset |> Coord.fromPoint2d
          , size = clickedMachine.size
          }
        hoveringChildren =
          Dict.get hovering.id components.transforms
            |> Maybe.map .children
            |> Maybe.withDefault Set.empty
        hoveringMachines = DictE.keepOnly hoveringChildren components.machines
      in
      if Rect.inside hoveringRect clickedRect
      then
        foldl2
          (\id machine transform sofar ->
            if sofar
            then
              let
                rect =
                  { position = Transform.toGlobal components id { x = 0, y = 0 }
                  , size = machine.size
                  }
              in
              id == clicked.id || Rect.noOverlap clickedRect rect
            else False
          ) True hoveringMachines components.transforms
      else
        False
    )
    (Dict.get clicked.id components.machines)
    (Dict.get clicked.id components.transforms)
    (Dict.get hovering.id components.machines)
    |> Maybe.withDefault True

isValidNewMachine : ClickHover -> Components -> Maybe (Set EntityId)
isValidNewMachine { clicked, hovering } components =
  if clicked.id == hovering.id
  then
    let
      newRect = Rect.fromCoords clicked.coord hovering.coord
      clickedChildren =
        Dict.get clicked.id components.transforms
          |> Maybe.map .children
          |> Maybe.withDefault Set.empty
      childMachines = DictE.keepOnly clickedChildren components.machines
    in
    foldl2
      (\id machine transform ->
        Maybe.andThen
          (\inside ->
            let
              rect =
                { position = Transform.toGlobal components id { x = 0, y = 0 }
                , size = machine.size
                }
            in
            if Rect.inside newRect rect
            then Just (Set.insert id inside)
            else
              if Rect.noOverlap newRect rect
              then Just inside
              else Nothing
          )
      ) (Just Set.empty) childMachines components.transforms
  else
    Nothing




drawSvg : Model -> List (Svg Msg)
drawSvg model =
  let
    components =
      case model.mode of
        TransformMode (Just move) ->
          applyMove move model.components
            |> setSvgClass move.clicked.id "moving"
            |> (if isValidMoveMachine move model.components then identity else setInvalid move.clicked.id)
        MachineMode (Just previous) ->
          let
            toLocal = Transform.toLocal model.components previous.clicked.id
            rect = Rect.fromCoords (toLocal previous.clicked.coord) (toLocal previous.hovering.coord)
            transform = Transform.empty rect.position
            machine = emptyMachine rect.size
          in
            addMachine machine transform model.components
              |> (\( components_, childId ) ->
                   ( setSvgClass childId "creating" components_, childId )
                 )
              |> (\(components_, childId) ->
                   setParentChild previous.clicked.id childId components_
                   |> (if isValidNewMachine previous model.components |> MaybeE.isJust then identity else setInvalid childId)
                 )
        _ -> model.components

    rootChildren =
      Dict.get rootTransformId components.transforms
        |> Maybe.map .children
        |> Maybe.withDefault Set.empty
  in
 -- [drawGMachine model.machine]
  --++
  foldl2
    (\id machine transform ->
      (::) (drawEMachine components id machine transform)
    ) [] (DictE.keepOnly rootChildren components.machines) components.transforms
    |> List.map (Svg.map PointerMsg)


machineOutputCoord : GMachine -> SVGCoord
machineOutputCoord { position, size } =
  { x = position.x + size.width/2
  , y = position.y + size.height
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
        , Svg.rect (SA.class "machine-contour" :: SA.clipPath clipPath :: Rect.svgAttributes machine) []
        ] ++ List.map drawGMachine floating)
    _ -> Debug.todo "draw"

drawBackground : EntityId -> Svg Msg
drawBackground id =
  let
    attributes =
     [ SE.on "click" <| JD.map (Pointer.Clicked id) <| Coord.pageCoordDecoder
     , SE.on "mousemove" <| JD.map (Pointer.MouseMoved id) <| Coord.pageCoordDecoder
     , SA.id "background"
     ] ++ Rect.svgAttributes { position = { x = 0, y = 0 }, size = { width = 800, height = 600 } }
  in
    Svg.rect attributes [] |> Svg.map PointerMsg

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
            , SA.id svgElementId
            ]
            (drawBackground rootTransformId :: drawSvg model)
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
    "m" -> ModeChanged initialMachineMode
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

