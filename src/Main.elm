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
import BoundingBox2d
import Frame2d
import Point2d exposing (Point2d)
import Rectangle2d
import Vector2d

import Components exposing (..)
import Ctm exposing (Ctm)
import CssPropPort
import Entity exposing (EntityId)
import Machine exposing (..)
import Pointer
import ScreenCtmPort
import Transform exposing (Transform)

import View.Background
import View.Machine exposing (drawEMachine)



main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



testComponents : Components
testComponents =
  let
    makeRect c1 c2 =
      Rectangle2d.from (Point2d.fromCoordinates c1) (Point2d.fromCoordinates c2)
  in
  initialComponents
    |> addMachine
         (emptyMachine (makeRect ( 120, 120 ) ( 180, 180 )))
         Transform.root
    |> (\( components, childId ) ->
       addMachine
         (emptyMachine (makeRect ( 100, 100 ) ( 400, 400 )))
         Transform.root
         components
         |> (\( components_, parentId ) ->
              nameEntity parentId "test" components_
                |> Transform.adoptBy rootTransformId parentId
                |> Transform.adoptBy parentId childId
            )
       )


type alias Model =
  { mode : Mode
  , components : Components
  , screenCtm : Ctm
  , msElapsed : Float
  }

type alias MouseEvent =
  { id : EntityId
  , point : Point2d
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

initialModel =
  { mode = DeleteMode
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
      let mouseEvent = { id = id, point = svgOfClientCoord model clientCoord }
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
                    rect = Rectangle2d.from clickHover.clicked.point clickHover.hovering.point
                    machine = emptyMachine rect
                    components =
                      addMachine machine Transform.root model.components
                        |> (\(components_, newId) ->
                             Transform.adoptBy id newId components_
                             |> (\components__ -> Set.foldl (Transform.adoptBy newId) components__ inside)
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
                        |> Transform.adoptBy move.hovering.id move.clicked.id
                } |> noCmd
              else
                noCmd model

        InputMode ->
          let
            components = model.components

            insertInput machine =
              let
                transform = Transform.placeInRoot components mouseEvent.id
                clickedIn = Point2d.relativeTo transform mouseEvent.point
                newInput = ( Point2d.xCoordinate clickedIn, Nothing )
              in
              { machine | inputs = newInput :: machine.inputs |> List.sortBy Tuple.first }

          in
          { model
            | components =
                { components
                  | machines =
                      Dict.update id (Maybe.map insertInput) components.machines
                }
          } |> noCmd

        _ -> noCmd model

    PointerMsg (Pointer.MouseMoved id clientCoord) ->
      let mouseEvent = { id = id, point = svgOfClientCoord model clientCoord }
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
  Pointer.svgOfClientCoord
    >> Ctm.matrixTransform screenCtm
    >> Point2d.fromCoordinates

applyMove : ClickHover -> Components -> Components
applyMove move components =
  let offset = Vector2d.from move.clicked.point move.hovering.point
  in Transform.map (Transform.translateBy offset) move.clicked.id components

-- TODO: detect when pointer is on root plane and
-- moving machine intersects another machine
isValidMoveMachine : ClickHover -> Components -> Bool
isValidMoveMachine { clicked, hovering } components =
  Maybe.map3
    (\clickedMachine clickedTransform hoveringMachine ->
      let
        hoveringRect =
          hoveringMachine.rectangle
            |> Rectangle2d.placeIn (Transform.placeInRoot components hovering.id)
            |> Rectangle2d.boundingBox
        offset = Vector2d.from clicked.point hovering.point
        clickedRect =
          clickedMachine.rectangle
            |> Rectangle2d.placeIn (Transform.placeInRoot components clicked.id)
            |> Rectangle2d.translateBy offset
            |> Rectangle2d.boundingBox
        hoveringChildren =
          Dict.get hovering.id components.transforms
            |> Maybe.map .children
            |> Maybe.withDefault Set.empty
        hoveringMachines = DictE.keepOnly hoveringChildren components.machines
      in
      if BoundingBox2d.isContainedIn hoveringRect clickedRect
      then
        Dict.foldl
          (\id machine sofar ->
            if sofar
            then
              let
                rect =
                  machine.rectangle
                    |> Rectangle2d.placeIn (Transform.placeInRoot components id)
                    |> Rectangle2d.boundingBox
              in
              id == clicked.id || not (BoundingBox2d.intersects clickedRect rect)
            else False
          ) True hoveringMachines
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
      newRect = Rectangle2d.from clicked.point hovering.point |> Rectangle2d.boundingBox
      clickedChildren =
        Dict.get clicked.id components.transforms
          |> Maybe.map .children
          |> Maybe.withDefault Set.empty
      childMachines = DictE.keepOnly clickedChildren components.machines
    in
    Dict.foldl
      (\id machine ->
        Maybe.andThen
          (\inside ->
            let
              rect =
                machine.rectangle
                  |> Rectangle2d.placeIn (Transform.placeInRoot components id)
                  |> Rectangle2d.boundingBox
            in
            if BoundingBox2d.isContainedIn newRect rect
            then Just (Set.insert id inside)
            else
              if BoundingBox2d.intersects newRect rect
              then Nothing
              else Just inside
          )
      ) (Just Set.empty) childMachines
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
            rect = Rectangle2d.from previous.clicked.point previous.hovering.point
            machine = emptyMachine rect
          in
            addMachine machine Transform.root model.components
              |> (\( components_, childId ) ->
                   ( setSvgClass childId "creating" components_, childId )
                 )
              |> (\(components_, childId) ->
                   Transform.adoptBy previous.clicked.id childId components_
                   |> (if isValidNewMachine previous model.components |> MaybeE.isJust then identity else setInvalid childId)
                 )
        _ -> model.components

    rootChildren =
      Dict.get rootTransformId components.transforms
        |> Maybe.map .children
        |> Maybe.withDefault Set.empty
  in
  foldl2
    (\id machine transform ->
      (::) (drawEMachine components id machine transform)
    ) [] (DictE.keepOnly rootChildren components.machines) components.transforms
    |> (::) (View.Background.draw rootTransformId)
    |> List.map (Svg.map PointerMsg)

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

