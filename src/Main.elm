module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE

import Dict.Extra as DictE
import Maybe.Extra as MaybeE
import BoundingBox2d exposing (BoundingBox2d)
import Frame2d
import Geometry.Svg as Svg
import LineSegment2d
import Point2d exposing (Point2d)
import Rectangle2d
import Vector2d

import Components exposing (..)
import Connection
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


addEmptyMachine : MachineType -> Point2d -> Point2d -> Components -> ( Components, EntityId )
addEmptyMachine typ from to =
  let
    rectangleGlobal = Rectangle2d.from from to
    machineFrame = Rectangle2d.axes rectangleGlobal
    rectangleLocal = Rectangle2d.relativeTo machineFrame rectangleGlobal
    rootTransform = Transform.root
  in
    Components.addMachine
      (emptyMachine typ rectangleLocal)
      { rootTransform | frame = machineFrame }

addEmptyAbs = addEmptyMachine Machine.TAbs
addEmptyGhost = addEmptyMachine Machine.Ghost

testComponents : Components
testComponents =
  let
    global = Point2d.fromCoordinates
  in
  initialComponents
    |> addEmptyAbs (global ( 120, 120 )) (global ( 180, 180 ))
    |> (\( components, childId ) ->
       addEmptyAbs (global ( 100, 100 )) (global ( 400, 400 ))
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

setHovering : MouseEvent -> ClickHover -> ClickHover
setHovering mouseEvent clickHover =
  { clickHover | hovering = mouseEvent }

type Mode
  = ConnectMode (Maybe ClickHover)
  | MachineMode (Maybe ClickHover)
  | GhostMode (Maybe ClickHover)
  | InputMode
  | DeleteMode
  | TransformMode (Maybe ClickHover)

initialTransformMode =
  TransformMode Nothing

initialMachineMode =
  MachineMode Nothing

initialGhostMode =
  GhostMode Nothing

initialConnectMode =
  ConnectMode Nothing

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
  { mode = initialMachineMode
  , components = testComponents
  , screenCtm = Ctm.unit
  , msElapsed = 0
  }

noCmd model =
  ( model, Cmd.none )

updateMachine fn machineId components =
  { components | machines = Dict.update machineId (Maybe.map fn) components.machines }

capturedConnections components =
  Set.foldl
    (\capturedMachineId ->
      let (incoming, outgoing) = Connection.machineConnections components capturedMachineId
      in Set.union incoming >> Set.union outgoing
    )
    Set.empty

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
        mouseEvent = { id = id, point = svgOfClientCoord model clientCoord }
        updateClickHover = Maybe.map (setHovering mouseEvent)
      in
      case model.mode of
        DeleteMode ->
          if id == rootTransformId
          then noCmd model
          else noCmd { model | components = deleteEntity id model.components }

        MachineMode maybeClickHover ->
          case updateClickHover maybeClickHover of
            Nothing ->
              let clickHover = initClickHover mouseEvent
              in noCmd { model | mode = MachineMode (Just clickHover) }
            Just clickHover ->
              case isValidNewMachine clickHover model.components of
                Just inside ->
                  let
                    captured = capturedConnections model.components inside
                    removeConnectionsFrom machine =
                      { machine | connections = Set.diff machine.connections captured }
                    addConnectionsTo machine =
                      { machine | connections = Set.union machine.connections captured }

                    components =
                      addEmptyAbs clickHover.clicked.point clickHover.hovering.point model.components
                        |> (\(components_, newId) ->
                             Transform.adoptBy id newId components_
                             |> (\components__ -> Set.foldl (Transform.adoptBy newId) components__ inside)
                             |> updateMachine removeConnectionsFrom clickHover.hovering.id
                             |> updateMachine addConnectionsTo newId
                           )
                  in
                  noCmd { model | components = components, mode = initialMachineMode }
                Nothing ->
                  noCmd model

        GhostMode maybeClickHover ->
          case updateClickHover maybeClickHover of
            Nothing ->
              let clickHover = initClickHover mouseEvent
              in noCmd { model | mode = GhostMode (Just clickHover) }
            Just clickHover ->
              case isValidNewMachine clickHover model.components of
                Just inside ->
                  if Set.isEmpty inside
                  then
                    let
                      components =
                        addEmptyGhost clickHover.clicked.point clickHover.hovering.point model.components
                          |> (\(components_, newId) ->
                               Transform.adoptBy id newId components_
                             )
                    in
                    noCmd { model | components = components, mode = initialGhostMode }
                  else
                    noCmd model
                Nothing ->
                  noCmd model


        TransformMode maybeClickHover ->
          case updateClickHover maybeClickHover of
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
            transform = Transform.placeInRoot components mouseEvent.id
            clickedIn = Point2d.relativeTo transform mouseEvent.point
            newInput = { parent = mouseEvent.id, position = Point2d.xCoordinate clickedIn }
          in
          { model
            | components =
                addInput mouseEvent.id newInput model.components |> Tuple.first
          } |> noCmd

        ConnectMode maybeClickHover ->
          case updateClickHover maybeClickHover of
            Nothing ->
              if Connection.isValidEndpoint model.components mouseEvent.id
              then
                noCmd { model | mode = ConnectMode (Just (initClickHover mouseEvent)) }
              else
                noCmd model
            Just { clicked, hovering } ->
              case Connection.from model.components clicked.id hovering.id of
                Nothing -> noCmd model
                Just connection ->
                  { model
                    | mode = initialConnectMode
                    , components =
                        addConnection connection model.components
                          |> Tuple.first
                  } |> noCmd

    PointerMsg (Pointer.MouseMoved id clientCoord) ->
      let
        mouseEvent = { id = id, point = svgOfClientCoord model clientCoord }
        updateClickHover = Maybe.map (setHovering mouseEvent)
      in
      case model.mode of
        TransformMode maybeClickHover ->
          noCmd { model | mode = TransformMode (updateClickHover maybeClickHover) }

        MachineMode maybeClickHover ->
          noCmd { model | mode = MachineMode (updateClickHover maybeClickHover) }

        GhostMode maybeClickHover ->
          noCmd { model | mode = GhostMode (updateClickHover maybeClickHover) }

        ConnectMode maybeClickHover ->
          noCmd { model | mode = ConnectMode (updateClickHover maybeClickHover) }

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

globalBoundingBox : Components -> EntityId -> EMachine -> BoundingBox2d
globalBoundingBox components id machine =
  machine.rectangle
    |> Rectangle2d.placeIn (Transform.placeInRoot components id)
    |> Rectangle2d.boundingBox

isValidMoveMachine : ClickHover -> Components -> Bool
isValidMoveMachine { clicked, hovering } components =
  Maybe.map2
    (\clickedMachine clickedTransform ->
      let
        boundingBox = globalBoundingBox components
        maybeHoveringRect =
          Dict.get hovering.id components.machines
            |> Maybe.map (boundingBox hovering.id)

        offset = Vector2d.from clicked.point hovering.point
        clickedRect =
          clickedMachine
            |> boundingBox clicked.id
            |> BoundingBox2d.translateBy offset

        hoveringChildren = Transform.getChildren components hovering.id
        hoveringMachines =
          components.machines
            |> DictE.keepOnly (Set.filter ((/=) clicked.id) hoveringChildren)

        noInOrOutgoing ( ingoing, outgoing ) = Set.isEmpty ingoing && Set.isEmpty outgoing

        containsClicked hoveringRect =
          BoundingBox2d.isContainedIn hoveringRect clickedRect
        intersectsClicked id machine =
          BoundingBox2d.intersects clickedRect (boundingBox id machine)
      in
      ((Set.member clicked.id hoveringChildren)
        || (noInOrOutgoing (Connection.machineConnections components clicked.id)))
      &&
      (Maybe.map containsClicked maybeHoveringRect |> Maybe.withDefault True)
      &&
      Dict.foldl (\id machine -> (&&) (not (intersectsClicked id machine)))
        True hoveringMachines
    )
    (Dict.get clicked.id components.machines)
    (Dict.get clicked.id components.transforms)
    |> Maybe.withDefault False
    |> ((||) (clicked == hovering))

connectionsOfCapturedMachinesCaptured components capturedSet =
  let
    all fn = Set.toList >> List.all fn

    validConnection field connectionId =
      Connection.getConnection components connectionId
        |> Maybe.andThen (field >> Connection.getEndpointMachine components)
        |> Maybe.map (\machineId -> Set.member machineId capturedSet)
        |> Maybe.withDefault False

    validCaptured capturedId =
      let
        ( incoming, outgoing ) = Connection.machineConnections components capturedId
      in
      all (validConnection .from) incoming
        && all (validConnection .to) outgoing
  in
  all validCaptured capturedSet


isValidNewMachine : ClickHover -> Components -> Maybe (Set EntityId)
isValidNewMachine { clicked, hovering } components =
  if clicked.id == hovering.id
  then
    let
      newRect =
        Rectangle2d.from clicked.point hovering.point
          |> Rectangle2d.boundingBox
      clickedChildren = Transform.getChildren components clicked.id
      childMachines = DictE.keepOnly clickedChildren components.machines
    in
    Dict.foldl
      (\id machine ->
        Maybe.andThen
          (\inside ->
            let rect = globalBoundingBox components id machine
            in
            if BoundingBox2d.isContainedIn newRect rect
            then Just (Set.insert id inside)
            else
              if BoundingBox2d.intersects newRect rect
              then Nothing
              else Just inside
          )
      ) (Just Set.empty) childMachines
      |> MaybeE.filter (connectionsOfCapturedMachinesCaptured components)
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
          addEmptyAbs previous.clicked.point previous.hovering.point model.components
            |> (\( components_, childId ) ->
                 ( setSvgClass childId "creating" components_, childId )
               )
            |> (\(components_, childId) ->
                 Transform.adoptBy previous.clicked.id childId components_
                 |> (if isValidNewMachine previous model.components |> MaybeE.isJust then identity else setInvalid childId)
               )
        GhostMode (Just previous) ->
          addEmptyGhost previous.clicked.point previous.hovering.point model.components
            |> (\( components_, childId ) ->
                 ( setSvgClass childId "creating" components_, childId )
               )
            |> (\(components_, childId) ->
                 Transform.adoptBy previous.clicked.id childId components_
                 |> (if isValidNewMachine previous model.components |> MaybeE.filter Set.isEmpty |> MaybeE.isJust then identity else setInvalid childId)
               )
        _ -> model.components

    rootChildren = Transform.getChildren components rootTransformId

    additional =
      case model.mode of
        ConnectMode (Just { clicked, hovering }) ->
          let valid = MaybeE.isJust (Connection.from model.components clicked.id hovering.id)
          in
          View.Machine.connectionPoint model.components clicked.id
            |> Maybe.map (\point -> LineSegment2d.from hovering.point point |> Svg.lineSegment2d [ SA.class "connection", SA.id "creating", SA.class (if valid then "valid" else "invalid") ])
            |> MaybeE.toList
        _ -> []

  in
  foldl2
    (\id machine transform ->
      (::) (drawEMachine components id machine transform)
    ) additional (DictE.keepOnly rootChildren components.machines) components.transforms
    |> (::) (View.Background.draw rootTransformId)
    |> List.map (Svg.map PointerMsg)

svgRadioButton attributes { title, src } =
  H.label
    []
    [ H.input (HA.type_ "radio" :: attributes) []
    , H.img [ HA.title title, HA.src src ] []
    ]

isTransformMode mode =
  case mode of
    TransformMode _ -> True
    _ -> False

isMachineMode mode =
  case mode of
    MachineMode _ -> True
    _ -> False

isGhostMode mode =
  case mode of
    GhostMode _ -> True
    _ -> False

isConnectMode mode =
  case mode of
    ConnectMode _ -> True
    _ -> False

toolbar : Mode -> List (Html Msg)
toolbar mode =
  [ svgRadioButton
      [ HE.onClick (ModeChanged initialMachineMode)
      , HA.checked (isMachineMode mode)
      ]
      { title = "make machine (m)"
      , src = "assets/baseline-add-24px.svg"
      }
  , svgRadioButton
      [ HE.onClick (ModeChanged initialGhostMode)
      , HA.checked (isGhostMode mode)
      ]
      { title = "ghost 👻 (g)"
      , src = "assets/ghost.svg"
      }
  , svgRadioButton
      [ HE.onClick (ModeChanged InputMode)
      , HA.checked (mode == InputMode)
      ]
      { title = "insert input (i)"
      , src = "assets/baseline-input-24px.svg"
      }
  , svgRadioButton
      [ HE.onClick (ModeChanged initialConnectMode)
      , HA.checked (isConnectMode mode)
      ]
      { title = "connect (c)"
      , src = "assets/baseline-link-24px.svg"
      }
  , svgRadioButton
      [ HE.onClick (ModeChanged initialTransformMode)
      , HA.checked (isTransformMode mode)
      ]
      { title = "transform (t)"
      , src = "assets/baseline-edit-24px.svg"
      }
  , svgRadioButton
      [ HE.onClick (ModeChanged DeleteMode)
      , HA.checked (mode == DeleteMode)
      ]
      { title = "delete (d)"
      , src = "assets/baseline-delete-24px.svg"
      }
  ]


view : Model -> Html Msg
view model =
  H.div [ HA.id "container" ]
    [ H.aside [] (toolbar model.mode)
    , H.main_ []
        [ Svg.svg
            [ SA.width "100%"
            , SA.height "100%"
            , SA.id svgElementId
            ]
            (drawSvg model)
        ]
    , H.footer []
        [ H.text "Welcome to Lazy Machines."
        ]
    ]

rawKeyDecoder : JD.Decoder String
rawKeyDecoder =
  JD.field "key" JD.string

keyHandler : String -> Msg
keyHandler s =
  case s of
    "c" -> ModeChanged initialConnectMode
    "m" -> ModeChanged initialMachineMode
    "g" -> ModeChanged initialGhostMode
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

