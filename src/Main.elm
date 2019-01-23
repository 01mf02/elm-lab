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
  { inputs : List (Maybe EntityId)
  , machineType : MachineType
  , size : SVGSize
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

type alias Transform =
  { translate : SVGCoord
  , parent : Maybe EntityId
  , children : Set EntityId
  }


type alias Components =
  { nextId : EntityId
  , names : Dict EntityId String
  , machines : Dict EntityId EMachine
  , connections : Dict EntityId Connection
  , transforms : Dict EntityId Transform
  , svgClasses : Dict EntityId String
  , invalids : Set EntityId
  }

foldl : (EntityId -> a -> b -> b) -> b -> Dict EntityId a -> b
foldl =
    Dict.foldl

stringFromSVGCoord { x, y } =
  "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"

drawMachineContour : EntityId -> EMachine -> Svg Msg
drawMachineContour id machine =
  let
    attributes =
      [ SA.class "machine-contour"
      , SE.on "click" <| JD.map (Clicked id) <| Ctm.pageCoordDecoder
      , SE.on "mousemove" <| JD.map (MouseMoved id) <| Ctm.pageCoordDecoder
      ]
      ++ rectAttributes { position = { x = 0, y = 0 }, size = machine.size }
  in
  Svg.rect attributes [ ]

drawMachineStrikethrough machine =
  drawStrikethrough
    [ SA.class "strikethrough" ]
    { position = { x = 0, y = 0 }, size = machine.size }

drawStrikethrough : List (Svg.Attribute msg) -> Rectangular a -> Svg msg
drawStrikethrough attrs { position, size } =
  let
    x = position.x
    y = position.x
    w = size.width
    h = size.height
  in
  Svg.g attrs
    [ Svg.line
        [ SA.x1 (String.fromFloat x)
        , SA.y1 (String.fromFloat y)
        , SA.x2 (String.fromFloat (x + w))
        , SA.y2 (String.fromFloat (y + h))
        ]
        []
    , Svg.line
        [ SA.x1 (String.fromFloat (x + w))
        , SA.y1 (String.fromFloat y)
        , SA.x2 (String.fromFloat x)
        , SA.y2 (String.fromFloat (y + h))
        ]
        []
    ]

drawEMachine : Components -> EntityId -> EMachine -> Transform -> Svg Msg
drawEMachine components id machine transform =
  let
    groupAttributes =
      [ SA.transform ("translate" ++ stringFromSVGCoord transform.translate) ]
      ++
      (Dict.get id components.svgClasses |> Maybe.map (SA.class >> List.singleton) |> Maybe.withDefault [])
  in
  Svg.g groupAttributes
    <| (::) (drawMachineContour id machine)
    <| (if Set.member id components.invalids then (::) (drawMachineStrikethrough machine) else identity)
    <| drawEMachines components
         (DictE.keepOnly transform.children components.machines)
         components.transforms

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

map2 : (a -> b -> c) -> EntityId -> Dict EntityId a -> Dict EntityId b -> Maybe c
map2 fn id component1 component2 =
  Dict.get id component1
    |> Maybe.andThen
      (\a ->
        Dict.get id component2
          |> Maybe.map (\b -> fn a b)
      )

rootTransform : Transform
rootTransform =
  { translate = { x = 0, y = 0 }
  , parent = Nothing
  , children = Set.empty
  }

initialComponents : Components
initialComponents =
  { nextId = 1
  , names = Dict.empty
  , machines = Dict.empty
  , connections = Dict.empty
  , transforms = Dict.singleton rootTransformId rootTransform
  , svgClasses = Dict.empty
  , invalids = Set.empty
  }

rootTransformId : EntityId
rootTransformId = 0

nameEntity : EntityId -> String -> Components -> Components
nameEntity id name components =
  { components
    | names = Dict.insert id name components.names
  }

setSvgClass : EntityId -> String -> Components -> Components
setSvgClass id class components =
  { components
    | svgClasses = Dict.insert id class components.svgClasses
  }

setParentChild : EntityId -> EntityId -> Components -> Components
setParentChild parentId childId components =
  let
    addChild parent = { parent | children = Set.insert childId parent.children }
    setParent child = { child | parent = Just parentId }
    updateTransforms =
      Dict.update parentId (Maybe.map addChild)
        >> Dict.update childId (Maybe.map setParent)
  in
  { components | transforms = updateTransforms components.transforms }

setInvalid : EntityId -> Components -> Components
setInvalid id components =
  { components | invalids = Set.insert id components.invalids }

-- remove children?
deleteEntity : EntityId -> Components -> Components
deleteEntity id components =
  { nextId = components.nextId
  , names = Dict.remove id components.names
  , machines = Dict.remove id components.machines
  , connections = Dict.remove id components.connections
  , transforms = Dict.remove id components.transforms
  , svgClasses = Dict.remove id components.svgClasses
  , invalids = Set.remove id components.invalids
  }

offsetTransform : EntityId -> SVGCoord -> Components -> Components
offsetTransform id coord components =
  let
    offset transform =
      { transform | translate = addCoords coord transform.translate }
  in
  { components
    | transforms = Dict.update id (Maybe.map offset) components.transforms
  }


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

coordInRect : SVGRect -> SVGCoord -> Bool
coordInRect rect coord =
  coord.x > rect.position.x &&
  coord.x < rect.position.x + rect.size.width &&
  coord.y > rect.position.y &&
  coord.y < rect.position.y + rect.size.height

combineCoords : (Float -> Float -> Float) -> SVGCoord -> SVGCoord -> SVGCoord
combineCoords fn c1 c2 =
  { x = fn c1.x c2.x
  , y = fn c1.y c2.y
  }

addCoords = combineCoords (+)
subtractCoords = combineCoords (-)

rectOfCoords : SVGCoord -> SVGCoord -> SVGRect
rectOfCoords c1 c2 =
  let
    minMax x y =
      if x < y then (x, y) else (y, x)
    (xmin, xmax) = minMax c1.x c2.x
    (ymin, ymax) = minMax c1.y c2.y
  in
  { position = { x = xmin, y = ymin }
  , size = { width = xmax - xmin, height = ymax - ymin }
  }


transformCoord : Transform -> SVGCoord -> SVGCoord
transformCoord transform =
  addCoords transform.translate

transformInverse : Transform -> SVGCoord -> SVGCoord
transformInverse transform coord =
  subtractCoords coord transform.translate


-- transformation is by default from local to global (local -> global)


-- transformations of more top-level machines come first
transformTrail : Components -> EntityId -> List Transform
transformTrail components =
  let
    aux acc id =
      Dict.get id components.transforms
        |> Maybe.map
          (\transform ->
            case transform.parent of
              Nothing -> transform :: acc
              Just parent -> aux (transform :: acc) parent
          )
        |> Maybe.withDefault []
  in
  aux []

localOfGlobalCoord : Components -> EntityId -> SVGCoord -> SVGCoord
localOfGlobalCoord components id coord =
  List.foldl transformInverse coord (transformTrail components id)

globalOfLocalCoord : Components -> EntityId -> SVGCoord -> SVGCoord
globalOfLocalCoord components id coord =
  List.foldr transformCoord coord (transformTrail components id)

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

emptyMachine : SVGSize -> EMachine
emptyMachine size =
  { inputs = []
  , machineType = TAbs
  , size = size
  }

emptyTransform : SVGCoord -> Transform
emptyTransform translate =
  { translate = translate
  , parent = Nothing
  , children = Set.empty
  }

testComponents : Components
testComponents =
  initialComponents
    |> addMachine
         (emptyMachine { width = 60, height = 60 })
         (emptyTransform { x = 20, y = 20 })
    |> (\( components, childId ) ->
       addMachine
         (emptyMachine { width = 300, height = 300 })
         (emptyTransform { x = 100, y = 100 })
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

type alias Move =
  { clicked : MouseEvent
  , hovering : MouseEvent
  }

type Mode
  = ConnectMode
  | MachineMode (Maybe Move)
  | InputMode
  | DeleteMode
  | TransformMode (Maybe Move)

initialTransformMode =
  TransformMode Nothing

initialMachineMode =
  MachineMode Nothing

type Msg
  = NoOp
  | ModeChanged Mode
  | ScreenCtmGot (Maybe Ctm)
  | MouseMoved EntityId Ctm.ClientCoord
  | Clicked EntityId Ctm.ClientCoord
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
        CssPropPort.set ":root" "--ms-elapsed" (String.fromFloat model.msElapsed)
      )

    Clicked id clientCoord ->
      let
        svgCoord = svgOfClientCoord model clientCoord
        localCoord = localOfGlobalCoord model.components id svgCoord
        mouseEvent = { id = id, coord = localCoord }
        _ = Debug.log "msg" (msg, svgCoord)
      in
      case model.mode of
        DeleteMode -> noCmd { model | components = deleteEntity id model.components }

        MachineMode maybeInfo ->
          case maybeInfo of
            Nothing ->
              noCmd { model | mode = MachineMode (Just { clicked = mouseEvent, hovering = mouseEvent }) |> Debug.log "lastClick" }
            Just previous ->
              if isValidNewMachine { previous | hovering = mouseEvent } model.components
              then
                let
                  rect = rectOfCoords previous.clicked.coord localCoord
                  transform = emptyTransform rect.position
                  machine = emptyMachine rect.size
                  components = addMachine machine transform model.components |> (\(components_, childId) -> setParentChild id childId components_)
                in
                noCmd { model | components = components, mode = initialMachineMode }
              else
                noCmd model

        TransformMode maybeMove ->
          case maybeMove of
            Nothing ->
              let move = { clicked = mouseEvent, hovering = mouseEvent }
              in noCmd { model | mode = TransformMode (Just move) }
            Just move ->
              noCmd { model | mode = initialTransformMode, components = applyMove move model.components }

        _ -> noCmd model

    MouseMoved id clientCoord ->
      let
        svgCoord = svgOfClientCoord model clientCoord
        -- _ = Debug.log "msg" (msg, svgCoord)
      in
      case model.mode of
        TransformMode (Just move) ->
          let
            localCoord = localOfGlobalCoord model.components move.clicked.id svgCoord
            mouseEvent = { id = id, coord = localCoord }
          in
          noCmd { model | mode = TransformMode (Just { move | hovering = mouseEvent }) }
        MachineMode (Just previous) ->
          let
            localCoord = localOfGlobalCoord model.components previous.clicked.id svgCoord
            mouseEvent = { id = id, coord = localCoord }
          in
          noCmd { model | mode = MachineMode (Just { previous | hovering = mouseEvent }) }

        _ -> noCmd model

    _ -> let _ = Debug.log "msg" msg in noCmd model

svgOfClientCoord { screenCtm } =
  Ctm.svgOfClientCoord >> Ctm.matrixTransform screenCtm

applyMove : Move -> Components -> Components
applyMove move components =
  let offset = subtractCoords move.hovering.coord move.clicked.coord
  in offsetTransform move.clicked.id offset components

isValidNewMachine : Move -> Components -> Bool
isValidNewMachine { clicked, hovering } components =
  if clicked.id == hovering.id
  then
    let
      newRect = rectOfCoords clicked.coord hovering.coord
      clickedChildren =
        Dict.get clicked.id components.transforms
          |> Maybe.map .children
          |> Maybe.withDefault Set.empty
    in
    foldl2
      (\id machine transform isValid ->
        if isValid
        then
          let rect = { position = transform.translate, size = machine.size }
          in noIntersectionBB rect newRect
        else
          False
      ) True (DictE.keepOnly clickedChildren components.machines) components.transforms
  else
    False

noOverlapBB : Rectangular a -> Rectangular b -> Bool
noOverlapBB bb1 bb2 =
     bb1.position.x + bb1.size.width  < bb2.position.x
  || bb2.position.x + bb2.size.width  < bb1.position.x
  || bb1.position.y + bb1.size.height < bb2.position.y
  || bb2.position.y + bb2.size.height < bb1.position.y

insideBB : Rectangular a -> Rectangular b -> Bool
insideBB bbOut bbIn =
  bbOut.position.x < bbIn.position.x &&
  bbOut.position.y < bbIn.position.y &&
  bbOut.position.x + bbOut.size.width  > bbIn.position.x + bbIn.size.width &&
  bbOut.position.y + bbOut.size.height > bbIn.position.y + bbIn.size.height

noIntersectionBB : Rectangular a -> Rectangular b -> Bool
noIntersectionBB bb1 bb2 =
     insideBB bb1 bb2
  || insideBB bb2 bb1
  || noOverlapBB bb1 bb2






drawSvg : Model -> List (Svg Msg)
drawSvg model =
  let
    components =
      case model.mode of
        TransformMode (Just move) ->
          applyMove move model.components
        MachineMode (Just previous) ->
          let
            rect = rectOfCoords previous.clicked.coord previous.hovering.coord
            transform = emptyTransform rect.position
            machine = emptyMachine rect.size
          in
            addMachine machine transform model.components
              |> (\( components_, childId ) ->
                   ( setSvgClass childId "creating" components_, childId )
                 )
              |> (\(components_, childId) ->
                   setParentChild previous.clicked.id childId components_
                   |> (if isValidNewMachine previous model.components then identity else setInvalid childId)
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

drawBackground : EntityId -> Svg Msg
drawBackground id =
  let
    attributes =
     [ SE.on "click" <| JD.map (Clicked id) <| Ctm.pageCoordDecoder
     , SE.on "mousemove" <| JD.map (MouseMoved id) <| Ctm.pageCoordDecoder
     , SA.id "background"
     ] ++ rectAttributes { position = { x = 0, y = 0 }, size = { width = 800, height = 600 } }
  in
    Svg.rect attributes []

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

