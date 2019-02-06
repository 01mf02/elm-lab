module Components exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Connection exposing (..)
import Entity exposing (EntityId)
import Input exposing (Input)
import Machine exposing (..)
import Transform exposing (Transform)

type alias Components =
  { nextId : EntityId
  , names : Dict EntityId String
  , machines : Dict EntityId EMachine
  , inputs : Dict EntityId Input
  , connections : Dict EntityId Connection
  , transforms : Dict EntityId Transform
  , svgClasses : Dict EntityId String
  , invalids : Set EntityId
  }


initialComponents : Components
initialComponents =
  { nextId = rootTransformId + 1
  , names = Dict.empty
  , machines = Dict.empty
  , inputs = Dict.empty
  , connections = Dict.empty
  , transforms = Dict.singleton rootTransformId Transform.root
  , svgClasses = Dict.empty
  , invalids = Set.empty
  }

rootTransformId : EntityId
rootTransformId = 0


foldl : (EntityId -> a -> b -> b) -> b -> Dict EntityId a -> b
foldl =
    Dict.foldl

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


addMachine : EMachine -> Transform -> Components -> ( Components, EntityId )
addMachine machine transform components =
  ( { components
      | nextId = components.nextId + 1
      , machines = Dict.insert components.nextId machine components.machines
      , transforms = Dict.insert components.nextId transform components.transforms
    }
  , components.nextId
  )

addInput : EntityId -> Input -> Components -> ( Components, EntityId )
addInput machineId input components =
  let
    updateMachine machine =
      { machine | inputs = components.nextId :: machine.inputs }
  in
  ( { components
      | nextId = components.nextId + 1
      , machines = Dict.update machineId (Maybe.map updateMachine) components.machines
      , inputs = Dict.insert components.nextId input components.inputs
    }
  , components.nextId
  )

addConnection : Connection -> Components -> ( Components, EntityId )
addConnection connection components =
  let
    updateMachine machine =
      { machine | connections = Set.insert components.nextId machine.connections }
    updateMachines machines =
      Maybe.map
        (\connectionMachine ->
          Dict.update connectionMachine (Maybe.map updateMachine) machines
        )
        (Connection.getMachineField components connection)
        |> Maybe.withDefault machines
  in
  ( { components
      | nextId = components.nextId + 1
      , machines = updateMachines components.machines
      , connections = Dict.insert components.nextId connection components.connections
    }
  , components.nextId
  )

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

setInvalid : EntityId -> Components -> Components
setInvalid id components =
  { components | invalids = Set.insert id components.invalids }

-- remove children?
deleteEntity : EntityId -> Components -> Components
deleteEntity id =
  deleteSpecific id >> deleteGeneric id

deleteSpecific : EntityId -> Components -> Components
deleteSpecific id components =
  if Dict.member id components.machines
  then deleteMachine id components
  else if Dict.member id components.inputs
  then deleteInput id components
  else components

deleteGeneric : EntityId -> Components -> Components
deleteGeneric id components =
  { nextId = components.nextId
  , names = Dict.remove id components.names
  , machines = Dict.remove id components.machines
  , inputs = Dict.remove id components.inputs
  , connections = Dict.remove id components.connections
  , transforms = Dict.remove id components.transforms
  , svgClasses = Dict.remove id components.svgClasses
  , invalids = Set.remove id components.invalids
  }

deleteMachine : EntityId -> Components -> Components
deleteMachine id components =
  case getMachine components id of
    Nothing -> components
    Just machine ->
      let
        validConnection =
          Connection.getConnection components
            >> Maybe.map (\connection -> connection.from.id /= id)
            >> Maybe.withDefault False
        updateParent parentMachine =
          { parentMachine | connections = Set.filter validConnection parentMachine.connections }
        updateMachines =
          case Transform.getParent components id of
            Nothing -> identity
            Just parentId -> Dict.update parentId (Maybe.map updateParent)
      in

      -- TODO: delete inside connections
      List.foldl deleteInput components machine.inputs
        |> (\components_ -> { components_ | machines = updateMachines components_.machines })
        |> deleteGeneric id



-- TODO: refactor!
deleteInputSurrounding inputId components =
  let
    surrounding =
      Input.getParent components inputId
        |> Maybe.andThen (Transform.getParent components)
    validConnection =
      Connection.getConnection components
        >> Maybe.map (Connection.hasEndpoint inputId >> not)
        >> Maybe.withDefault False
    updateMachine machine =
      { machine | connections = Set.filter validConnection machine.connections }
  in
  case surrounding of
    Nothing -> identity
    Just m -> Dict.update m (Maybe.map updateMachine)

deleteInputInside inputId components =
  let
    parentId = Input.getParent components inputId
    validConnection =
      Connection.getConnection components
        >> Maybe.map (Connection.hasEndpoint inputId >> not)
        >> Maybe.withDefault False
    updateMachine machine =
      { machine | connections = Set.filter validConnection machine.connections }
  in
  case parentId of
    Nothing -> identity
    Just m -> Dict.update m (Maybe.map updateMachine)


deleteInput : EntityId -> Components -> Components
deleteInput inputId components =
  case Input.getInput components inputId of
    Nothing -> components
    Just input ->
      let
        updateMachines =
          deleteInputInside inputId components
            >> deleteInputSurrounding inputId components
      in
      { components | machines = updateMachines components.machines }
        |> deleteGeneric inputId
