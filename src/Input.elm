module Input exposing (..)

import Dict exposing (Dict)

import Entity exposing (EntityId)

type alias Input =
  { parent : EntityId
  , position : Float
  }

type alias Inputs a =
  { a | inputs : Dict EntityId Input }

getInput : Inputs a -> EntityId -> Maybe Input
getInput components inputId =
  Dict.get inputId components.inputs

getParent : Inputs a -> EntityId -> Maybe EntityId
getParent components inputId =
  getInput components inputId
    |> Maybe.map .parent
