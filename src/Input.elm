module Input exposing (..)

import Entity exposing (EntityId)

type alias Input =
  { parent : EntityId
  , position : Float
  }
