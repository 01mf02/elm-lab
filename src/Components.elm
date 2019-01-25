module Components exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Connection exposing (..)
import Machine exposing (..)
import Transform exposing (Transform, rootTransformId, setParentChild, reorientParentChild)

type alias EntityId = Int

type alias Components =
  { nextId : EntityId
  , names : Dict EntityId String
  , machines : Dict EntityId EMachine
  , connections : Dict EntityId Connection
  , transforms : Dict EntityId Transform
  , svgClasses : Dict EntityId String
  , invalids : Set EntityId
  }
