module Connection exposing (..)

type alias EntityId = Int

type alias Connection =
  { from : EntityId
  , to : EntityId
  }
