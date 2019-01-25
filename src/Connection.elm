module Connection exposing (..)

type alias EntityId = Int

type ConnectionEndpoint
  = Output EntityId
  | Input EntityId Int

type alias Connection =
  { from : ConnectionEndpoint
  , to : ConnectionEndpoint
  }
