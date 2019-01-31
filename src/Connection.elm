module Connection exposing (..)

import Entity exposing (EntityId)

type Type = Input | Output

{-
if from is Input, then it is the input of the outer machine
if from is Output, it is the output of an inner machine
if to is Input, then it is the input of an inner machine
if to is Output, then it is the output of the outer machine
-}

type alias Connection =
  { from : ( EntityId, Type )
  , to : ( EntityId, Type )
  }
