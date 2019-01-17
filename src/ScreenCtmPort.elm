port module ScreenCtmPort exposing (request, receive)

import Json.Decode as JD
import Json.Encode as JE
import Result

import Ctm exposing (Ctm)

port requestScreenCtm : JE.Value -> Cmd msg

port receiveScreenCtm : (JE.Value -> msg) -> Sub msg

request : String -> Cmd msg
request =
  JE.string >> requestScreenCtm

receive : (Maybe Ctm -> msg) -> Sub msg
receive fun =
  receiveScreenCtm (JD.decodeValue Ctm.ctmDecoder >> Result.toMaybe >> fun)
