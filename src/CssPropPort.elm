port module CssPropPort exposing (set)

import Json.Encode as JE

-- Thanks to harrysarson for the CSS custom variable workaround in
-- <https://github.com/elm/html/issues/177>

port setCssProp : JE.Value -> Cmd msg

set selector prop value =
  setCssProp (JE.list JE.string [selector, prop, value])
