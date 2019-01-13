module Builtin exposing (..)

import Type exposing (..)

type Value
  = IntValue Int

intTy = TyConst "int" []

valueEquals : Value -> Value -> Bool
valueEquals x y =
  case (x, y) of
    (IntValue ix, IntValue iy) -> ix == iy

stringFromValue v =
  case v of
    IntValue i -> String.fromInt i

valueType v =
  case v of
    IntValue _ -> TyConst "int" []

intfun2 f =
  ( TyAbs intTy (TyAbs intTy intTy)
  , \ args ->
      case args of
        [IntValue x, IntValue y] -> Just (IntValue (f x y))
        _ -> Nothing
  )

builtinFunctions =
  [ ("add", intfun2 (+))
  , ("sub", intfun2 (-))
  , ("mul", intfun2 (*))
  ]
