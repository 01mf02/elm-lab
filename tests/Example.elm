module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Machines as M

typechecking : List Test
typechecking =
  [ test "And-Machine"
      \ _ ->
        Expect.equal (M.algW M.emptyConstMap M.emptyVarMap M.initFreshGen M.andMachine |> Result.map Tuple.second) (M.TyAbs M.boolTy (M.TyAbs M.boolTy M.boolTy))
  ]


suite : Test
suite =
  describe "Type-checking" typechecking
