module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Machines


suite : Test
suite =
  test "combArgs" <|
    \_ ->
      Machines.combArgs [Just 2, Nothing] [Nothing, Just 1, Nothing]
        |> Expect.equal (Just [Just 2,Just 1,Nothing])
