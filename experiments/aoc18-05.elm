-- Solution for:
-- https://adventofcode.com/2018/day/5

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
  { content : String }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Polymer", value model.content, onInput Change ] []
    , div [] [ text (process model.content) ]
    ]

process =
  String.toList >> iterate polymerReact >> String.fromList

iterate f x =
  let y = f x in
  if x == y then x else iterate f y

polymerReact l =
  case l of
    x :: y :: xs ->
      if xor (Char.isUpper x) (Char.isUpper y) &&
         Char.toLower x == Char.toLower y
      then polymerReact xs
      else x :: polymerReact (y :: xs)
    
    _ -> l
