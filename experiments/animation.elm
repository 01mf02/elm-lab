import Animation exposing (px)
import Animation.Messenger
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes as SA


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { buttonStyle : Animation.Messenger.State Msg
    , rectStyle : Animation.Messenger.State Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { buttonStyle =
            Animation.style
                [ Animation.opacity 1.0
                ]
      , rectStyle =
            Animation.styleWith
            (Animation.spring
                { stiffness = 400
                , damping = 23 }
            )
            --Animation.style
                [ Animation.translate (Animation.px 0) (Animation.px 0)
                , Animation.scale3d 1.0 1.0 1.0
                ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.buttonStyle, model.rectStyle ]


type Msg
    = FadeInFadeOut
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FadeInFadeOut ->
            ( { model
                | buttonStyle =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 0
                            ]
                        , Animation.to
                            [ Animation.opacity 1
                            ]
                        ]
                        model.buttonStyle
                 , rectStyle = rectAnimation model.rectStyle
              }
            , Cmd.none
            )

        Animate animMsg ->
            let
                (newButtonStyle, buttonCmds) =
                    Animation.Messenger.update
                        animMsg
                        model.buttonStyle
                (newRectStyle, rectCmds) =
                    Animation.Messenger.update
                        animMsg
                        model.rectStyle
            in
            ( { model
                | buttonStyle = newButtonStyle
                , rectStyle = newRectStyle
              }
            , Cmd.batch [ buttonCmds, rectCmds ]
            )

rectAnimation =
    Animation.interrupt
       [ Animation.to
           [ Animation.scale3d 0.5 0.5 1.0
           ]
       , Animation.to
           [ Animation.translate (Animation.px 0) (Animation.px 100)
           ]
       , Animation.to
           [ Animation.scale3d 2.0 1.0 1.0
           ]
       , Animation.Messenger.send FadeInFadeOut
       ]

view : Model -> Html Msg
view model =
  div
  []
  [ div
        (Animation.render model.buttonStyle
            ++ [ onClick FadeInFadeOut
               , style "position" "relative"
               , style "margin" "100px auto"
               , style "padding" "25px"
               , style "width" "200px"
               , style "height" "200px"
               , style "background-color" "#268bd2"
               , style "color" "white"
               ]
        )
        [ text "Click to Animate!" ]
  , Svg.svg
    [ SA.width "200"
    , SA.height "200"
    , SA.viewBox "-100 -100 200 200"
    ]
    [ Svg.rect
        ([SA.x "-50"
        , SA.width "100"
        , SA.y "-100"
        , SA.height "100"
        ] ++ Animation.render model.rectStyle
        )
        []
    ]
  ]
