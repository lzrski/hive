module Main exposing (..)

import Html exposing (Html, program, text, h1, div, pre)
import Time exposing (Time, second)
import AnimationFrame


type alias Model =
    Time


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "Hello!" ]
        , pre
            []
            [ text <| toString model ]
        ]


type Msg
    = NoOp
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( model + delta, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


init =
    ( 0, Cmd.none )


subscriptions model =
    AnimationFrame.diffs Tick
