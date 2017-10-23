module Main exposing (..)

import AnimationFrame
import Html exposing (Html, button, div, h1, pre, program, text)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


type alias Model =
    { elapsed : Time
    , paused : Bool
    }


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
        , if model.paused then
            button [ onClick Resume ] [ text "Resume" ]
          else
            button [ onClick Pause ] [ text "Pause" ]
        ]


type Msg
    = NoOp
    | Frame Time
    | Pause
    | Resume


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Frame delta ->
            ( { model | elapsed = model.elapsed + delta }, Cmd.none )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )


init =
    ( Model 0 False, Cmd.none )


subscriptions model =
    if model.paused then
        Sub.none
    else
        AnimationFrame.diffs Frame
