module Main exposing (..)

import AnimationFrame
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
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
            [ Html.text "Hello!" ]
        , pre
            []
            [ Html.text <| toString model ]
        , if model.paused then
            button [ onClick Resume ] [ Html.text "Resume" ]
          else
            button [ onClick Pause ] [ Html.text "Pause" ]
        , div []
            [ svg
                [ width "800px", height "800px", viewBox "0 0 800 800" ]
                [ rect
                    [ x "200"
                    , y "200"
                    , width "100"
                    , height "100"
                    , rx "20"
                    , ry "20"
                    ]
                    []
                ]
            ]
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
