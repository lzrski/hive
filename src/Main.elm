module Main exposing (..)

import AnimationFrame
import Bug exposing (Bug, create)
import Html exposing (..)
import Html.Events exposing (onClick)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


type alias Model =
    { elapsed : Time
    , paused : Bool
    , bugs : List Bug
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
        , div [] [ scene model ]
        ]


scene model =
    Svg.g
        []
        (List.map
            bug
            model.bugs
        )
        |> Svg.render2d
            (BoundingBox2d.with
                { minX = -400
                , maxX = 400
                , minY = -400
                , maxY = 400
                }
            )


bug : Bug -> Svg Msg
bug bug =
    Svg.point2d
        { radius = 3
        , attributes =
            [ Svg.Attributes.stroke "red"
            , Svg.Attributes.fill "pink"
            ]
        }
        bug.position


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
    ( { elapsed = 0
      , paused = False
      , bugs =
            [ Bug.create <| Point2d.fromCoordinates ( 10, 10 )
            , Bug.create <| Point2d.fromCoordinates ( -10, 20 )
            ]
      }
    , Cmd.none
    )


subscriptions model =
    if model.paused then
        Sub.none
    else
        AnimationFrame.diffs Frame
