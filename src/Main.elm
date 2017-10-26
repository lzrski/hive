module Main exposing (..)

import AnimationFrame
import Bug exposing (Bug)
import Food exposing (Food)
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
    , entities : List Entity
    }


type Entity
    = Bug Bug
    | Food Food


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
        , div [] [ sceneView model ]
        ]


sceneView model =
    Svg.g
        []
        (List.map
            entityView
            model.entities
        )
        |> Svg.render2d
            (BoundingBox2d.with
                { minX = -400
                , maxX = 400
                , minY = -400
                , maxY = 400
                }
            )


entityView entity =
    case entity of
        Bug bug ->
            bugView bug

        Food food ->
            foodView food


bugView : Bug -> Svg Msg
bugView bug =
    Svg.point2d
        { radius = 3
        , attributes =
            [ Svg.Attributes.stroke "red"
            , Svg.Attributes.fill "pink"
            ]
        }
        bug.position


foodView : Food -> Svg Msg
foodView { position } =
    Svg.point2d
        { radius = 2
        , attributes =
            [ Svg.Attributes.stroke "green"
            , Svg.Attributes.fill "lime"
            ]
        }
        position


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
            ( { model
                | elapsed = model.elapsed + delta
                , entities =
                    model.entities
                        |> List.map (updateEntity delta)
              }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )


updateEntity delta entity =
    case entity of
        Bug bug ->
            Bug (Bug.update delta bug)

        Food food ->
            Food (Food.update delta food)


init =
    ( { elapsed = 0
      , paused = False
      , entities =
            [ Bug <| Bug.create ( 10, 10 )
            , Bug <| Bug.create ( -10, 20 )
            , Food <| Food.create ( 20, -20 )
            ]
      }
    , Cmd.none
    )


subscriptions model =
    if model.paused then
        Sub.none
    else
        AnimationFrame.diffs Frame
