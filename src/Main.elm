module Main exposing (main)

import Time exposing (Time, second)
import Dict exposing (Dict)
import AnimationFrame


-- Geometry

import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


-- View related

import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import OpenSolid.Svg as Svg


type alias Model =
    { elapsed : Time
    , paused : Bool
    , world : World
    }


type alias World =
    { seed : ID
    , entities : Entities
    }


type alias ID =
    Int


type alias Entities =
    Dict ID Entity


type Entity
    = Bug
        { position : Point2d
        , nutrition : Float
        }
    | Food
        { position : Point2d
        , quantity : Float
        }


type Action
    = Idle
    | Crawl Direction2d
    | Consume ID


type alias Actions =
    Dict ID Action



-- Public constructors


world_empty : World
world_empty =
    { seed = 0
    , entities = Dict.empty
    }


world_insert : Entity -> World -> World
world_insert entity { seed, entities } =
    { seed = seed + 1
    , entities = Dict.insert seed entity entities
    }


bug : ( Float, Float ) -> Entity
bug coordinates =
    Bug
        { position = (Point2d.fromCoordinates coordinates)
        , nutrition = 1.0
        }


food : ( Float, Float ) -> Entity
food coordinates =
    Food
        { position = (Point2d.fromCoordinates coordinates)
        , quantity = 1.0
        }



-- Update


world_update : Time -> World -> World
world_update delta world =
    world.entities
        |> reason
        |> \actions -> perform delta actions world


reason : Entities -> Actions
reason =
    Dict.map
        (\key entity ->
            let
                _ =
                    Debug.log "Reasoning" entity
            in
                Idle
        )


perform : Time -> Actions -> World -> World
perform delta actions world =
    {- TODO: Fold actions into a new world, starting with current -}
    actions
        |> Dict.foldl
            (\id action world ->
                let
                    _ =
                        Debug.log "Performing action" action

                    { entities, seed } =
                        world

                    entity =
                        Dict.get id entities
                in
                    case entity of
                        -- Entity was previously removed. Move on.
                        Nothing ->
                            world

                        -- TODO: Perform the action of a bug
                        Just (Bug bug) ->
                            case action of
                                Idle ->
                                    world

                                _ ->
                                    world

                        -- Food can take no actions ATM
                        Just (Food food) ->
                            world
            )
            world


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
        [ code
            []
            [ Html.text <| toString model ]
        , if model.paused then
            button [ onClick Resume ] [ Html.text "Resume" ]
          else
            button [ onClick Pause ] [ Html.text "Pause" ]
        , div [] [ sceneView model.world ]
        ]


sceneView world =
    world.entities
        |> Dict.values
        |> List.map entityView
        |> Svg.g []
        |> Svg.render2d
            (BoundingBox2d.with
                { minX = -400
                , maxX = 400
                , minY = -400
                , maxY = 400
                }
            )


entityView : Entity -> Svg Msg
entityView entity =
    case entity of
        Bug bug ->
            Svg.point2d
                { radius = 3
                , attributes =
                    [ Svg.Attributes.stroke "red"
                    , Svg.Attributes.fill "pink"
                    ]
                }
                bug.position

        Food food ->
            Svg.point2d
                { radius = 2
                , attributes =
                    [ Svg.Attributes.stroke "green"
                    , Svg.Attributes.fill "lime"
                    ]
                }
                food.position


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
                , world = world_update delta model.world
              }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )


init =
    ( { elapsed = 0
      , paused = False
      , world =
            world_empty
                |> world_insert (bug ( 10, 10 ))
                |> world_insert (bug ( -10, 20 ))
                |> world_insert (food ( 20, -20 ))
      }
    , Cmd.none
    )


subscriptions model =
    if model.paused then
        Sub.none
    else
        AnimationFrame.diffs Frame
