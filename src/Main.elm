{-
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


module Main exposing (main)

-- Geometry
-- View related

import AnimationFrame
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


type alias Model =
    { elapsed : Time
    , paused : Bool
    , world : World
    }


type alias World =
    { seed : Id
    , entities : Entities
    }


type alias Id =
    Int


type alias Entities =
    Dict Id Entity


type Entity
    = Bug
        { position : Point2d
        , nutrition : Float
        , mass : Float
        }
    | Food
        { position : Point2d
        , quantity : Float
        }


type Action
    = Idle
    | Crawl Direction2d
    | Consume Id


type alias Actions =
    Dict Id Action



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
        { position = Point2d.fromCoordinates coordinates
        , nutrition = 1.0
        , mass = 1.0
        }


food : ( Float, Float ) -> Entity
food coordinates =
    Food
        { position = Point2d.fromCoordinates coordinates
        , quantity = 1.0
        }



-- Update


world_update : Time -> World -> World
world_update delta world =
    world.entities
        |> reason
        |> (\actions -> perform delta actions world)


reason : Entities -> Actions
reason entities =
    entities
        |> Dict.map
            (\id entity ->
                let
                    _ =
                        Debug.log "Reasoning" entity
                in
                    case entity of
                        Bug bug ->
                            case reachableFood entities bug of
                                Just target ->
                                    Consume target

                                Nothing ->
                                    attraction entities bug
                                        |> Vector2d.direction
                                        |> Maybe.withDefault Direction2d.x
                                        |> Crawl

                        _ ->
                            Idle
            )


reachableFood :
    Entities
    -> { a | position : Point2d }
    -> Maybe Id
reachableFood entities { position } =
    entities
        |> Dict.foldl
            (\id entity result ->
                case entity of
                    Food food ->
                        let
                            distance =
                                Point2d.distanceFrom food.position position
                        in
                            if distance < 5 then
                                Just id
                            else
                                result

                    _ ->
                        result
            )
            Nothing


{-| Calculate the attraction vector for a bug at a given position.

The bug will consider every Food and Bug entity in the environment and assign value to it based on it's distance (the further the food is, the less attraction value it has). Then resulting vectors will be summed.

TODO: Take into account how hungry is the bug (hungry goes is more affected by distance - just go to the nearest food).

-}
attraction :
    Entities
    -> { a | position : Point2d }
    -> Vector2d
attraction entities { position } =
    Dict.foldl
        (\_ entity current ->
            case entity of
                {- Bugs are attracted to food -}
                Food food ->
                    let
                        direction =
                            Direction2d.from position food.position

                        value =
                            1 / Point2d.distanceFrom food.position position
                    in
                        case direction of
                            Nothing ->
                                current

                            Just direction ->
                                Vector2d.sum current <|
                                    Vector2d.with
                                        { length = value
                                        , direction = direction
                                        }

                {- Bugs avoid direct contact with other bugs -}
                Bug other ->
                    let
                        direction =
                            Direction2d.from position other.position

                        distance =
                            Point2d.distanceFrom other.position position

                        value =
                            if distance == 0 then
                                0
                            else
                                {- deterence is very strong when bugs are close, but weakens significantly with distance -}
                                -10 / (distance ^ 3)
                    in
                        case direction of
                            Nothing ->
                                {- probably other is self -}
                                current

                            Just direction ->
                                Vector2d.with
                                    { length = value
                                    , direction = direction
                                    }
                                    |> Vector2d.sum current
        )
        Vector2d.zero
        entities


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

                        Just (Bug state) ->
                            case action of
                                Idle ->
                                    world

                                Crawl direction ->
                                    let
                                        distance =
                                            delta * 0.02

                                        energy =
                                            delta * 0.0001

                                        newState =
                                            state
                                                |> move direction distance
                                                |> burn energy
                                    in
                                        if newState.nutrition <= 0 then
                                            entities
                                                |> Dict.remove id
                                                |> World seed
                                                |> world_insert
                                                    (Food
                                                        { position = newState.position
                                                        , quantity = newState.mass
                                                        }
                                                    )
                                        else
                                            entities
                                                |> Dict.insert id (Bug newState)
                                                |> World seed

                                Consume target ->
                                    case Dict.get target entities of
                                        Just (Food food) ->
                                            let
                                                amount =
                                                    Basics.min food.quantity (0.0001 * delta)

                                                remaining =
                                                    food.quantity - amount

                                                remains =
                                                    if remaining > 0 then
                                                        Just <| Food { food | quantity = remaining }
                                                    else
                                                        Nothing

                                                bug =
                                                    Bug
                                                        { state
                                                            | nutrition =
                                                                state.nutrition + amount
                                                        }

                                                newEntities =
                                                    entities
                                                        |> Dict.update target (always remains)
                                                        |> Dict.insert id bug
                                            in
                                                { world
                                                    | entities = newEntities
                                                }

                                        _ ->
                                            world

                        -- Food can take no actions ATM
                        Just (Food food) ->
                            world
            )
            world


move :
    Direction2d
    -> Float
    -> { a | position : Point2d }
    -> { a | position : Point2d }
move direction distance state =
    let
        { position } =
            state

        displacement =
            Vector2d.with { length = distance, direction = direction }
    in
        { state | position = Point2d.translateBy displacement position }


burn :
    Float
    -> { a | nutrition : Float }
    -> { a | nutrition : Float }
burn energy state =
    { state | nutrition = Basics.max 0 (state.nutrition - energy) }


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
        [ div [] [ sceneView model.world ]
        , div []
            [ if model.paused then
                button [ onClick Resume ] [ Html.text "Resume" ]
              else
                button [ onClick Pause ] [ Html.text "Pause" ]
            ]
        , div []
            [ code
                []
                [ Html.text <| toString model ]
            ]
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
                |> world_insert (bug ( 0, 0 ))
                |> world_insert (bug ( 0, 10 ))
                |> world_insert (bug ( 10, 0 ))
                |> world_insert (bug ( 19, 4 ))
                |> world_insert (bug ( -10, 3 ))
                |> world_insert (bug ( -100, 200 ))
                |> world_insert (food ( 210, -10 ))
                |> world_insert (food ( 200, -20 ))
                |> world_insert (food ( 200, -50 ))
                |> world_insert (food ( 180, -100 ))
                |> world_insert (food ( 150, -150 ))
                |> world_insert (food ( 100, 290 ))
                |> world_insert (food ( 100, -200 ))
                |> world_insert (food ( 0, -150 ))
                |> world_insert (food ( 20, -100 ))
                |> world_insert (food ( -200, 300 ))
                |> world_insert (food ( -200, -200 ))
                |> world_insert (food ( -200, -300 ))
      }
    , Cmd.none
    )


subscriptions model =
    if model.paused then
        Sub.none
    else
        AnimationFrame.diffs Frame
