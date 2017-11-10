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
import Char
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Keyboard
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Random exposing (initialSeed)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


type alias Model =
    { elapsed : Time
    , paused : Bool
    , count : Dict String Int
    , zoom : Float
    , translation : Vector2d
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
    | Seed
        { position : Point2d
        , size : Float
        , nutrient : Float
        }


type Action
    = Idle
    | Crawl Direction2d
    | Consume Id
    | Spawn
    | Ripe


type alias Actions =
    Dict Id Action



-- Public constructors


world_empty : World
world_empty =
    { seed = 0
    , entities = Dict.empty
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
                case entity of
                    Bug state ->
                        if state.mass > 1 then
                            Spawn
                        else if state.nutrition > state.mass then
                            Idle
                        else
                            case reachableFood entities state of
                                Just target ->
                                    Consume target

                                Nothing ->
                                    attraction entities state
                                        |> Vector2d.direction
                                        |> Maybe.withDefault Direction2d.x
                                        |> Crawl

                    Seed state ->
                        if state.nutrient > 0 then
                            Idle
                        else
                            Ripe

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
    -> { a | position : Point2d, nutrition : Float }
    -> Vector2d
attraction entities { position, nutrition } =
    Dict.foldl
        (\_ entity current ->
            case entity of
                {- Bugs are attracted to food -}
                Food piece ->
                    let
                        direction =
                            Direction2d.from position piece.position

                        distance =
                            Point2d.distanceFrom piece.position position

                        value =
                            1 / (distance ^ (2 - nutrition / 2))
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
                                {- deterence is very strong when bugs are close, but weakens significantly with distance. Also bigger bugs are more scary -}
                                -10 * other.mass / (distance ^ 3)
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

                _ ->
                    current
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
                                    {- bugs grow when idle -}
                                    let
                                        newState =
                                            { state
                                                | mass = state.mass + delta / 1000
                                                , nutrition = state.nutrition - delta / 100
                                            }
                                    in
                                        world
                                            |> world_replace id (Bug newState)

                                Crawl direction ->
                                    let
                                        distance =
                                            delta * 0.02

                                        energy =
                                            delta * 0.00001

                                        newState =
                                            state
                                                |> move direction distance
                                                |> burn energy
                                    in
                                        if newState.nutrition <= 0 then
                                            world
                                                |> world_remove id
                                                |> world_insert
                                                    (Seed
                                                        { position = newState.position
                                                        , size = 0.01
                                                        , nutrient = newState.mass
                                                        }
                                                    )
                                        else
                                            world
                                                |> world_replace id (Bug newState)

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

                                Spawn ->
                                    let
                                        offset =
                                            Vector2d.with
                                                { length = 1
                                                , direction = Direction2d.x
                                                }

                                        position =
                                            Point2d.translateBy
                                                offset
                                                state.position

                                        offspring =
                                            Bug
                                                { position = position
                                                , nutrition = 1
                                                , mass = 0.1
                                                }

                                        parent =
                                            Bug
                                                { state
                                                    | mass = state.mass - 0.1
                                                    , nutrition = state.nutrition - 0.3
                                                }
                                    in
                                        world
                                            |> world_insert offspring
                                            |> world_replace id parent

                                _ ->
                                    world

                        -- Food can take no actions ATM
                        Just (Food state) ->
                            world

                        Just (Seed state) ->
                            case action of
                                Idle ->
                                    let
                                        nutrition =
                                            Basics.min (delta * 0.000002) state.nutrient

                                        growth =
                                            nutrition * 5

                                        seed =
                                            Seed
                                                { state
                                                    | size = state.size + growth
                                                    , nutrient = state.nutrient - nutrition
                                                }
                                    in
                                        world |> world_replace id seed

                                Ripe ->
                                    let
                                        food =
                                            Food { position = state.position, quantity = state.size }
                                    in
                                        world
                                            |> world_remove id
                                            |> world_insert food

                                _ ->
                                    world
            )
            world



-- World mutations


world_insert : Entity -> World -> World
world_insert entity { seed, entities } =
    { seed = seed + 1
    , entities = Dict.insert seed entity entities
    }


world_replace : Id -> Entity -> World -> World
world_replace id entity world =
    { world | entities = Dict.insert id entity world.entities }


world_remove id world =
    { world | entities = Dict.remove id world.entities }


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
    {--
    TODO: Stretch to container size and make background hsla(211, 76%, 10%, 1)
    --}
    div []
        [ div [ style "background:  hsl(200, 30%, 20%)" ]
            [ sceneView model ]
        , div []
            [ if model.paused then
                button [ onClick Resume ] [ Html.text "Resume" ]
              else
                button [ onClick Pause ] [ Html.text "Pause" ]
            ]
        , div []
            [ code
                []
                [ Html.text <| toString model.elapsed ]
            , br [] []
            , code
                [ onClick Count ]
                [ Html.text <| toString model.count ]
            ]
        ]


sceneView { zoom, translation, world } =
    world.entities
        |> Dict.values
        |> List.map entityView
        |> Svg.g []
        |> Svg.translateBy translation
        |> Svg.scaleAbout (Point2d.fromCoordinates ( 0, 0 )) zoom
        |> Svg.render2d
            (BoundingBox2d.with
                { minX = -800
                , maxX = 800
                , minY = -800
                , maxY = 800
                }
            )


entityView : Entity -> Svg Msg
entityView entity =
    case entity of
        Bug state ->
            let
                size =
                    3 * state.mass

                saturation =
                    round ((state.nutrition / state.mass) * 100)
            in
                Svg.point2d
                    { radius = size
                    , attributes =
                        [ stroke
                            ("hsl(0, "
                                ++ (toString saturation)
                                ++ "%, 30%)"
                            )
                        , fill
                            ("hsl(0, "
                                ++ toString (saturation)
                                ++ "%, 80%)"
                            )
                        ]
                    }
                    state.position

        Food state ->
            Svg.point2d
                { radius = 2 * state.quantity
                , attributes =
                    [ stroke "green"
                    , fill "hsl(100, 80%, 80%)"
                    ]
                }
                state.position

        Seed state ->
            let
                saturation =
                    round (state.size * 80)

                outline =
                    3 * state.nutrient

                radius =
                    2 * state.size + outline
            in
                Svg.point2d
                    { radius = radius
                    , attributes =
                        [ stroke "hsl(0, 0%, 30%)"
                        , strokeWidth <| (toString outline) ++ "px"
                        , fill
                            ("hsl(100, "
                                ++ toString (saturation)
                                ++ "%, 80%)"
                            )
                        ]
                    }
                    state.position


type Msg
    = NoOp
    | Frame Time
    | Pause
    | Resume
    | Count
    | ZoomOut
    | ZoomIn
    | Pan Vector2d


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Count ->
            { model
                | count = count_entities model.world.entities
            }
                ! []

        ZoomOut ->
            { model | zoom = model.zoom * 0.9 } ! []

        ZoomIn ->
            { model | zoom = model.zoom / 0.9 } ! []

        Pan vector ->
            { model
                | translation =
                    Vector2d.sum model.translation vector
            }
                ! []

        Frame delay ->
            let
                delta =
                    {- Limit virtual detla to 32ms. In effect, with framerates below 30fps the simulation will slow down from the users perspective, but the physics will remain accurate. For high frame rates real delta will be used, making physics even more accurate.

                       Additional bonus - if browser tab is not visible, it will effectively pause the game.
                    -}
                    Basics.min delay 32
            in
                { model
                    | elapsed = model.elapsed + delta
                    , world = world_update delta model.world
                }
                    ! []

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )


count_entities =
    Dict.foldl
        (\id entity counter ->
            let
                name =
                    entity
                        |> toString
                        |> String.split " "
                        |> List.head
                        |> Maybe.withDefault "Wa0000t?"
            in
                Dict.update name
                    (Maybe.withDefault 0
                        >> (+) 1
                        >> Just
                    )
                    counter
        )
        Dict.empty


world_populate :
    (( Float, Float ) -> Entity)
    -> Int
    -> World
    -> World
world_populate constructor count world =
    let
        generator =
            Random.list count <|
                Random.pair
                    (Random.int -800 800)
                    (Random.int -800 800)

        ( positions, _ ) =
            Random.step generator <| Random.initialSeed world.seed
    in
        positions
            |> List.map (\( x, y ) -> constructor ( toFloat x, toFloat y ))
            |> List.foldl (\entity current -> world_insert entity current) world


init =
    ( { elapsed = 0
      , paused = False
      , count = Dict.empty
      , zoom = 1
      , translation = Vector2d.fromComponents ( 0, 0 )
      , world =
            world_empty
                |> world_populate bug 10
                |> world_populate food 100
      }
    , Cmd.none
    )


subscriptions model =
    if model.paused then
        -- Sub.none
        Sub.batch [ Keyboard.presses handleKeyboard ]
    else
        Sub.batch
            [ AnimationFrame.diffs Frame
            , Keyboard.presses handleKeyboard
            , Time.every
                (second * 3)
                (always Count)
            ]


handleKeyboard keycode =
    let
        char =
            Char.fromCode keycode
    in
        case char of
            '-' ->
                ZoomOut

            '+' ->
                ZoomIn

            'h' ->
                Pan <| Vector2d.fromComponents ( 10, 0 )

            'j' ->
                Pan <| Vector2d.fromComponents ( 0, 10 )

            'k' ->
                Pan <| Vector2d.fromComponents ( 0, -10 )

            'l' ->
                Pan <| Vector2d.fromComponents ( -10, 0 )

            _ ->
                NoOp
