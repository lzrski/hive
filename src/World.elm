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


module World exposing (World, init, update, view, stats)

import Dict exposing (Dict)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Random exposing (initialSeed)
import Time exposing (Time)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias World =
    { seed : Id
    , entities : Entities
    }


init : World
init =
    empty
        |> populate bug_create 20
        |> populate food_create 100
        |> populate predator_create 5


update : Time -> World -> World
update delta world =
    world.entities
        |> reason
        |> (\actions -> perform delta actions world)


view : World -> Svg msg
view world =
    world.entities
        |> Dict.values
        |> List.map entityView
        |> Svg.g []


stats : World -> Dict String Float
stats { entities } =
    let
        update :
            Id
            -> Entity
            -> Dict String Float
            -> Dict String Float
        update id entity counter =
            Dict.update (label entity)
                (Maybe.withDefault 0
                    >> (+) 1
                    >> Just
                )
                counter

        label entity =
            entity
                |> toString
                |> String.split " "
                |> List.head
                |> Maybe.withDefault "Say wat?"
    in
        entities
            |> Dict.foldl update Dict.empty


type alias Id =
    Int


type alias Entities =
    Dict Id Entity


empty : World
empty =
    { seed = 0
    , entities = Dict.empty
    }


populate :
    (( Float, Float ) -> Entity)
    -> Int
    -> World
    -> World
populate constructor count world =
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
            |> List.foldl (\entity current -> insert entity current) world


bug_create : ( Float, Float ) -> Entity
bug_create coordinates =
    Bug
        { position = Point2d.fromCoordinates coordinates
        , nutrition = 1.0
        , mass = 1.0
        }


food_create : ( Float, Float ) -> Entity
food_create coordinates =
    Food
        { position = Point2d.fromCoordinates coordinates
        , quantity = 1.0
        }


predator_create : ( Float, Float ) -> Entity
predator_create coordinates =
    Predator
        { position = Point2d.fromCoordinates coordinates
        , attachedTo = Nothing
        , nutrition = 1.0
        , mass = 1.0
        }


type alias Emotions =
    { attraction : Vector2d
    , prey : Maybe Id
    }


reason : Entities -> Actions
reason entities =
    let
        delegate id entity =
            case entity of
                Bug state ->
                    bug state

                Seed state ->
                    seed state

                Predator state ->
                    predator state

                Food state ->
                    Idle

        bug state =
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

        seed state =
            if state.nutrient > 0 then
                Idle
            else
                Ripe

        predator state =
            let
                hunger =
                    state.mass / state.nutrition

                consider :
                    Id
                    -> Entity
                    -> Emotions
                    -> Emotions
                consider id entity emotions =
                    case entity of
                        Bug bug ->
                            bug.position
                                |> Vector2d.from state.position
                                |> Vector2d.lengthAndDirection
                                |> Maybe.map
                                    (\( distance, direction ) ->
                                        if emotions.prey /= Nothing then
                                            -- Ignore if the predator is already after a prey
                                            emotions
                                        else if distance > (150 ^ hunger) then
                                            -- Ignore if it's too far away
                                            emotions
                                        else if distance < 5 then
                                            -- Attack if it's close enough
                                            { emotions | prey = Just id }
                                        else
                                            { emotions
                                                | attraction =
                                                    Vector2d.with
                                                        { length = 1 / (distance ^ 3)
                                                        , direction = direction
                                                        }
                                                        |> Vector2d.sum emotions.attraction
                                            }
                                    )
                                |> Maybe.withDefault
                                    { emotions
                                        | prey = Just id
                                    }

                        Predator other ->
                            let
                                repulsion =
                                    other.position
                                        |> Vector2d.from state.position
                                        |> Vector2d.lengthAndDirection
                            in
                                if other.attachedTo == Nothing then
                                    case repulsion of
                                        Nothing ->
                                            emotions

                                        Just ( distance, direction ) ->
                                            if distance < (120 ^ hunger) then
                                                { emotions
                                                    | attraction =
                                                        Vector2d.with
                                                            { length = 1 / distance ^ 3
                                                            , direction = Direction2d.flip direction
                                                            }
                                                            |> Vector2d.sum emotions.attraction
                                                }
                                            else
                                                emotions
                                else
                                    emotions

                        _ ->
                            emotions

                decide : Emotions -> Action
                decide emotions =
                    case emotions.prey of
                        Nothing ->
                            emotions.attraction
                                |> Vector2d.direction
                                |> Maybe.map Crawl
                                |> Maybe.withDefault Idle

                        Just prey ->
                            Consume prey
            in
                case state.attachedTo of
                    Nothing ->
                        entities
                            |> Dict.foldl consider
                                { attraction = Vector2d.zero, prey = Nothing }
                            |> decide

                    Just previous ->
                        Idle
    in
        entities
            |> Dict.map delegate


perform : Time -> Actions -> World -> World
perform delta actions world =
    let
        delegate : Id -> Action -> World -> World
        delegate id action world =
            case Dict.get id world.entities of
                -- Entity was previously removed. Move on.
                Nothing ->
                    world

                Just (Predator state) ->
                    predator action id state world

                Just (Bug state) ->
                    bug action id state world

                Just (Food state) ->
                    -- Food can take no actions ATM
                    world

                Just (Seed state) ->
                    seed action id state world

        predator action id state world =
            let
                crawl direction world =
                    -- TODO: Move head in the direction
                    state
                        |> move direction (delta * 0.03)
                        |> burn (delta * 0.00001)
                        |> update

                pullTo other this =
                    this.position
                        |> Vector2d.from other.position
                        |> Vector2d.normalize
                        |> Vector2d.scaleBy 5
                        |> (flip Point2d.translateBy) other.position
                        |> Direction2d.from state.position
                        |> Maybe.map
                            (\direction ->
                                move direction (delta * 0.03) this
                            )
                        |> Maybe.withDefault this
                        |> burn (delta * 0.00004)
                        |> update

                detach this =
                    update { this | attachedTo = Nothing }

                update this =
                    if this.nutrition <= 0 then
                        world
                            |> remove id
                            |> insert
                                (Seed
                                    { position = this.position
                                    , size = 0.01
                                    , nutrient = this.mass
                                    }
                                )
                    else
                        replace id (Predator this) world
            in
                case ( state.attachedTo, action ) of
                    ( Nothing, Idle ) ->
                        -- TODO: Deplete nutrition
                        world

                    ( Nothing, Crawl direction ) ->
                        crawl direction world

                    ( Nothing, Consume target ) ->
                        case Dict.get target world.entities of
                            Just (Bug other) ->
                                world
                                    |> remove target
                                    |> insert
                                        (Predator
                                            { position = other.position
                                            , mass = other.mass
                                            , nutrition = other.nutrition
                                            , attachedTo = Nothing
                                            }
                                        )
                                    |> replace id
                                        (Predator
                                            { state
                                                | attachedTo = Just world.seed
                                            }
                                        )

                            _ ->
                                world

                    ( Just other, _ ) ->
                        case Dict.get other world.entities of
                            Nothing ->
                                detach state

                            Just (Predator segment) ->
                                state
                                    |> pullTo segment

                            Just _ ->
                                world

                    ( _, _ ) ->
                        -- No other actions can be performed by a predator
                        world

        bug action id state world =
            let
                { mass, nutrition, position } =
                    state

                idle world =
                    {- bugs grow when idle -}
                    let
                        bug =
                            Bug
                                { state
                                    | mass =
                                        mass + delta * 0.003
                                    , nutrition =
                                        nutrition - delta * 0.01
                                }
                    in
                        replace id bug world

                crawl direction world =
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
                                |> remove id
                                |> insert
                                    (Seed
                                        { position = newState.position
                                        , size = 0.01
                                        , nutrient = newState.mass
                                        }
                                    )
                        else
                            replace id (Bug newState) world

                consume target world =
                    case Dict.get target world.entities of
                        Just (Food food) ->
                            let
                                bug =
                                    Bug { state | nutrition = nutrition + amount }

                                update_food world =
                                    if remaining > 0 then
                                        replace target
                                            (Food
                                                { food | quantity = remaining }
                                            )
                                            world
                                    else
                                        remove target world

                                amount =
                                    Basics.min
                                        food.quantity
                                        (0.0001 * delta)

                                remaining =
                                    food.quantity - amount
                            in
                                world
                                    |> replace id bug
                                    |> update_food

                        _ ->
                            world

                spawn world =
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
                            |> insert offspring
                            |> replace id parent
            in
                case action of
                    Idle ->
                        idle world

                    Crawl direction ->
                        crawl direction world

                    Consume target ->
                        consume target world

                    Spawn ->
                        spawn world

                    _ ->
                        world

        seed action id state world =
            case action of
                Idle ->
                    let
                        nutrition =
                            Basics.min (delta * 0.00001) state.nutrient

                        growth =
                            nutrition * 5

                        seed =
                            Seed
                                { state
                                    | size = state.size + growth
                                    , nutrient = state.nutrient - nutrition
                                }
                    in
                        world |> replace id seed

                Ripe ->
                    let
                        food =
                            Food { position = state.position, quantity = state.size }
                    in
                        world
                            |> remove id
                            |> insert food

                _ ->
                    world
    in
        actions
            |> Dict.foldl delegate world


entityView : Entity -> Svg msg
entityView entity =
    case entity of
        Predator state ->
            let
                size =
                    state.mass * 3

                saturation =
                    round ((state.nutrition / state.mass) * 100)
            in
                Svg.point2d
                    { radius = size
                    , attributes =
                        [ stroke
                            ("hsl(60, "
                                ++ (toString saturation)
                                ++ "%, 50%)"
                            )
                        , fill
                            ("hsl(30, "
                                ++ toString (saturation)
                                ++ "%, 80%)"
                            )
                        ]
                    }
                    state.position

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
    | Predator
        { position : Point2d
        , mass : Float
        , nutrition : Float
        , attachedTo : Maybe Id
        }


insert : Entity -> World -> World
insert entity { seed, entities } =
    { seed = seed + 1
    , entities = Dict.insert seed entity entities
    }


replace : Id -> Entity -> World -> World
replace id entity world =
    { world | entities = Dict.insert id entity world.entities }


remove : Id -> World -> World
remove id world =
    { world | entities = Dict.remove id world.entities }


type Action
    = Idle
    | Crawl Direction2d
    | Consume Id
    | Spawn
    | Ripe


type alias Actions =
    Dict Id Action


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

The bug will consider every Food, Predator and Bug entity in the environment and assign value to it based on it's distance (the further the food is, the less attraction value it has). Then resulting vectors will be summed.

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

                        repulsion =
                            if distance == 0 then
                                0
                            else
                                {- deterence is very strong when bugs are close, but weakens significantly with distance. Also bigger bugs are more scary -}
                                10 * other.mass / (distance ^ 3)
                    in
                        case direction of
                            Nothing ->
                                {- probably other is self -}
                                current

                            Just direction ->
                                Vector2d.with
                                    { length = repulsion
                                    , direction = Direction2d.flip direction
                                    }
                                    |> Vector2d.sum current

                Predator other ->
                    let
                        ( distance, direction ) =
                            position
                                |> Vector2d.from other.position
                                |> Vector2d.lengthAndDirection
                                |> Maybe.withDefault ( 0, Direction2d.x )

                        threat =
                            if distance == 0 then
                                -- Either it got us and there is not much we can do about it, or it has no head and it poses no threat.
                                0
                            else
                                {- Threat is very strong if the head is close and weakens with distance, but not as much as repulsion towards other bugs. The size of the head doesn't matter -}
                                300 / (distance ^ (3 - nutrition))
                    in
                        if other.attachedTo == Nothing then
                            Vector2d.with
                                { length = threat
                                , direction = direction
                                }
                                |> Vector2d.sum current
                        else
                            current

                Seed _ ->
                    current
        )
        Vector2d.zero
        entities



-- World mutations


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
