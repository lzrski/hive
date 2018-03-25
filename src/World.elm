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
        [ { position = Point2d.fromCoordinates coordinates
          , nutrition = 1.0
          , mass = 1.0
          }
        ]


reason : Entities -> Actions
reason entities =
    let
        delegate id entity =
            case entity of
                Bug state ->
                    bug state

                Seed state ->
                    seed state

                _ ->
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
    in
        entities
            |> Dict.map delegate


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

                        Just (Predator chain) ->
                            case action of
                                Idle ->
                                    -- TODO: Deplete nutrition of each chani
                                    world

                                Crawl direction ->
                                    -- TODO: Move head in the direction and all segments toward the preceeding one
                                    world

                                Consume bug ->
                                    -- TODO: Cons bug to the chain
                                    world

                                _ ->
                                    -- No other actions can be performed by a predator
                                    world

                        Just (Bug state) ->
                            case action of
                                Idle ->
                                    {- bugs grow when idle -}
                                    let
                                        newState =
                                            { state
                                                | mass = state.mass + delta * 0.003
                                                , nutrition = state.nutrition - delta * 0.01
                                            }
                                    in
                                        world
                                            |> replace id (Bug newState)

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
                                                |> remove id
                                                |> insert
                                                    (Seed
                                                        { position = newState.position
                                                        , size = 0.01
                                                        , nutrient = newState.mass
                                                        }
                                                    )
                                        else
                                            world
                                                |> replace id (Bug newState)

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
                                            |> insert offspring
                                            |> replace id parent

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
            )
            world


entityView : Entity -> Svg msg
entityView entity =
    case entity of
        Predator chain ->
            -- TODO: An Svg.triangle2d for each segment
            let
                segment state =
                    let
                        size =
                            state.mass * 2

                        saturation =
                            round ((state.nutrition / state.mass) * 100)
                    in
                        Svg.point2d
                            { radius = size
                            , attributes =
                                [ stroke
                                    ("hsl(60, "
                                        ++ (toString saturation)
                                        ++ "%, 30%)"
                                    )
                                , fill
                                    ("hsl(30, "
                                        ++ toString (saturation)
                                        ++ "%, 80%)"
                                    )
                                ]
                            }
                            state.position
            in
                chain
                    |> List.map segment
                    |> Svg.g []

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
    | Predator (List Segment)


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


type alias Segment =
    { position : Point2d
    , mass : Float
    , nutrition : Float
    }


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

                Predator chain ->
                    let
                        direction =
                            chain
                                |> List.head
                                |> Maybe.andThen
                                    (\head ->
                                        Direction2d.from
                                            position
                                            head.position
                                    )

                        distance =
                            chain
                                |> List.head
                                |> Maybe.map
                                    (\head ->
                                        Point2d.distanceFrom
                                            head.position
                                            position
                                    )
                                |> Maybe.withDefault 0

                        threat =
                            if distance == 0 then
                                -- Either it got us and there is not much we can do about it, or it has no head and it poses no threat.
                                0
                            else
                                {- Threat is very strong if the head is close and weakens with distance, but not as much as repulsion towards other bugs. The size of the head doesn't matter -}
                                2 * nutrition / distance
                    in
                        case direction of
                            Nothing ->
                                current

                            Just direction ->
                                Vector2d.with
                                    { length = threat
                                    , direction = Direction2d.flip direction
                                    }
                                    |> Vector2d.sum current

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
