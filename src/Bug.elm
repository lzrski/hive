module Bug exposing (Bug, create, update)

import OpenSolid.Direction2d exposing (Direction2d)
import OpenSolid.Point2d exposing (Point2d)
import OpenSolid.Vector2d
import Time exposing (Time)


type alias Bug =
    { position : Point2d
    , nutrition : Float
    }


create : Point2d -> Bug
create point =
    Bug point 1.0


update : Time -> Bug -> Bug
update delta bug =
    let
        towardOrigin =
            OpenSolid.Direction2d.from bug.position OpenSolid.Point2d.origin

        {- If bug is hungry, it will go out to seek food.
           If it's fed, it will go toward the origin.
        -}
        direction =
            if bug.nutrition > 0.7 then
                towardOrigin
            else
                Maybe.map OpenSolid.Direction2d.flip towardOrigin
    in
        case direction of
            Just dir ->
                let
                    velocity =
                        OpenSolid.Vector2d.with { length = 0.1, direction = dir }
                in
                    { bug
                        | nutrition = bug.nutrition - (delta / 10000)
                        , position = OpenSolid.Point2d.translateBy velocity bug.position
                    }

            Nothing ->
                bug
