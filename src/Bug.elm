module Bug exposing (Bug, create, update)

import OpenSolid.Point2d exposing (Point2d)
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
    { bug | nutrition = bug.nutrition - (delta / 10000) }
