module Bug exposing (Bug, create)

import OpenSolid.Point2d exposing (Point2d)


type alias Bug =
    { position : Point2d
    , nutrition : Float
    }


create : Point2d -> Bug
create point =
    Bug point 1.0
