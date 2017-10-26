module Food exposing (Food, create, update)

import OpenSolid.Direction2d exposing (Direction2d)
import OpenSolid.Point2d exposing (Point2d)
import OpenSolid.Vector2d
import Time exposing (Time)


type alias Food =
    { position : Point2d
    , quantity : Float
    }


create : ( Float, Float ) -> Food
create coordinates =
    Food
        (OpenSolid.Point2d.fromCoordinates coordinates)
        1.0


update : Time -> Food -> Food
update delta food =
    food
