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
import Keyboard.Extra as Keyboard
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Time, second)
import Window
import World exposing (World)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { elapsed : Time
    , paused : Bool
    , count : Dict String Float
    , zoom : Float
    , translation : Vector2d
    , window : Window.Size
    , world : World
    , keys : List Keyboard.Key
    }


type Msg
    = NoOp
    | Frame Time
    | Pause
    | Resume
    | Count
    | ZoomOut
    | ZoomIn
    | Resize Window.Size
    | Pan Vector2d
    | KeyboardMsg Keyboard.Msg


init : ( Model, Cmd Msg )
init =
    { elapsed = 0
    , paused = False
    , count = Dict.empty
    , zoom = 1
    , translation = Vector2d.fromComponents ( 0, 0 )
    , window = { width = 0, height = 0 }
    , world = World.init
    , keys = []
    }
        ! [ Task.perform Resize Window.size ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Count ->
            { model
                | count = World.stats model.world
            }
                ! []

        ZoomOut ->
            { model | zoom = model.zoom * 0.99 } ! []

        ZoomIn ->
            { model | zoom = model.zoom / 0.99 } ! []

        Pan vector ->
            { model
                | translation =
                    vector
                        |> Vector2d.scaleBy (2 / model.zoom)
                        |> Vector2d.sum model.translation
            }
                ! []

        Resize size ->
            { model | window = size } ! []

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
                    , world = World.update delta model.world
                }
                    ! []

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )

        KeyboardMsg m ->
            { model
                | keys = Keyboard.update m model.keys
            }
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        common =
            [ AnimationFrame.diffs (handleKeyboard model.keys)
            , Sub.map KeyboardMsg Keyboard.subscriptions
            , Window.resizes Resize
            ]
    in
        if model.paused then
            Sub.batch common
        else
            Sub.batch <|
                common
                    ++ [ AnimationFrame.diffs Frame
                       , Time.every
                            (second * 3)
                            (always Count)
                       ]


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



-- Public constructors
-- Update


sceneView : Model -> Html Msg
sceneView { zoom, translation, window, world } =
    world
        |> World.view
        |> Svg.translateBy translation
        |> Svg.scaleAbout (Point2d.fromCoordinates ( 0, 0 )) zoom
        |> Svg.render2d
            (BoundingBox2d.with
                { minX = (toFloat window.width) / -2
                , maxX = (toFloat window.width) / 2
                , minY = (toFloat window.height) / 2
                , maxY = (toFloat window.height) / -2
                }
            )


handleKeyboard : List Keyboard.Key -> Time -> Msg
handleKeyboard keys delta =
    let
        { x, y } =
            Keyboard.wasd keys

        pan : Vector2d
        pan =
            ( x, y )
                |> Tuple.mapFirst toFloat
                |> Tuple.mapSecond toFloat
                |> Vector2d.fromComponents
                |> Vector2d.flip
                |> Vector2d.scaleBy (delta / 10)
    in
        if List.member Keyboard.HyphenMinus keys then
            ZoomOut
        else if List.member Keyboard.Equals keys then
            ZoomIn
        else
            Pan pan
