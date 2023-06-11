module Common exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Html.Events.Extra.Touch as Touch
import Random


randCommand : (( Float, Float ) -> msg) -> Cmd msg
randCommand m =
    let
        gen =
            Random.float 0 1
    in
    Random.generate m (Random.pair gen gen)


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


type GameStatus
    = Menu
    | Playing
    | GameOver


playerSrc : Int -> String
playerSrc i =
    "assets/player" ++ String.fromInt i ++ ".png"


difficultyIncrease : Int -> Float
difficultyIncrease score =
    let
        fs =
            score |> toFloat

        i =
            sqrt (2.5 * fs)
                ^ 1.3
    in
    i


borderColliders : List EntityBase
borderColliders =
    [ { pos = newPosition -101 -canvasS.h
      , dim = newDimension 100 (canvasS.h * 2)
      , rot = initialRotation
      }
    , { pos = newPosition canvasS.w -canvasS.h
      , dim = newDimension 100 (canvasS.h * 2)
      , rot = initialRotation
      }
    , { pos = newPosition 0 canvasS.h
      , dim = newDimension canvasS.w 100
      , rot = initialRotation
      }
    ]
