port module Common exposing (..)

import Engine exposing (..)
import Html.Events.Extra.Touch as Touch
import Random


canvasS =
    { w = 120, sw = "120", h = 280, sh = "280" }


plrS =
    { gravityStrength = 1097
    , jumpStrength = 320
    , frictionStrength = 2000
    , leftRightStrength = 144
    , w = 9
    , h = 12
    , maxSel = 5
    }


lavaS =
    { startingY = canvasS.h + 1 }


platformS =
    { w = 24
    , h = 6
    , newYA = 50
    , newYB = 55
    , speed = 50
    , divToNew = 22
    }


backS =
    { changeNum = 50, max = 8 }


rightKeys =
    [ "ArrowRight", "d", "D", "ג" ]


leftKeys =
    [ "ArrowLeft", "a", "A", "ש" ]


randCommand : (( Float, Float ) -> msg) -> Cmd msg
randCommand m =
    let
        gen =
            Random.float 0 1
    in
    Random.generate m (Random.pair gen gen)


port playSound : String -> Cmd msg


soundScoreUp : Cmd msg
soundScoreUp =
    playSound "ScoreUp"


soundBigScore : Cmd msg
soundBigScore =
    playSound "BigScore"


soundFire : Cmd msg
soundFire =
    playSound "Fire"


difficultyIncrease : Int -> Float
difficultyIncrease score =
    let
        x =
            score |> toFloat

        y =
            sqrt (2.5 * x)
                ^ 1.3
    in
    y


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



-- difficultyIncrease : Int -> Float
-- difficultyIncrease score =
--     let
--         fs =
--             score |> toFloat
--         i =
--             sqrt (2.5 * fs)
--                 ^ 1.3
--     in
--     i


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
