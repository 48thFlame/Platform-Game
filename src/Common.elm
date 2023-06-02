module Common exposing (..)

import Constants exposing (..)
import Engine exposing (..)


type GameStatus
    = Menu
    | Playing
    | GameOver


difficultyIncrease : Int -> Float
difficultyIncrease score =
    let
        fs =
            score |> toFloat

        i =
            sqrt fs
                ^ 1.8
    in
    i



-- difficultyIncrease score


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
