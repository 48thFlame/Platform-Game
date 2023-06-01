module Lava exposing (..)

import Common exposing (..)
import Constants exposing (..)
import Engine exposing (..)


newLava : EntityBase
newLava =
    { pos = newPosition 0 lavaS.startingY
    , dim = newDimension canvasS.w canvasS.h
    , rot = initialRotation
    }


updateLava : Int -> EntityBase -> EntityBase
updateLava score lava =
    let
        pos =
            lava.pos
    in
    { lava | pos = { pos | y = lavaS.startingY - difficultyIncrease score } }
