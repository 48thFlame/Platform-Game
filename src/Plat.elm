module Plat exposing (..)

import Common exposing (..)
import Constants exposing (..)
import Engine exposing (..)


newPlatform : Float -> Float -> Platform
newPlatform x y =
    { dim = newDimension platformS.w platformS.h
    , pos = newPosition x y
    , rot = initialRotation
    }


type alias Platform =
    EntityBase


platformsAnimationFrame : Float -> Int -> List Platform -> ( List Platform, Int )
platformsAnimationFrame delta score platforms =
    let
        firstLen =
            List.length platforms

        movedDown =
            List.map (actAction delta (MoveUpDown (platformS.speed + difficultyIncrease score))) platforms

        filtered =
            List.filter (\p -> p.pos.y < canvasS.h) movedDown

        secondLen =
            List.length filtered

        scoreIncrease =
            firstLen - secondLen
    in
    ( filtered, scoreIncrease )
