module Plat exposing (..)

import Common exposing (..)
import Engine exposing (..)


newPlatform : Float -> Float -> Platform
newPlatform x y =
    { dim = newDimension platformS.w platformS.h
    , pos = newPosition x y
    , rot = initialRotation
    }


type alias Platform =
    EntityBase


platformsAnimationFrame : Float -> List Platform -> List Platform
platformsAnimationFrame delta platforms =
    let
        deadsRemoved =
            if bottomPlatShouldDie platforms then
                List.reverse platforms |> List.drop 1 |> List.reverse

            else
                platforms

        movedDown =
            List.map
                (actAction delta (MoveUpDown platformS.speed))
                deadsRemoved
    in
    movedDown


shouldNewPlatform : Float -> List Platform -> Bool
shouldNewPlatform rand l =
    case List.head l of
        Nothing ->
            True

        Just p ->
            let
                py =
                    p.pos.y
            in
            rand > platformS.divToNew / py


bottomPlatShouldDie : List Platform -> Bool
bottomPlatShouldDie l =
    let
        bottomPlatform =
            List.reverse l |> List.head
    in
    case bottomPlatform of
        Nothing ->
            False

        Just p ->
            if p.pos.y > canvasS.h then
                True

            else
                False
