module Player exposing (..)

import Constants exposing (..)
import Engine exposing (..)



-- import Svg


newPlayer : Player
newPlayer =
    { eb =
        { pos = newPosition (canvasS.w / 2 - 28) (canvasS.h / 2 - 32)
        , dim = newDimension 48 64
        , rot = initialRotation
        , img = "assets/player.png"
        }
    , vel = initialVelocity
    }


type alias Player =
    { eb : EntityBase
    , vel : Velocity
    }


{-| Respond to `SpaceBar` `GameMsg`
-}
playerSpaceBar : Player -> Player
playerSpaceBar plr =
    if playerCanJump plr then
        playerActJump plr

    else
        plr


{-| checks whether plr can jump - touching ground or something
-}
playerCanJump : Player -> Bool
playerCanJump plr =
    actAction 1 (MoveUpDown 1) plr.eb |> yTooBig canvasS.h


{-| Make player jump without checking if able
-}
playerActJump : Player -> Player
playerActJump plr =
    let
        vel =
            plr.vel

        newVel =
            { vel | dy = -plrS.jumpStrength }
    in
    { plr | vel = newVel }


{-| Pressed right going key
-}
playerRight : Player -> Player
playerRight plr =
    let
        vel =
            plr.vel

        newVel =
            { vel | dx = plrS.leftRightStrength }
    in
    { plr | vel = newVel }


{-| Pressed left going key
-}
playerLeft : Player -> Player
playerLeft plr =
    let
        vel =
            plr.vel

        newVel =
            { vel | dx = -plrS.leftRightStrength }
    in
    { plr | vel = newVel }


{-| `actAction` and make sure good - not out of bounds
-}
playerActActionSafely : Float -> EntityAction -> Player -> Player
playerActActionSafely delta action plr =
    let
        eb =
            plr.eb

        tempEb =
            actAction delta action eb

        newEbYSafe =
            let
                tPos =
                    tempEb.pos
            in
            if yTooBig canvasS.h tempEb then
                { tempEb | pos = { tPos | y = canvasS.h - tempEb.dim.height } }

            else if yTooSmall tempEb then
                { tempEb | pos = { tPos | y = 0 } }

            else
                tempEb

        newEbXYSafe =
            let
                tPos =
                    newEbYSafe.pos
            in
            if xTooBig canvasS.w newEbYSafe then
                { newEbYSafe | pos = { tPos | x = canvasS.w - newEbYSafe.dim.width } }

            else if xTooSmall newEbYSafe then
                { newEbYSafe | pos = { tPos | x = 0 } }

            else
                newEbYSafe
    in
    { plr | eb = newEbXYSafe }


{-| Update Player vel - gravity and friction
-}
playerUpdateVel : Float -> Player -> Player
playerUpdateVel delta plr =
    let
        vel =
            plr.vel

        newVel =
            let
                newDy =
                    vel.dy + plrS.gravityStrength * delta

                newDx =
                    let
                        sign =
                            if vel.dx > 0 then
                                1

                            else
                                -1

                        value =
                            abs vel.dx

                        nDx =
                            value - plrS.frictionStrength * delta
                    in
                    if nDx > 0 then
                        nDx * sign

                    else
                        0
            in
            { vel
                | dy =
                    if newDy >= plrS.maxDy then
                        plrS.maxDy

                    else
                        newDy
                , dx =
                    newDx
            }
    in
    { plr | vel = newVel }


{-| Game update message happened - apply pending changes
-}
playerAnimationFrame : Float -> Player -> Player
playerAnimationFrame delta plr =
    playerActActionSafely delta (ApplyVelocity plr.vel) plr |> playerUpdateVel delta
