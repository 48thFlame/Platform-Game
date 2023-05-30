module Player exposing (..)

import Constants exposing (..)
import Engine exposing (..)



-- import Svg


newPlayer : Player
newPlayer =
    { eb =
        { pos = newPosition (canvasS.w / 2 - 28) (canvasS.h / 2 - 32)
        , dim = newDimension 9 12
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
playerSpaceBar : List EntityBase -> Player -> Player
playerSpaceBar clrs plr =
    let
        -- {-| checks whether plr can jump - touching ground or something
        -- -}
        playerCanJump : Bool
        playerCanJump =
            List.any (isCollided (actAction 1 (MoveUpDown 4) plr.eb)) clrs

        --  |> yTooBig canvasS.h
    in
    if playerCanJump then
        playerActJump plr

    else
        plr


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
playerActActionSafely :
    Float
    -> Float
    -> (Float -> EntityAction)
    -> List EntityBase
    -> Player
    -> Player
playerActActionSafely delta actionValue actionType colliders plr =
    let
        eb =
            plr.eb

        tempEb =
            actAction delta (actionType actionValue) eb

        newPlr =
            if List.any (isCollided tempEb) colliders then
                case actionType actionValue of
                    MoveUpDown _ ->
                        -- if moving up down fix dumb issue where freezes in the air
                        let
                            vel =
                                plr.vel
                        in
                        { plr | vel = { vel | dy = 0 } }

                    _ ->
                        plr

            else
                -- safe!
                { plr | eb = tempEb }
    in
    newPlr


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
                    let
                        value =
                            vel.dy + plrS.gravityStrength * delta
                    in
                    if value >= plrS.maxDy then
                        plrS.maxDy
                        -- - plrS.gravityStrength * delta

                    else
                        value

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
                | dy = newDy
                , dx =
                    newDx
            }
    in
    { plr | vel = newVel }


{-| Game update message happened - apply pending changes
-}
playerAnimationFrame : Float -> EntityBase -> Player -> Player
playerAnimationFrame delta plat plr =
    playerActActionSafely delta
        plr.vel.dy
        MoveUpDown
        [ plat ]
        plr
        |> playerActActionSafely delta
            plr.vel.dx
            MoveLeftRight
            [ plat ]
        |> playerUpdateVel delta
