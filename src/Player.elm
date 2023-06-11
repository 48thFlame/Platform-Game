module Player exposing (..)

import Common exposing (..)
import Constants exposing (..)
import Engine exposing (..)


newPlayer : Player
newPlayer =
    { eb =
        { pos = newPosition (canvasS.w / 2 - 28) (canvasS.h / 2 - 32)
        , dim = newDimension 9 12
        , rot = initialRotation
        }
    , vel = initialVelocity
    }


type alias Player =
    { eb : EntityBase
    , vel : Velocity
    }


{-| Move player to a side,
`right` - a bool saying whether should go right, leave False to go left
-}
playerSide : Bool -> Player -> Player
playerSide right plr =
    let
        vel =
            plr.vel

        sign =
            if right then
                1

            else
                -1

        newVel =
            { vel | dx = plrS.leftRightStrength * sign }
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
                        let
                            vel =
                                plr.vel
                        in
                        if vel.dy > 0 then
                            -- if falling and collided - JUMP!
                            { plr | vel = { vel | dy = -plrS.jumpStrength } }

                        else
                            --if vel.dy < 0 then
                            { plr | eb = tempEb }

                    MoveLeftRight _ ->
                        let
                            pos =
                                tempEb.pos

                            width =
                                tempEb.dim.width
                        in
                        if pos.x > 0 && (pos.x + width) < canvasS.w then
                            -- if inbounds then not going outside then can allow to go threw platforms
                            { plr | eb = tempEb }

                        else
                            plr

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

        newDy =
            let
                value =
                    vel.dy + plrS.gravityStrength * delta
            in
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
                    value - plrS.frictionStrength
            in
            if nDx > 0 then
                nDx * sign

            else
                0

        newVel =
            { vel
                | dy = newDy
                , dx =
                    newDx
            }
    in
    { plr | vel = newVel }


{-| Game update message happened - apply pending changes
-}
playerAnimationFrame : Float -> List EntityBase -> Player -> Player
playerAnimationFrame delta colliders plr =
    playerActActionSafely delta
        plr.vel.dy
        MoveUpDown
        colliders
        plr
        |> playerActActionSafely delta
            plr.vel.dx
            MoveLeftRight
            colliders
        |> playerUpdateVel delta
