module Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Svg


newGameState : GameState
newGameState =
    { player = newPlayer
    }


type alias GameState =
    { player : Player
    }


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


playerRight : Player -> Player
playerRight plr =
    let
        vel =
            plr.vel

        newVel =
            { vel | dx = plrS.leftRightStrength }
    in
    { plr | vel = newVel }


playerLeft : Player -> Player
playerLeft plr =
    let
        vel =
            plr.vel

        newVel =
            { vel | dx = -plrS.leftRightStrength }
    in
    { plr | vel = newVel }


{-| checks whether plr can jump - touching ground or something
-}
playerCanJump : Player -> Bool
playerCanJump plr =
    actAction 1 (MoveUpDown 1) plr.eb |> yTooBig canvasS.h


{-| Respond to `SpaceBar` `GameMsg`
-}
playerSpaceBar : Player -> Player
playerSpaceBar plr =
    if playerCanJump plr then
        playerActJump plr

    else
        plr


{-| actAction and make sure good - not out of bounds
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


playerAnimationFrame : Float -> Player -> Player
playerAnimationFrame delta plr =
    playerActActionSafely delta (ApplyVelocity plr.vel) plr |> playerUpdateVel delta


type GameMsg
    = AnimationFrame
    | JumpButton
    | RightButton
    | LeftButton


updateGameStateModelCall : Float -> KeysPressed -> GameState -> GameState
updateGameStateModelCall delta keys gs =
    List.foldl (updateGameState delta) gs (getGameMsgs keys)


getGameMsgs : KeysPressed -> List GameMsg
getGameMsgs keys =
    [ Just AnimationFrame
    , if isPressed " " keys then
        Just JumpButton

      else
        Nothing
    , if isPressed "ArrowRight" keys then
        Just RightButton

      else
        Nothing
    , if isPressed "ArrowLeft" keys then
        Just LeftButton

      else
        Nothing
    ]
        |> List.filterMap identity



-- convert maybe to just by dropping `Nothing`


updateGameState : Float -> GameMsg -> GameState -> GameState
updateGameState delta msg gs =
    -- let
    --     directionMove value =
    -- in
    case msg of
        AnimationFrame ->
            { gs | player = playerAnimationFrame delta gs.player }

        JumpButton ->
            { gs | player = playerSpaceBar gs.player }

        RightButton ->
            { gs | player = playerRight gs.player }

        LeftButton ->
            { gs | player = playerLeft gs.player }


viewGameState : GameState -> Svg.Svg msg
viewGameState gs =
    viewEntity gs.player.eb
