module Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Player exposing (..)
import Svg



-- INIT


newGameState : GameState
newGameState =
    { player = newPlayer
    }



-- MODEL


type alias GameState =
    { player : Player
    }



-- VIEW


viewGameState : GameState -> Svg.Svg msg
viewGameState gs =
    viewEntity gs.player.eb



-- UPDATE


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
        -- convert maybe to just value by dropping `Nothing`
        |> List.filterMap identity


updateGameState : Float -> GameMsg -> GameState -> GameState
updateGameState delta msg gs =
    case msg of
        AnimationFrame ->
            { gs | player = playerAnimationFrame delta gs.player }

        JumpButton ->
            { gs | player = playerSpaceBar gs.player }

        RightButton ->
            { gs | player = playerRight gs.player }

        LeftButton ->
            { gs | player = playerLeft gs.player }
