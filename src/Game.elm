module Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Player exposing (..)
import Svg



-- INIT


newGameState : GameState
newGameState =
    { player = newPlayer
    , platform =
        { dim = newDimension 128 64
        , img = "assets/platform.png"
        , pos = newPosition 300 600
        , rot = initialRotation
        }
    }



-- MODEL


type alias GameState =
    { player : Player
    , platform : EntityBase
    }



-- VIEW


viewGameState : GameState -> Svg.Svg msg
viewGameState gs =
    Svg.g [] [ viewEntity gs.player.eb, viewEntity gs.platform ]



-- UPDATE


type GameMsg
    = AnimationFrame
    | JumpButton
    | RightButton
    | LeftButton


updateGameStateModelCall : Float -> KeysPressed -> ( Maybe Position, Position ) -> GameState -> GameState
updateGameStateModelCall delta keys mouse gs =
    List.foldl (updateGameState delta) gs (getGameMsgs keys mouse)


getGameMsgs : KeysPressed -> ( Maybe Position, Position ) -> List GameMsg
getGameMsgs keys mouse =
    [ Just AnimationFrame
    , if isPressed "ArrowUp" keys then
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
        ++ (case mouse of
                ( Nothing, _ ) ->
                    [ Nothing ]

                ( Just pos, middlePos ) ->
                    [ if pos.x > middlePos.x then
                        Just RightButton

                      else
                        Just LeftButton
                    , if pos.y < middlePos.y then
                        Just JumpButton

                      else
                        Nothing
                    ]
           )
        -- convert maybe to just value by dropping `Nothing`
        |> List.filterMap identity


updateGameState : Float -> GameMsg -> GameState -> GameState
updateGameState delta msg gs =
    case msg of
        AnimationFrame ->
            { gs | player = playerAnimationFrame delta gs.platform gs.player }

        JumpButton ->
            { gs | player = playerSpaceBar [ gs.platform ] gs.player }

        RightButton ->
            { gs | player = playerRight gs.player }

        LeftButton ->
            { gs | player = playerLeft gs.player }
