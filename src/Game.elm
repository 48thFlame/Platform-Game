module Game exposing (..)

import Common exposing (..)
import Constants exposing (..)
import Engine exposing (..)
import Lava exposing (..)
import Plat exposing (..)
import Player exposing (..)
import Svg
import Svg.Attributes as SvgA


getGameOverStatus : GameState -> GameStatus
getGameOverStatus gs =
    if isCollided gs.lava gs.player.eb then
        GameOver

    else
        Playing



-- INIT


newGameState : GameState
newGameState =
    { player = newPlayer
    , platforms = []
    , score = 0
    , lava = newLava
    }



-- MODEL


type alias GameState =
    { player : Player
    , platforms : List Platform
    , lava : EntityBase
    , score : Int
    }



-- UPDATE


updateGameStateModelCall :
    Float
    -> ( Float, Float )
    -> KeysPressed
    -> ( Maybe Position, Float )
    -> GameState
    -> GameState
updateGameStateModelCall delta randTuple keys touch gs =
    List.foldl
        (updateGameState delta (Tuple.first randTuple))
        gs
        (getGameMsgs keys touch (Tuple.second randTuple) gs)


type GameMsg
    = AnimationFrame
    | Right
    | Left
    | NewPlatform


updateGameState : Float -> Float -> GameMsg -> GameState -> GameState
updateGameState delta rand msg gs =
    case msg of
        AnimationFrame ->
            let
                colliders =
                    gs.platforms ++ borderColliders

                platformUpdate =
                    platformsAnimationFrame delta gs.score gs.platforms

                newPlatforms =
                    Tuple.first platformUpdate

                scoreIncrease =
                    Tuple.second platformUpdate
            in
            { gs
                | player = playerAnimationFrame delta colliders gs.player
                , platforms = newPlatforms
                , lava = updateLava gs.score gs.lava
                , score = gs.score + scoreIncrease
            }

        Right ->
            { gs | player = playerSide True gs.player }

        Left ->
            { gs | player = playerSide False gs.player }

        NewPlatform ->
            { gs
                | platforms =
                    newPlatform
                        (getRandomInRange rand 0 (canvasS.w - platformS.w))
                        0
                        :: gs.platforms
            }


getGameMsgs : KeysPressed -> ( Maybe Position, Float ) -> Float -> GameState -> List GameMsg
getGameMsgs keys touch rand gs =
    let
        keyCheckFunc kl =
            List.any (isPressed keys) kl
    in
    [ Just AnimationFrame
    , if shouldNewPlatform rand gs.platforms then
        Just NewPlatform

      else
        Nothing
    , if keyCheckFunc rightKeys then
        Just Right

      else
        Nothing
    , if keyCheckFunc leftKeys then
        Just Left

      else
        Nothing
    , case touch of
        ( Nothing, _ ) ->
            Nothing

        ( Just pos, middleX ) ->
            if pos.x > middleX then
                Just Right

            else
                --  if pos.x < middlePos.x then
                Just Left
    ]
        -- convert maybe to just value by dropping `Nothing`
        |> List.filterMap identity



-- VIEW


viewGameState : Image -> GameState -> Svg.Svg msg
viewGameState playerSrc gs =
    Svg.g
        []
        [ viewEntity "assets/lava.png" gs.lava
        , viewEntity playerSrc gs.player.eb
        , Svg.g [] (List.map (viewEntity "assets/platform.png") gs.platforms)
        , Svg.text_ [ SvgA.x "7", SvgA.y "20" ] [ Svg.text (String.fromInt gs.score) ]
        ]
