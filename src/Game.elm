module Game exposing (..)

import Common exposing (..)
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
    , background = 0
    , lava = newLava
    }



-- MODEL


type alias GameState =
    { player : Player
    , platforms : List Platform
    , lava : EntityBase
    , background : Int
    , score : Int
    }



-- UPDATE


updateGameStateModelCall :
    Float
    -> ( Float, Float )
    -> KeysPressed
    -> ( Maybe Position, Float )
    -> GameState
    -> ( GameState, List (Cmd msg) )
updateGameStateModelCall delta randTuple keys touch gs =
    let
        msgs =
            getGameMsgs keys touch (Tuple.second randTuple) delta gs
    in
    ( List.foldl
        (updateGameState delta (Tuple.first randTuple))
        gs
        msgs
    , getCmds gs msgs
    )


type GameMsg
    = AnimationFrame
    | Right
    | Left
    | Jump
    | NewPlatform
    | ScoreIncrease
    | ChangeBackground


updateGameState : Float -> Float -> GameMsg -> GameState -> GameState
updateGameState delta rand msg gs =
    case msg of
        AnimationFrame ->
            let
                colliders =
                    gs.platforms ++ borderColliders

                platformUpdate =
                    platformsAnimationFrame delta gs.platforms
            in
            { gs
                | player = playerAnimationFrame delta colliders gs.player
                , platforms = platformUpdate
                , lava = updateLava gs.score gs.lava
            }

        ScoreIncrease ->
            { gs | score = gs.score + 1 }

        ChangeBackground ->
            if gs.background < backS.max then
                { gs | background = gs.background + 1 }

            else
                gs

        Right ->
            { gs | player = playerSide True gs.player }

        Left ->
            { gs | player = playerSide False gs.player }

        Jump ->
            { gs | player = playerJump gs.player }

        NewPlatform ->
            { gs
                | platforms =
                    newPlatform
                        (getRandomInRange rand 0 (canvasS.w - platformS.w))
                        0
                        :: gs.platforms
            }


getCmds : GameState -> List GameMsg -> List (Cmd msg)
getCmds gs gm =
    let
        newScore =
            gs.score + 1
    in
    [ if List.member ScoreIncrease gm then
        if modBy 10 newScore == 0 then
            soundBigScore

        else
            soundScoreUp

      else
        Cmd.none
    , if List.member ChangeBackground gm then
        soundFire

      else
        Cmd.none
    ]


getGameMsgs :
    KeysPressed
    -> ( Maybe Position, Float )
    -> Float
    -> Float
    -> GameState
    -> List GameMsg
getGameMsgs keys touch rand delta gs =
    let
        keyCheckFunc kl =
            List.any (isPressed keys) kl

        colliders =
            gs.platforms ++ borderColliders

        plrJumpedCheck =
            let
                plr =
                    gs.player

                eb =
                    plr.eb

                vel =
                    plr.vel

                plrJumped =
                    actAction delta (MoveUpDown vel.dy) eb
            in
            List.any (isCollided plrJumped) colliders

        shouldScoreUp =
            bottomPlatShouldDie gs.platforms
    in
    [ if shouldScoreUp then
        Just ScoreIncrease

      else
        Nothing
    , if shouldScoreUp && modBy backS.changeNum (gs.score + 1) == 0 then
        Just ChangeBackground

      else
        Nothing
    , if plrJumpedCheck then
        Just Jump

      else
        Nothing
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
    , Just AnimationFrame
    ]
        -- convert maybe to just value by dropping `Nothing`
        |> List.filterMap identity



-- VIEW


displayBackground : Int -> Svg.Svg msg
displayBackground i =
    let
        path =
            "assets/background" ++ String.fromInt i ++ ".png"

        ent =
            { pos = newPosition 0 0
            , dim = newDimension canvasS.w canvasS.h
            , rot = initialRotation
            }
    in
    Svg.g [ SvgA.opacity "0.4" ] [ viewEntity path ent ]


viewGameState : Image -> GameState -> Svg.Svg msg
viewGameState playerSrc gs =
    Svg.g
        []
        [ displayBackground gs.background
        , viewEntity "assets/lava.png" gs.lava
        , viewEntity playerSrc gs.player.eb
        , Svg.g [] (List.map (viewEntity "assets/platform.png") gs.platforms)
        , Svg.text_ [ SvgA.x "7", SvgA.y "20" ] [ Svg.text (String.fromInt gs.score) ]
        ]
