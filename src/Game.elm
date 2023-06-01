module Game exposing (..)

import Common exposing (..)
import Constants exposing (..)
import Engine exposing (..)
import Lava exposing (newLava)
import Plat exposing (..)
import Player exposing (..)
import Svg
import Svg.Attributes as SvgA



-- module Main exposing (main)
-- import Browser
-- import Html exposing (..)
-- main : Program flags Model Msg
-- main =
--     Browser.element
--         { init = init
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         }
-- type alias Model =
--     { property : Int
--     , property2 : String
--     }
-- init : flags -> ( Model, Cmd Msg )
-- init flags =
--     ( Model 0 "modelInitialValue2", Cmd.none )
-- type Msg
--     = Msg1
--     | Msg2
-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         Msg1 ->
--             ( model, Cmd.none )
--         Msg2 ->
--             ( model, Cmd.none )
-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.none
-- view : Model -> Html Msg
-- view model =
--     div []
--         [ text "New Element" ]
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


updateGameStateModelCall : Float -> Float -> KeysPressed -> ( Maybe Position, Position ) -> GameState -> GameState
updateGameStateModelCall delta rand keys mouse gs =
    List.foldl
        (updateGameState delta (lcgRandom (lcgRandom (lcgRandom rand))))
        gs
        (getGameMsgs keys mouse (lcgRandom rand) gs)


type GameMsg
    = AnimationFrame
    | JumpButton
    | RightButton
    | LeftButton
    | NewPlatform


updateGameState : Float -> Float -> GameMsg -> GameState -> GameState
updateGameState delta rand msg gs =
    let
        colliders =
            gs.platforms ++ borderColliders

        a =
            lcgRandom rand
    in
    case msg of
        AnimationFrame ->
            let
                platformUpdate =
                    platformsAnimationFrame delta gs.score gs.platforms

                newPlatforms =
                    Tuple.first platformUpdate

                scoreIncrease =
                    Tuple.second platformUpdate
            in
            { gs
                | player = playerAnimationFrame delta gs.score colliders gs.player
                , platforms = newPlatforms
                , score = gs.score + scoreIncrease
            }

        JumpButton ->
            { gs | player = playerUp colliders gs.score gs.player }

        RightButton ->
            { gs | player = playerRight gs.player }

        LeftButton ->
            { gs | player = playerLeft gs.player }

        NewPlatform ->
            { gs
                | platforms =
                    newPlatform
                        (getRandomInRange a 0 (canvasS.w - platformS.w))
                        0
                        :: gs.platforms
            }


getGameMsgs : KeysPressed -> ( Maybe Position, Position ) -> Float -> GameState -> List GameMsg
getGameMsgs keys mouse rand gs =
    let
        keyCheckFunc kl =
            List.any (isPressed keys) kl
    in
    [ Just AnimationFrame
    , if keyCheckFunc upKeys then
        Just JumpButton

      else
        Nothing
    , if keyCheckFunc rightKeys then
        Just RightButton

      else
        Nothing
    , if keyCheckFunc leftKeys then
        Just LeftButton

      else
        Nothing
    , case List.head gs.platforms of
        Nothing ->
            Just NewPlatform

        Just p ->
            let
                yToNew =
                    getRandomInRange
                        rand
                        (platformS.newYA + sqrt (difficultyIncrease gs.score))
                        (platformS.newYB + sqrt (difficultyIncrease gs.score))
            in
            if p.pos.y > yToNew then
                Just NewPlatform

            else
                Nothing
    ]
        ++ (case mouse of
                ( Nothing, _ ) ->
                    [ Nothing ]

                ( Just pos, middlePos ) ->
                    [ if pos.x > middlePos.x + 25 then
                        Just RightButton

                      else if pos.x < middlePos.x - 25 then
                        Just LeftButton

                      else
                        Nothing
                    , if pos.y < middlePos.y then
                        Just JumpButton

                      else
                        Nothing
                    ]
           )
        -- convert maybe to just value by dropping `Nothing`
        |> List.filterMap identity



-- VIEW


viewGameState : GameState -> Svg.Svg msg
viewGameState gs =
    Svg.g
        []
        [ viewEntity "assets/lava.png" gs.lava
        , viewEntity "assets/player.png" gs.player.eb
        , Svg.g [] (List.map (viewEntity "assets/platform.png") gs.platforms)
        , Svg.text_ [ SvgA.x "7", SvgA.y "20" ] [ Svg.text (String.fromInt gs.score) ]
        ]
