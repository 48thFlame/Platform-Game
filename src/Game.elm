module Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Player exposing (..)
import Svg


newPlatform : Float -> Float -> Platform
newPlatform x y =
    { eb =
        { dim = newDimension platformS.w platformS.h
        , pos = newPosition x y
        , rot = initialRotation
        }
    }


type alias Platform =
    { eb : EntityBase
    }



-- INIT


newGameState : GameState
newGameState =
    { player = newPlayer
    , platform =
        [ newPlatform 0 230
        , newPlatform (canvasS.w - platformS.w) 185
        , newPlatform 0 140
        , newPlatform (canvasS.w - platformS.w) 95
        ]
    }



-- MODEL


type alias GameState =
    { player : Player
    , platform : List Platform
    }



-- VIEW


viewGameState : GameState -> Svg.Svg msg
viewGameState gs =
    let
        viewPlatform p =
            viewEntity "assets/platform.png" p.eb
    in
    Svg.g
        []
        ([ viewEntity "assets/player.png" gs.player.eb ]
            ++ List.map viewPlatform gs.platform
        )



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


borderColliders : List EntityBase
borderColliders =
    [ { pos = newPosition -1 0, dim = newDimension 1 canvasS.h, rot = initialRotation }
    , { pos = newPosition -1 0, dim = newDimension canvasS.w 1, rot = initialRotation }
    , { pos = newPosition canvasS.w 0, dim = newDimension 1 canvasS.h, rot = initialRotation }
    , { pos = newPosition 0 canvasS.h, dim = newDimension canvasS.w 1, rot = initialRotation }
    ]


updateGameState : Float -> GameMsg -> GameState -> GameState
updateGameState delta msg gs =
    let
        colliders =
            List.map .eb gs.platform ++ borderColliders
    in
    case msg of
        AnimationFrame ->
            { gs | player = playerAnimationFrame delta colliders gs.player }

        JumpButton ->
            { gs | player = playerUp colliders gs.player }

        RightButton ->
            { gs | player = playerRight gs.player }

        LeftButton ->
            { gs | player = playerLeft gs.player }
