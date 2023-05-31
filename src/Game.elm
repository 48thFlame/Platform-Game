module Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Player exposing (..)
import Svg
import Svg.Attributes as SvgA


newPlatform : Float -> Float -> Platform
newPlatform x y =
    { dim = newDimension platformS.w platformS.h
    , pos = newPosition x y
    , rot = initialRotation
    }


type alias Platform =
    EntityBase


platformsAnimationFrame : Float -> Float -> List Platform -> ( List Platform, Int )
platformsAnimationFrame delta rand platforms =
    let
        firstLen =
            List.length platforms

        movedDown =
            List.map (actAction delta (MoveUpDown platformS.speed)) platforms

        filtered =
            List.filter (\p -> p.pos.y < canvasS.h) movedDown

        secondLen =
            List.length filtered

        scoreIncrease =
            firstLen - secondLen
    in
    ( filtered, scoreIncrease )



-- INIT


newGameState : GameState
newGameState =
    { player = newPlayer
    , platforms = []
    , score = 0
    }



-- MODEL


type alias GameState =
    { player : Player
    , platforms : List Platform
    , score : Int
    }



-- VIEW


viewGameState : GameState -> Svg.Svg msg
viewGameState gs =
    Svg.g
        []
        [ Svg.text_ [ SvgA.x "7", SvgA.y "20" ] [ Svg.text (String.fromInt gs.score) ]
        , viewEntity "assets/player.png" gs.player.eb
        , Svg.g [] (List.map (viewEntity "assets/platform.png") gs.platforms)
        ]



-- UPDATE


type GameMsg
    = AnimationFrame
    | JumpButton
    | RightButton
    | LeftButton
    | NewPlatform


updateGameStateModelCall : Float -> Float -> KeysPressed -> ( Maybe Position, Position ) -> GameState -> GameState
updateGameStateModelCall delta rand keys mouse gs =
    List.foldl (updateGameState delta (lcgRandom (lcgRandom (lcgRandom rand)))) gs (getGameMsgs keys mouse (lcgRandom rand) gs)


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
                -- prefers lower numbers because keeps trying until works
                yToNew =
                    getRandomInRange rand platformS.newYA platformS.newYB
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


borderColliders : List EntityBase
borderColliders =
    [ { pos = newPosition -101 0
      , dim = newDimension 100 canvasS.h
      , rot = initialRotation
      }

    -- , { pos = newPosition -101 0
    --   , dim = newDimension canvasS.w 100
    --   , rot = initialRotation
    --   }
    , { pos = newPosition canvasS.w 0
      , dim = newDimension 100 canvasS.h
      , rot = initialRotation
      }
    , { pos = newPosition 0 canvasS.h
      , dim = newDimension canvasS.w 100
      , rot = initialRotation
      }
    ]


updateGameState : Float -> Float -> GameMsg -> GameState -> GameState
updateGameState delta rand msg gs =
    let
        colliders =
            gs.platforms ++ borderColliders

        a =
            lcgRandom rand

        b =
            lcgRandom a
    in
    case msg of
        AnimationFrame ->
            let
                platformUpdate =
                    platformsAnimationFrame delta a gs.platforms

                newPlatforms =
                    Tuple.first platformUpdate

                scoreIncrease =
                    Tuple.second platformUpdate
            in
            { gs
                | player = playerAnimationFrame delta colliders gs.player
                , platforms = newPlatforms
                , score = gs.score + scoreIncrease
            }

        JumpButton ->
            { gs | player = playerUp colliders gs.player }

        RightButton ->
            { gs | player = playerRight gs.player }

        LeftButton ->
            { gs | player = playerLeft gs.player }

        NewPlatform ->
            { gs
                | platforms =
                    newPlatform
                        (getRandomInRange b 0 (canvasS.w - platformS.w))
                        0
                        :: gs.platforms
            }
