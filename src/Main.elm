module Main exposing (main)

import Browser
import Browser.Events as Events
import Common exposing (..)
import Engine exposing (..)
import Game exposing (..)
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlEvents
import Html.Events.Extra.Touch as Touch
import Svg
import Svg.Attributes as SvgA



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Flags =
    { x : Float
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel d =
    ( { gs = newGameState
      , player = 1
      , gameStatus = Menu
      , keys = initialKeysPressed
      , rand = ( 0, 0 )
      , middleX = d.x + 80
      , touchDown = False
      , touchPos = newPosition 0 0
      }
    , randCommand NewRandom
    )



-- MODEL


type alias Model =
    { gs : GameState
    , player : Int
    , gameStatus : GameStatus
    , keys : KeysPressed
    , rand : ( Float, Float )
    , middleX : Float
    , touchDown : Bool
    , touchPos : Position
    }



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | PlayerSelectLeft
    | PlayerSelectRight
    | KeyDown String
    | KeyUp String
    | ClickDown ( Float, Float )
    | ClickMove ( Float, Float )
    | ClickUp
    | NewRandom ( Float, Float )
    | Blur Events.Visibility
    | PlayButton
    | ToMenu


changePlayerSelect : Int -> Int
changePlayerSelect try =
    if try > plrS.maxSel then
        1

    else if try < 1 then
        plrS.maxSel

    else
        try


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayButton ->
            ( { model
                | gameStatus = Playing
                , gs = newGameState
                , keys = initialKeysPressed
                , touchDown = False
              }
            , Cmd.none
            )

        ToMenu ->
            ( { model | gameStatus = Menu }, Cmd.none )

        OnAnimationFrame deltaTime ->
            -- main game loop
            let
                delta =
                    deltaTime / 1000

                touch =
                    if model.touchDown then
                        ( Just model.touchPos, model.middleX )

                    else
                        ( Nothing, model.middleX )

                gsUpdCall =
                    updateGameStateModelCall
                        delta
                        model.rand
                        model.keys
                        touch
                        model.gs
            in
            ( { model
                | gs = Tuple.first gsUpdCall
                , gameStatus = getGameOverStatus model.gs
              }
            , Cmd.batch (randCommand NewRandom :: Tuple.second gsUpdCall)
            )

        PlayerSelectLeft ->
            ( { model | player = changePlayerSelect (model.player + 1) }, Cmd.none )

        PlayerSelectRight ->
            ( { model | player = changePlayerSelect (model.player - 1) }, Cmd.none )

        NewRandom r ->
            ( { model | rand = r }, Cmd.none )

        KeyDown key ->
            -- add key to model.keys
            ( applyFuncToModelKeys (addKey key) model, Cmd.none )

        KeyUp key ->
            -- remove key from model.keys
            ( applyFuncToModelKeys (removeKey key) model, Cmd.none )

        ClickDown ( x, y ) ->
            ( { model | touchDown = True, touchPos = newPosition x y }, Cmd.none )

        ClickMove ( x, y ) ->
            ( { model | touchPos = newPosition x y }, Cmd.none )

        ClickUp ->
            ( { model | touchDown = False }, Cmd.none )

        Blur _ ->
            -- clear model.keys
            ( { model | touchDown = False } |> applyFuncToModelKeys clearKeys, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (case model.gameStatus of
            Playing ->
                [ Events.onAnimationFrameDelta OnAnimationFrame
                , Events.onKeyDown (keyDecoder KeyDown)
                , Events.onKeyUp (keyDecoder KeyUp)
                , Events.onVisibilityChange Blur
                ]

            _ ->
                [ Sub.none ]
        )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ HtmlA.class "main" ]
        (case model.gameStatus of
            Playing ->
                let
                    canvas =
                        Svg.g
                            [ SvgA.class "canvas"
                            ]
                            [ viewGameState (playerSrc model.player) model.gs
                            ]
                in
                [ Svg.svg
                    [ SvgA.class "canvasContainer"
                    , SvgA.viewBox ("0 0 " ++ canvasS.sw ++ " " ++ canvasS.sh)
                    , Touch.onStart (ClickDown << touchCoordinates)
                    , Touch.onMove (ClickMove << touchCoordinates)
                    , Touch.onEnd (\_ -> ClickUp)
                    ]
                    [ canvas ]
                ]

            GameOver ->
                [ Html.h1 [ HtmlA.class "title" ] [ Html.text "נגמר המשחק!" ]
                , Html.p [ HtmlA.class "pDescription" ]
                    [ Html.text "צברת "
                    , Html.text (model.gs.score |> String.fromInt)
                    , Html.text " נקודות"
                    ]
                , Html.button [ HtmlEvents.onClick ToMenu, HtmlA.class "controlButton" ] [ Html.text "לתפריט" ]
                ]

            Menu ->
                [ Html.div [ HtmlA.class "menu" ]
                    [ Html.div [ HtmlA.class "menuTopSection" ]
                        [ Html.h1
                            [ HtmlA.class "title" ]
                            [ Html.text "ברוכים הבאים למשחק!" ]
                        ]
                    , Html.div
                        [ HtmlA.class "menuMiddleSection" ]
                        [ Html.div []
                            [ Html.h3
                                [ HtmlA.class "subTitle" ]
                                [ Html.text "הסבר:" ]
                            , Html.p
                                [ HtmlA.class "pDescription"
                                ]
                                [ Html.text "קפוץ מפלטפורמה לפלטפורמה והימנע ממגע בלבה המגיעה מהרצפה."
                                ]
                            ]
                        , Html.div []
                            [ Html.button
                                [ HtmlEvents.onClick PlayButton, HtmlA.class "controlButton" ]
                                [ Html.text "שחק" ]
                            ]
                        , Html.div [ HtmlA.class "playerSelection" ]
                            [ Html.button
                                [ HtmlA.class "playerSelectButton", HtmlEvents.onClick PlayerSelectLeft ]
                                [ Html.text "<" ]
                            , Html.div
                                [ HtmlA.class "playerSelectShow" ]
                                [ Html.img
                                    [ HtmlA.class "playerSelectShowImg", HtmlA.src (playerSrc model.player) ]
                                    []
                                ]
                            , Html.button
                                [ HtmlA.class "playerSelectButton", HtmlEvents.onClick PlayerSelectRight ]
                                [ Html.text ">" ]
                            ]
                        ]
                    , Html.div [ HtmlA.class "menuBottomSection" ]
                        [ Html.p
                            [ HtmlA.class "pDescription" ]
                            [ Html.text "המשחק תוכנת על ידי "
                            , Html.a
                                [ HtmlA.class "pDescription"
                                , HtmlA.href "http://www.github.com/48thFlame"
                                ]
                                [ Html.text "אבישי" ]
                            ]
                        ]
                    ]
                ]
        )
