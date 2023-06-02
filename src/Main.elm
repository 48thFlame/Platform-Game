module Main exposing (main)

import Browser
import Browser.Events as Events
import Common exposing (..)
import Constants exposing (..)
import Engine exposing (..)
import Game exposing (..)
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlEvents
import Html.Events.Extra.Touch as Touch
import Random
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
    , y : Float
    , isMobile : Bool
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel d =
    ( { gs = newGameState
      , gameStatus = Menu
      , keys = initialKeysPressed
      , isMobile = d.isMobile
      , rand = 0
      , middlePos = { x = d.x, y = d.y }
      , mousePressed = False
      , mousePos = newPosition 0 0
      }
    , randCommand
    )


randCommand : Cmd Msg
randCommand =
    Random.generate NewRandom (Random.float 0 1)



-- MODEL


type alias Model =
    { gs : GameState
    , gameStatus : GameStatus
    , keys : KeysPressed
    , rand : Float
    , isMobile : Bool
    , middlePos : Position
    , mousePressed : Bool
    , mousePos : Position
    }



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
                            [ viewGameState model.gs
                            ]
                in
                [ Svg.svg
                    [ SvgA.class "canvasContainer"
                    , SvgA.viewBox ("0 0 " ++ canvasS.sw ++ " " ++ canvasS.sh)
                    , Touch.onStart (ClickDown << touchCoordinates)
                    , Touch.onMove (ClickMove << touchCoordinates)
                    , Touch.onEnd (\_ -> ClickUp)
                    ]
                    (if model.isMobile then
                        [ Svg.g [ SvgA.class "mobileExp" ]
                            [ viewEntity
                                "assets/mobileBackground.png"
                                { pos = newPosition 0 0
                                , dim = newDimension canvasS.w canvasS.h
                                , rot = initialRotation
                                }
                            ]
                        , canvas
                        ]

                     else
                        [ canvas ]
                    )
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
                [ Html.h1 [ HtmlA.class "title" ] [ Html.text "ברוכים הבאים למשחק!" ]
                , Html.div [ HtmlA.class "controlContainer" ]
                    [ Html.button [ HtmlEvents.onClick PlayButton, HtmlA.class "controlButton" ] [ Html.text "שחק" ]
                    , Html.br [] []
                    , Html.br [] []
                    , Html.h3 [ HtmlA.class "subTitle" ] [ Html.text "הסבר:" ]
                    , Html.p
                        [ HtmlA.class "pDescription"
                        ]
                        [ Html.text "קפוץ מפלטפורמה לפלטפורמה והימנע ממגע בלבה."
                        ]
                    , Html.p [ HtmlA.class "pDescription" ] [ Html.text "המשחק תוכנת על ידי ", Html.a [ HtmlA.class "pDescription", HtmlA.href "http://www.github.com/48thFlame" ] [ Html.text "אבישי" ] ]
                    ]
                ]
        )


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyDown String
    | KeyUp String
    | ClickDown ( Float, Float )
    | ClickMove ( Float, Float )
    | ClickUp
    | NewRandom Float
    | Blur Events.Visibility
    | PlayButton
    | ToMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayButton ->
            ( { model | gameStatus = Playing, gs = newGameState }, Cmd.none )

        ToMenu ->
            ( { model | gameStatus = Menu }, Cmd.none )

        OnAnimationFrame deltaTime ->
            -- main game loop
            let
                delta =
                    deltaTime / 1000

                mouse =
                    if model.mousePressed then
                        ( Just model.mousePos, model.middlePos )

                    else
                        ( Nothing, model.middlePos )
            in
            ( { model
                | gs = updateGameStateModelCall delta model.rand model.keys mouse model.gs
                , gameStatus = getGameOverStatus model.gs
              }
            , randCommand
            )

        NewRandom r ->
            ( { model | rand = r }, Cmd.none )

        KeyDown key ->
            -- add key to model.keys
            ( applyFuncToModelKeys model (addKey key), Cmd.none )

        KeyUp key ->
            -- remove key from model.keys
            ( applyFuncToModelKeys model (removeKey key), Cmd.none )

        ClickDown ( x, y ) ->
            ( { model | mousePressed = True, mousePos = newPosition x y }, Cmd.none )

        ClickUp ->
            ( { model | mousePressed = False }, Cmd.none )

        ClickMove ( x, y ) ->
            ( { model | mousePos = newPosition x y }, Cmd.none )

        Blur _ ->
            -- clear model.keys
            ( applyFuncToModelKeys model clearKeys, Cmd.none )



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
