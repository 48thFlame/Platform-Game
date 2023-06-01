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
import Json.Decode as Decode
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
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel d =
    ( { gs = newGameState
      , gameStatus = Menu
      , keys = initialKeysPressed
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
    , middlePos : Position
    , mousePressed : Bool
    , mousePos : Position
    }



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ HtmlA.class "canvasContainer" ]
        (case model.gameStatus of
            Playing ->
                [ Svg.svg
                    [ SvgA.viewBox ("0 0 " ++ canvasS.sw ++ " " ++ canvasS.sh)
                    , SvgA.class "canvas"
                    ]
                    [ viewGameState model.gs
                    ]
                ]

            GameOver ->
                [ Html.br [] []
                , Html.h1 [ HtmlA.class "title" ] [ Html.text "נגמר המשחק!" ]
                , Html.p [ HtmlA.class "pDescription" ]
                    [ Html.text "צברת "
                    , Html.text (model.gs.score |> String.fromInt)
                    , Html.text " נקודות"
                    ]
                , Html.button [ HtmlEvents.onClick ToMenu, HtmlA.class "controlButton" ] [ Html.text "לתפריט" ]
                ]

            Menu ->
                [ Html.br [] []
                , Html.h1 [ HtmlA.class "title" ] [ Html.text "ברוכים הבאים למשחק!" ]
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
                    , Html.p [] [ Html.text "המשחק תוכנת על ידי ", Html.a [ HtmlA.href "http://www.github.com/48thFlame" ] [ Html.text "אבישי" ] ]
                    ]
                ]
        )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyDown String
    | KeyUp String
    | MouseDown Float Float
    | MouseMove Float Float
    | NewRandom Float
    | MouseUp
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

        MouseDown x y ->
            ( { model | mousePressed = True, mousePos = newPosition x y }, Cmd.none )

        MouseUp ->
            ( { model | mousePressed = False }, Cmd.none )

        MouseMove x y ->
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
                , Events.onMouseDown
                    (Decode.map2 MouseDown
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                    )
                , Events.onMouseUp (Decode.succeed MouseUp)
                , if model.mousePressed then
                    Events.onMouseMove
                        (Decode.map2 MouseMove
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                        )

                  else
                    Sub.none
                ]

            _ ->
                [ Sub.none ]
        )
