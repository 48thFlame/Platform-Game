module Main exposing (main)

import Browser
import Browser.Events as Events
import Constants exposing (..)
import Engine exposing (..)
import Game exposing (..)
import Html
import Html.Attributes as HtmlA
import Svg
import Svg.Attributes as SvgA


type alias Model =
    { gs : GameState
    , keys : KeysPressed
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { gs = newGameState
      , keys = initialKeysPressed
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyDown String
    | KeyUp String
    | Blur Events.Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame deltaTime ->
            -- main game loop
            let
                delta =
                    deltaTime / 1000
            in
            ( { model | gs = updateGameStateModelCall delta model.keys model.gs }, Cmd.none )

        KeyDown key ->
            -- add key to model.keys
            ( applyFuncToModelKeys model (addKey key), Cmd.none )

        KeyUp key ->
            -- remove key from model.keys
            ( applyFuncToModelKeys model (removeKey key), Cmd.none )

        Blur _ ->
            -- clear model.keys
            ( applyFuncToModelKeys model clearKeys, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ HtmlA.class "canvasContainer" ]
        [ Svg.svg
            [ SvgA.viewBox ("0 0 " ++ canvasS.sw ++ " " ++ canvasS.sh)
            , SvgA.class "canvas"
            ]
            [ viewGameState model.gs
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta OnAnimationFrame
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        , Events.onVisibilityChange Blur
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
