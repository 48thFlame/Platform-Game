module Main exposing (main)

import Browser
import Browser.Events as Events
import Constants exposing (..)
import Engine exposing (..)
import Game exposing (..)
import Html
import Html.Attributes as HtmlA
import Json.Decode as Decode
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
      , keys = initialKeysPressed
      , middlePos = { x = d.x, y = d.y }
      , mousePressed = False
      , mousePos = newPosition 0 0
      }
    , Cmd.none
    )
        |> Debug.log "init"



-- MODEL


type alias Model =
    { gs : GameState
    , keys : KeysPressed

    -- , pageDim : Dimension
    , middlePos : Position
    , mousePressed : Bool
    , mousePos : Position
    }



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



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyDown String
    | KeyUp String
    | MouseDown Float Float
    | MouseMove Float Float
    | MouseUp
    | Blur Events.Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model | gs = updateGameStateModelCall delta model.keys mouse model.gs }, Cmd.none )

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
