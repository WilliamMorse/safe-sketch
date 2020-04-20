port module Main exposing (Model, Msg(..), Point, blockContextMenu, decodeBundle, defaultEvent, init, main, penMoveEvent, pressureChart, subscriptions, update, velocity, view, xyChart)

import Browser
import Element exposing (Column, Element, column, fill, height, html, htmlAttribute, padding, row, spacing, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import LineChart
import PenTilt as Tilt
import Pointer exposing (DeviceType(..), Event, eventDecoder, onDown, onMove)
import Vector as V


port penMoveEvent : (Encode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Point =
    { x : Float
    , y : Float
    }


defaultEvent : Event
defaultEvent =
    { pointerId = -1
    , width = 0
    , height = 0
    , pressure = 0
    , tangentialpressure = 0
    , tiltX = 0
    , tiltY = 0
    , twist = 0
    , pointerType = Mouse
    , isPrimary = False
    , offsetX = 0
    , offsetY = 0
    , screenX = 0
    , screenY = 0
    , pageX = 0
    , pageY = 0
    , timestamp = 0
    }


type alias Model =
    { events : List Event
    , predictions : List Event
    }


init : ( Model, Cmd Msg )
init =
    ( Model [ defaultEvent ] [], Cmd.none )


type Msg
    = NoOp
    | Down Event
    | Move Value


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ penMoveEvent Move ]


decodeBundle : Decode.Decoder Model
decodeBundle =
    Decode.map2 Model
        (Decode.field "events" (Decode.list eventDecoder))
        (Decode.field "predictions" (Decode.list eventDecoder))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Down ev ->
            if ev.pointerType == Pen then
                ( Model [ ev ] [], Cmd.none )

            else
                ( model, Cmd.none )

        Move ev ->
            case Decode.decodeValue decodeBundle ev of
                Ok bundle ->
                    ( Model (model.events ++ bundle.events) bundle.predictions, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        , htmlAttribute <| onDown Down
        , htmlAttribute <| blockContextMenu NoOp
        , htmlAttribute <| Html.Attributes.id "pen"

        --, htmlAttribute <| onUp Pointer
        ]
        (column
            [ padding 20
            , spacing 20
            , width fill
            , Font.variant Font.tabularNumbers
            , Element.alignRight
            ]
            [ text "Pen Info Charts"
            , row [ width fill ]
                [ pressureChart model
                , velocityChart model
                ]
            ]
        )


blockContextMenu : Msg -> Html.Attribute Msg
blockContextMenu msg =
    Html.Events.preventDefaultOn
        "contextmenu"
        (Decode.map (\m -> ( m, True )) (Decode.succeed msg))


pressureChart : Model -> Element msg
pressureChart model =
    html <|
        LineChart.view1
            .x
            .y
            (List.indexedMap (\x y -> Point (toFloat x) y.pressure) model.events)


velocityChart : Model -> Element msg
velocityChart model =
    html <|
        LineChart.view1
            .x
            .y
            (List.indexedMap (\x y -> Point (toFloat x) y) (velocity model))


xyChart : Model -> Element msg
xyChart model =
    html <|
        LineChart.view2
            .x
            .y
            (List.indexedMap (\x y -> Point (toFloat x) y.pageX) model.events)
            (List.indexedMap (\x y -> Point (toFloat x) y.pageY) model.events)


velocity : Model -> List Float
velocity model =
    let
        t1 =
            List.map (\e -> e.timestamp) model.events

        t2 =
            List.drop 1 t1

        p1 =
            List.map (\e -> ( e.screenX, e.screenY )) model.events

        p2 =
            List.drop 1 p1

        posDiff =
            List.map2 V.rel p1 p2
                |> List.map V.length

        tDiff =
            List.map2 (-) t2 t1

        vel =
            List.map2 (/) posDiff tDiff
    in
    posDiff
