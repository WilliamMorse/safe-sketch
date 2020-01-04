port module Main exposing (Model, Msg(..), init, main, penMoveEvents, penPredictedEvents, pointToString, stroke, subscriptions, svgCanvas, svgPoint, svgPolylineStringFromPoints, unpackEvents, update, view)

import Browser
import Browser.Dom
import Browser.Events as Be
import Element exposing (..)
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Pointer exposing (DeviceType, Event, blockContextMenu, eventDecoder, onDown, onUp)
import Svg as S exposing (Svg)
import Svg.Attributes as Sa
import Svg.Lazy as So
import Task
import Vector as Vector exposing (Point)


port penMoveEvents : (Encode.Value -> msg) -> Sub msg


port penPredictedEvents : (Encode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = Html.Lazy.lazy view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { inputType : DeviceType
    , pointerDown : Bool
    , currentStroke : List Point
    , predictedStroke : List Point
    , currentStrokeCoalesced : List Point
    , strokes : List (List Point)
    , viewportHeight : Float
    , viewportWidth : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { inputType = Pointer.Pen
      , pointerDown = False
      , currentStroke = []
      , predictedStroke = []
      , currentStrokeCoalesced = []

      -- hadcoded some starting strokes to test the smoothing
      , strokes = [] --[ [ ( 100, 100 ), ( 500, 500 ), ( 100, 600 ), ( 200, 600 ) ] ]
      , viewportHeight = 30
      , viewportWidth = 30
      }
    , Task.perform UpdateViewport Browser.Dom.getViewport
    )


type Msg
    = NoOp
    | Down Pointer.Event
    | Move Value
    | Prediction Value
    | Up Pointer.Event
    | UpdateViewport Browser.Dom.Viewport
    | WindowResize Int Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Be.onResize WindowResize
        , penMoveEvents Move
        , penPredictedEvents Prediction
        ]


unpackEvents : Value -> List Event
unpackEvents ev =
    case Decode.decodeValue (Decode.list eventDecoder) ev of
        Ok evl ->
            evl

        Err _ ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Down event ->
            if event.pointerType == Pointer.Pen then
                ( { model
                    | pointerDown = True
                    , inputType = event.pointerType
                    , currentStroke = List.append model.currentStroke [ ( event.offsetX, event.offsetY ) ]
                    , currentStrokeCoalesced = List.append model.currentStrokeCoalesced [ ( event.offsetX, event.offsetY ) ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Move portEvent ->
            case unpackEvents portEvent of
                event :: restOfEvents ->
                    ( { model
                        | inputType = event.pointerType
                        , currentStroke =
                            event
                                :: restOfEvents
                                |> List.map (\e -> ( e.offsetX, e.offsetY ))
                                |> List.append model.currentStroke
                        , currentStrokeCoalesced =
                            event
                                |> (\e -> ( e.offsetX, e.offsetY ))
                                |> (\p -> p :: model.currentStrokeCoalesced)
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        Prediction portEvent ->
            case unpackEvents portEvent of
                event :: restOfEvents ->
                    ( { model
                        | predictedStroke =
                            event
                                :: restOfEvents
                                |> List.map (\e -> ( e.offsetX, e.offsetY ))
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        Up event ->
            if event.pointerType == Pointer.Pen then
                ( { model
                    -- Now that we have render time, update the view with the denser stroke points
                    | strokes =
                        model.strokes
                            ++ [ model.currentStroke
                                    ++ [ ( event.offsetX, event.offsetY ) ]
                               ]
                    , currentStrokeCoalesced = []
                    , currentStroke = []
                    , predictedStroke = []
                    , pointerDown = False
                    , inputType = event.pointerType
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        UpdateViewport vp ->
            let
                w =
                    vp.viewport.width

                h =
                    vp.viewport.height
            in
            ( { model | viewportWidth = w, viewportHeight = h }, Cmd.none )

        WindowResize w h ->
            ( { model | viewportWidth = toFloat w, viewportHeight = toFloat h }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
    <|
        column
            [ centerX
            , centerY
            ]
            [ el
                [ width fill
                , height fill
                , Border.width 3
                , htmlAttribute <| Pointer.onDown Down
                , htmlAttribute <| Pointer.onUp Up
                , htmlAttribute <| blockContextMenu NoOp
                ]
                (html <| So.lazy svgCanvas model)
            ]


svgPoint : Point -> S.Svg Msg
svgPoint ( x, y ) =
    S.circle
        [ Sa.cx <| String.fromFloat x
        , Sa.cy <| String.fromFloat y
        , Sa.r "2"
        , Sa.fill "grey"
        ]
        []


pointToString : Point -> String
pointToString ( x, y ) =
    String.append
        (String.fromFloat x ++ ",")
        (String.fromFloat y ++ " ")


svgPolylineStringFromPoints : List Point -> String
svgPolylineStringFromPoints points =
    points
        |> List.map pointToString
        |> String.concat


stroke : List Point -> S.Svg Msg
stroke points =
    S.polyline
        [ Sa.fill "none"
        , Sa.stroke "black"
        , Sa.strokeWidth "2"
        , Sa.strokeLinecap "round"
        , Sa.strokeLinejoin "round"
        , Sa.points (svgPolylineStringFromPoints points)
        ]
        []


svgCanvas : Model -> Svg Msg
svgCanvas model =
    let
        width =
            String.fromFloat <| model.viewportWidth - 30

        height =
            String.fromFloat <| model.viewportHeight - 30
    in
    S.svg
        [ Sa.width width
        , Sa.height height
        , Sa.viewBox <|
            ""
                ++ "0"
                ++ " "
                ++ "0"
                ++ " "
                ++ width
                ++ " "
                ++ height
        , Sa.id "sketchspace"
        ]
        (List.concat
            [ List.map stroke model.strokes
            , [ stroke (model.currentStroke ++ model.predictedStroke) ]
            ]
        )
