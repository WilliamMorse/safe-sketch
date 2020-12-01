port module Sketch exposing (main)

import Browser
import Browser.Dom
import Browser.Events as Be
import Element exposing (centerX, centerY, column, el, fill, height, html, htmlAttribute, width)
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Pointer exposing (Event, blockContextMenu, eventDecoder, onDown, onUp)
import Svg as S exposing (Svg)
import Svg.Attributes as Sa
import Svg.Lazy as So
import Task


port penMoveEvent : (Encode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = Html.Lazy.lazy view
        , update = update
        , subscriptions = subscriptions
        }


type alias Point =
    ( Float, Float )


type alias Model =
    { currentStroke : List Point
    , predictedStroke : List Point
    , strokes : List (List Point)
    , viewportHeight : Float
    , viewportWidth : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { currentStroke = []
      , predictedStroke = []
      , strokes = []
      , viewportHeight = 30
      , viewportWidth = 30
      }
    , Task.perform UpdateViewport Browser.Dom.getViewport
    )


type Msg
    = NoOp
    | Down Pointer.Event
    | Move Value
    | Up Pointer.Event
    | UpdateViewport Browser.Dom.Viewport
    | WindowResize Int Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Be.onResize WindowResize
        , penMoveEvent Move
        ]


type alias EventBundle =
    { events : List Event
    , predictions : List Event
    }


decodeBundle : Decoder EventBundle
decodeBundle =
    Decode.map2 EventBundle
        (Decode.field "events" (Decode.list eventDecoder))
        (Decode.field "predictions" (Decode.list eventDecoder))


getOffset : Event -> Point
getOffset { offsetX, offsetY } =
    ( offsetX, offsetY )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Down event ->
            ( if event.pointerType == Pointer.Pen then
                { model | currentStroke = [ getOffset event, getOffset event ] }

              else
                model
            , Cmd.none
            )

        Move bundeledEvent ->
            ( case Decode.decodeValue decodeBundle bundeledEvent of
                Ok evb ->
                    { model
                        | currentStroke =
                            List.foldl (getOffset >> (::))
                                model.currentStroke
                                evb.events
                        , predictedStroke =
                            List.foldl (getOffset >> (::))
                                []
                                evb.predictions
                    }

                Err _ ->
                    model
            , Cmd.none
            )

        Up event ->
            ( if event.pointerType == Pointer.Pen then
                { model
                    | strokes =
                        (getOffset event :: model.currentStroke) :: model.strokes
                    , currentStroke = []
                    , predictedStroke = []
                }

              else
                model
            , Cmd.none
            )

        UpdateViewport vp ->
            let
                w =
                    vp.viewport.width

                h =
                    vp.viewport.height
            in
            ( { model | viewportWidth = w, viewportHeight = h }, Cmd.none )

        WindowResize w h ->
            ( { model | viewportWidth = toFloat w, viewportHeight = toFloat h }
            , Task.perform UpdateViewport Browser.Dom.getViewport
            )


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
                (html <| svgSketchSpace model)
            ]


svgLine : Point -> Point -> S.Svg Msg
svgLine a b =
    S.line
        [ Sa.x1 <| String.fromFloat <| Tuple.first a
        , Sa.y1 <| String.fromFloat <| Tuple.second a
        , Sa.x2 <| String.fromFloat <| Tuple.first b
        , Sa.y2 <| String.fromFloat <| Tuple.second b
        , Sa.stroke "black"
        , Sa.strokeWidth "2"
        ]
        []


svgLines : List Point -> List (Svg Msg)
svgLines p1 =
    let
        p2 =
            p1 |> List.drop 1
    in
    List.map2 (So.lazy2 svgLine) p1 p2


pointToString : Point -> String
pointToString ( x, y ) =
    (String.fromFloat x |> String.cons ' ')
        ++ (String.fromFloat y |> String.cons ',')


svgPolylineStringFromPoints : List Point -> String
svgPolylineStringFromPoints points =
    points
        |> List.map pointToString
        |> String.concat


polyline : List Point -> S.Svg Msg
polyline points =
    S.polyline
        [ Sa.fill "none"
        , Sa.stroke "darkGrey"
        , Sa.strokeWidth "10"
        , Sa.strokeLinecap "round"
        , Sa.strokeLinejoin "round"
        , Sa.points (svgPolylineStringFromPoints points)
        ]
        []


svgSketchSpace : Model -> Svg Msg
svgSketchSpace model =
    let
        width =
            String.fromFloat <| model.viewportWidth - 30

        height =
            String.fromFloat <| model.viewportHeight - 30
    in
    S.svg
        [ Sa.width width
        , Sa.height height
        , Sa.viewBox <| "0 0 " ++ width ++ " " ++ height
        , Sa.id "sketchspace"
        ]
    <|
        List.foldl (So.lazy polyline >> (::)) [] <|
            (model.predictedStroke ++ model.currentStroke)
                :: model.strokes
