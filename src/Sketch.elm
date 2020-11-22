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
import Pointer exposing (DeviceType, Event, blockContextMenu, eventDecoder, onDown, onUp)
import Svg as S exposing (Svg)
import Svg.Attributes as Sa
import Svg.Keyed
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
    { inputType : DeviceType
    , pointerDown : Bool
    , currentStroke : List Point
    , predictedStroke : List Point
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
      , strokes = [ [ ( 100, 100 ), ( 200, 200 ) ] ]
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
            if event.pointerType == Pointer.Pen then
                ( { model
                    | pointerDown = True
                    , inputType = event.pointerType
                    , currentStroke = [ getOffset event ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Move bundeledEvent ->
            case Decode.decodeValue decodeBundle bundeledEvent of
                Ok evb ->
                    case evb.events of
                        event :: _ ->
                            ( { model
                                | inputType = event.pointerType
                                , currentStroke =
                                    model.currentStroke
                                        ++ List.map getOffset evb.events
                                , predictedStroke =
                                    List.map getOffset evb.predictions
                              }
                            , Cmd.none
                            )

                        [] ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Up event ->
            if event.pointerType == Pointer.Pen then
                ( { model
                    | strokes =
                        model.strokes
                            ++ [ model.currentStroke
                                    ++ [ ( event.offsetX, event.offsetY ) ]
                               ]
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
                (html <| svgSketchSpace model)
            ]


svgPoint : Point -> S.Svg Msg
svgPoint ( x, y ) =
    S.circle
        [ Sa.cx <| String.fromFloat x
        , Sa.cy <| String.fromFloat y
        , Sa.r "2"
        , Sa.fill "red"
        ]
        []


svgLine : Point -> Point -> S.Svg Msg
svgLine a b =
    --Debug.log "Line"
    S.line
        [ Sa.x1 <| String.fromFloat <| Tuple.first a
        , Sa.y1 <| String.fromFloat <| Tuple.second a
        , Sa.x2 <| String.fromFloat <| Tuple.first b
        , Sa.y2 <| String.fromFloat <| Tuple.second b
        , Sa.stroke "Black"
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


stroke : List Point -> S.Svg Msg
stroke points =
    --Debug.log "Stroke"
    S.polyline
        [ Sa.fill "none"
        , Sa.stroke "black"
        , Sa.strokeWidth "2"
        , Sa.strokeLinecap "round"
        , Sa.strokeLinejoin "round"
        , Sa.points (svgPolylineStringFromPoints points)
        ]
        []


packStroke : Int -> List Point -> ( String, Svg Msg )
packStroke i s =
    let
        str =
            "stroke" ++ String.fromInt i
    in
    ( str, So.lazy stroke s )


svgSketchSpace : Model -> Svg Msg
svgSketchSpace model =
    let
        width =
            String.fromFloat <| model.viewportWidth - 30

        height =
            String.fromFloat <| model.viewportHeight - 30
    in
    Svg.Keyed.node "svg"
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
    <|
        ( "current"
        , stroke <|
            model.currentStroke
                ++ model.predictedStroke
        )
            :: List.indexedMap packStroke
                model.strokes
