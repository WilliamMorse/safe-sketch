port module Sketch exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element
    exposing
        ( centerX
        , centerY
        , column
        , el
        , fill
        , height
        , html
        , htmlAttribute
        , layout
        , width
        )
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Pointer
import Svg exposing (Svg)
import Svg.Attributes as Sa
import Svg.Lazy
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



-- Subscriptions --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize WindowResize
        , penMoveEvent Move
        ]



-- Update --


type Msg
    = NoOp
    | Down Pointer.Event
    | Move Value
    | Up Pointer.Event
    | UpdateViewport Browser.Dom.Viewport
    | WindowResize Int Int


getOffset : Pointer.Event -> Point
getOffset { offsetX, offsetY } =
    ( offsetX, offsetY )


type alias EventBundle =
    { events : List Pointer.Event
    , predictions : List Pointer.Event
    }


bundleDecoder : Decoder EventBundle
bundleDecoder =
    Decode.map2 EventBundle
        (Decode.field "events" (Decode.list Pointer.eventDecoder))
        (Decode.field "predictions" (Decode.list Pointer.eventDecoder))


updateCurrentStroke : Model -> EventBundle -> Model
updateCurrentStroke model eventBundle =
    { model
        | currentStroke =
            List.foldl (getOffset >> (::))
                model.currentStroke
                eventBundle.events
        , predictedStroke =
            List.foldl (getOffset >> (::))
                []
                eventBundle.predictions
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Down event ->
            ( if event.pointerType == Pointer.Pen then
                -- Use two copies of the event to render the first line segment
                { model | currentStroke = [ getOffset event, getOffset event ] }

              else
                model
            , Cmd.none
            )

        Move bundeledEvent ->
            ( Decode.decodeValue bundleDecoder bundeledEvent
                |> Result.map (updateCurrentStroke model)
                |> Result.withDefault model
            , Cmd.none
            )

        Up event ->
            ( if event.pointerType == Pointer.Pen then
                { model
                    | strokes =
                        (getOffset event :: model.currentStroke)
                            :: model.strokes
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



-- View --


pointToString : Point -> String
pointToString ( x, y ) =
    " " ++ String.fromFloat x ++ "," ++ String.fromFloat y


svgPolylineStringFromPoints : List Point -> String
svgPolylineStringFromPoints points =
    points
        |> List.map pointToString
        |> String.concat


polyline : List Point -> Svg Msg
polyline points =
    Svg.polyline
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
    Svg.svg
        [ Sa.width width
        , Sa.height height
        , Sa.viewBox <| "0 0 " ++ width ++ " " ++ height
        , Sa.id "sketchspace"
        ]
    <|
        List.foldl (Svg.Lazy.lazy polyline >> (::)) [] <|
            (model.predictedStroke ++ model.currentStroke)
                :: model.strokes


view : Model -> Html Msg
view model =
    layout
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
                , htmlAttribute <| Pointer.blockContextMenu NoOp
                ]
                (html <| svgSketchSpace model)
            ]
