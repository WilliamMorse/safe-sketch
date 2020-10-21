port module ScrollSketch exposing (..)

import Browser
import Browser.Dom
import Browser.Events as Be
import Canvas
import Canvas.Settings as Ca
import Canvas.Settings.Line as La
import Color
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
import Svg.Keyed as Keyed
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
    , strokes : List ( String, List Point )
    , viewportHeight : Float
    , viewportWidth : Float
    , pageWidth : Float
    , pageHeight : Float
    , viewport : Browser.Dom.Viewport
    }



--hi


init : ( Model, Cmd Msg )
init =
    ( { inputType = Pointer.Pen
      , pointerDown = False
      , currentStroke = []
      , predictedStroke = []
      , strokes = []
      , viewportWidth = 30
      , viewportHeight = 30
      , pageWidth = 20
      , pageHeight = 40
      , viewport =
            { scene =
                { width = 0
                , height = 0
                }
            , viewport =
                { x = 0
                , y = 0
                , width = 0
                , height = 0
                }
            }
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


unpackEvents : Value -> List Event
unpackEvents ev =
    case Decode.decodeValue (Decode.list eventDecoder) ev of
        Ok evl ->
            evl

        Err _ ->
            []


type alias EventBundle =
    { events : List Event
    , predictions : List Event
    }


decodeBundle : Decoder EventBundle
decodeBundle =
    Decode.map2 EventBundle
        (Decode.field "events" (Decode.list eventDecoder))
        (Decode.field "predictions" (Decode.list eventDecoder))


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
                    , currentStroke =
                        List.append model.currentStroke
                            [ ( event.offsetX, event.offsetY )
                            ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Move bundeledEvent ->
            case Decode.decodeValue decodeBundle bundeledEvent of
                Ok evb ->
                    case evb.events of
                        event :: restOfEvents ->
                            ( { model
                                | inputType = event.pointerType
                                , currentStroke =
                                    event
                                        :: restOfEvents
                                        |> List.map (\e -> ( e.offsetX, e.offsetY ))
                                        |> List.append model.currentStroke
                                , predictedStroke =
                                    evb.predictions
                                        |> List.map (\e -> ( e.offsetX, e.offsetY ))
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
                    -- Now that we have render time, update the view with the denser stroke points
                    | strokes =
                        model.strokes
                            ++ [ ( String.fromInt event.pointerId
                                 , model.currentStroke
                                    ++ [ ( event.offsetX, event.offsetY ) ]
                                 )
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
            ( { model | viewportWidth = w, viewportHeight = h, viewport = vp }, Cmd.none )

        WindowResize w h ->
            ( { model
                | viewportWidth = toFloat w
                , viewportHeight = toFloat h
              }
            , Task.perform UpdateViewport Browser.Dom.getViewport
            )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        ]
    <|
        Element.column []
            [ Element.row
                [ centerX
                , centerY
                ]
                [ el
                    [ width fill
                    , height fill
                    , Border.width 3
                    , htmlAttribute <| blockContextMenu NoOp
                    ]
                  <|
                    html <|
                        svgCanvas
                            ( round (model.viewportWidth * (2 / 5))
                            , round (model.viewportHeight * 4)
                            )
                            (List.map (scaleStroke (2 / 3))
                                (( "-1", model.currentStroke )
                                    :: model.strokes
                                )
                            )
                , el
                    [ width fill
                    , height fill
                    , Border.width 3
                    , htmlAttribute <| Pointer.onDown Down
                    , htmlAttribute <| Pointer.onUp Up
                    , htmlAttribute <| blockContextMenu NoOp
                    , htmlAttribute <| Html.Attributes.id "sketchspace"
                    , htmlAttribute <| Html.Attributes.style "touch-action" "none"
                    , htmlAttribute <| Html.Attributes.style "user-select" "none"
                    ]
                    (html <|
                        svgCanvas
                            ( round (model.viewportWidth * (3 / 5))
                            , round (model.viewportHeight * 4)
                            )
                            (( "-1", model.currentStroke )
                                :: model.strokes
                            )
                    )
                ]
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


polyline : ( String, List Point ) -> ( String, S.Svg Msg )
polyline ( idString, points ) =
    ( idString
    , S.polyline
        [ Sa.fill "none"
        , Sa.stroke "black"
        , Sa.strokeWidth "2"
        , Sa.strokeLinecap "round"
        , Sa.strokeLinejoin "round"
        , Sa.points (svgPolylineStringFromPoints points)
        ]
        []
    )


svgCanvas : ( Int, Int ) -> List ( String, List Point ) -> Svg Msg
svgCanvas ( width, height ) idStrokes =
    Keyed.node "svg"
        [ Sa.width <| String.fromInt width
        , Sa.height <| String.fromInt height
        , Sa.viewBox <|
            ""
                ++ "0"
                ++ " "
                ++ "0"
                ++ " "
                ++ String.fromInt width
                ++ " "
                ++ String.fromInt height
        ]
    <|
        List.map
            polyline
            idStrokes


scalePoint : Float -> ( Float, Float ) -> ( Float, Float )
scalePoint scaleBy =
    Tuple.mapBoth ((*) scaleBy) ((*) scaleBy)


scaleStroke : Float -> ( String, List Point ) -> ( String, List Point )
scaleStroke scaleBy ( id, stroke ) =
    ( id, List.map (scalePoint scaleBy) stroke )



-- try to make a canvas one now


canvas : ( Int, Int ) -> List (List Point) -> Html msg
canvas widthHeight strokes =
    Canvas.toHtml widthHeight
        []
        (List.map
            (\points ->
                Canvas.shapes [ Ca.stroke Color.black, La.lineWidth 3 ] <|
                    case points of
                        h :: t ->
                            [ Canvas.path h
                                (List.map Canvas.lineTo t)
                            ]

                        [] ->
                            []
            )
            strokes
        )


testCanvas : Html msg
testCanvas =
    Canvas.toHtml ( 400, 400 )
        []
        [ Canvas.shapes [ Ca.stroke Color.black ]
            [ Canvas.path ( 100, 100 )
                [ Canvas.lineTo ( 200, 200 ) ]
            ]
        ]
