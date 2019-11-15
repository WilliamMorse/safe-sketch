module Main exposing (Model, Msg(..), Point, alwaysPreventDefault, blockContextMenu, init, main, svgCanvas, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events as Be
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Svg as S
import Svg.Attributes as Sa
import Svg.Lazy as L
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Point =
    ( Float, Float )


type alias Model =
    { inputType : Pointer.DeviceType
    , tiltX : Float
    , tiltY : Float
    , pressure : Float
    , latestEvent : Maybe Pointer.Event
    , offsetPos : Point
    , pointerDown : Bool
    , lastStroke : List Point
    , strokes : List (List Point)
    , viewportHeight : Float
    , viewportWidth : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { inputType = Pointer.MouseType
      , tiltX = 0.0
      , tiltY = 0.0
      , pressure = 0
      , latestEvent = Nothing
      , pointerDown = False
      , offsetPos = ( 0, 0 )
      , lastStroke = []
      , strokes = [ [] ]
      , viewportHeight = 10
      , viewportWidth = 10
      }
    , Task.perform UpdateViewport Browser.Dom.getViewport
    )


type Msg
    = NoOp
    | Down Pointer.Event
    | Move Pointer.Event
    | Up Pointer.Event
    | UpdateViewport Browser.Dom.Viewport
    | WindowResize Int Int
    | AnimationUpdate Float


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Be.onResize WindowResize
        , if model.pointerDown then
            Be.onAnimationFrameDelta AnimationUpdate

          else
            Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Down event ->
            ( { model | pointerDown = True }
            , Cmd.none
            )

        Move event ->
            if event.pointerType == Pointer.PenType then
                ( { model
                    | inputType = event.pointerType
                    , tiltX = event.contactDetails.tiltX
                    , tiltY = event.contactDetails.tiltY
                    , pressure = event.contactDetails.pressure
                    , latestEvent = Just event
                    , offsetPos = event.pointer.offsetPos
                    , lastStroke = List.append model.lastStroke [ event.pointer.offsetPos ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Up event ->
            ( { model
                | strokes = model.lastStroke :: model.strokes
                , lastStroke = []
                , pointerDown = False
              }
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
            ( { model | viewportWidth = toFloat w, viewportHeight = toFloat h }, Cmd.none )

        AnimationUpdate mills ->
            ( model, Cmd.none )


blockContextMenu : Msg -> Html.Attribute Msg
blockContextMenu msg =
    Html.Events.preventDefaultOn
        "contextmenu"
        (D.map (\a -> ( a, True )) (D.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


svgCanvas : Model -> Element Msg
svgCanvas model =
    let
        width =
            String.fromFloat <| model.viewportWidth - 40

        height =
            String.fromFloat <| model.viewportHeight - 40

        halfWidth =
            String.fromFloat ((model.viewportWidth - 40) / 2)

        halfHeight =
            String.fromFloat ((model.viewportHeight - 40) / 2)
    in
    html <|
        S.svg
            [ Sa.width width
            , Sa.height height
            , Sa.viewBox <|
                ""
                    ++ "-"
                    ++ halfWidth
                    ++ " "
                    ++ "-"
                    ++ halfHeight
                    ++ halfWidth
                    ++ " "
                    ++ halfHeight
            ]
            (List.concat
                [ [ stroke model.lastStroke
                  ]
                , List.map stroke model.strokes
                ]
            )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , padding 10
        , htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
    <|
        column
            [ centerX
            , centerY
            , spacing 7
            ]
            [ el
                [ Border.width 1
                , htmlAttribute <| Pointer.onDown Down
                , htmlAttribute <| Pointer.onMove Move
                , htmlAttribute <| Pointer.onUp Up
                , htmlAttribute <| blockContextMenu NoOp
                ]
                (svgCanvas model)

            {--
            , text <| "TiltX = " ++ String.fromFloat model.tiltX
            , text <| "TiltY = " ++ String.fromFloat model.tiltY
            , text <| "width = " ++ String.fromFloat model.viewportWidth
            , text <| "height = " ++ String.fromFloat model.viewportHeight
            , text <| "Pressure = " ++ String.fromFloat model.pressure
            , text <| "DeviceType = " ++ Debug.toString model.inputType
            , paragraph [] [ text <| "event: " ++ Debug.toString model.latestEvent ]
            --}
            ]


svgPoint : Point -> S.Svg Msg
svgPoint point =
    S.circle
        [ Sa.cx (point |> Tuple.first |> String.fromFloat)
        , Sa.cy (point |> Tuple.second |> String.fromFloat)
        , Sa.r "2"
        ]
        []


length : ( Point, Point ) -> Float
length ( ( x1, y1 ), ( x2, y2 ) ) =
    -- just use some pythagoris
    ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) ^ 0.5


normWithScaling : Float -> Point -> ( Point, Point ) -> Point
normWithScaling smoothing basePoint pointPair =
    let
        c =
            length pointPair

        ( ( x1, y1 ), ( x2, y2 ) ) =
            pointPair

        ( x3, y3 ) =
            basePoint
    in
    ( x3
        + ((x2 - x1) * (smoothing * c))
    , y3
        + ((y2 - y1) * (smoothing * c))
    )


pointToString : Point -> String
pointToString ( x1, y1 ) =
    " "
        ++ String.fromFloat x1
        ++ " "
        ++ String.fromFloat y1


myS : Point -> Point -> String
myS perce controll =
    "S"
        ++ pointToString perce
        ++ pointToString controll


points_to_smooth_svg_path : List Point -> Float -> String
points_to_smooth_svg_path points smoothingFactor =
    -- makes the args to the svg path command following this algo:
    -- https://medium.com/@francoisromain/smooth-a-svg-path-with-cubic-bezier-curves-e37b49d46c74
    let
        -- these are the points with bezier controll points ascoceated with them
        basePoints =
            List.drop 1 points

        -- combined with the normal points we use these to define the tangents at each base point
        gashiftedPoints =
            List.drop 2 points

        --  map2 will truncate the longer list
        pointPairs =
            List.map2 Tuple.pair points gashiftedPoints

        bezierControllPoints =
            List.map2 (normWithScaling smoothingFactor) basePoints pointPairs

        firstPoint =
            let
                point =
                    case List.head points of
                        Just p ->
                            p

                        Nothing ->
                            ( 0, 0 )
            in
            "M" ++ pointToString point

        subsequentPoints =
            List.map2 myS basePoints bezierControllPoints
    in
    String.join " " <| firstPoint :: subsequentPoints


smoothStroke : List Point -> S.Svg Msg
smoothStroke points =
    S.path
        [ Sa.fill "none"
        , Sa.stroke "black"
        , Sa.d (points_to_smooth_svg_path points 0.05)
        ]
        []


svgPolylineStringFromPoints : List Point -> String
svgPolylineStringFromPoints points =
    points
        |> List.map
            (\p ->
                (++)
                    (String.fromFloat (Tuple.first p) ++ ",")
                    (String.fromFloat (Tuple.second p) ++ " ")
            )
        |> String.concat


stroke : List Point -> S.Svg Msg
stroke points =
    S.polyline
        [ Sa.fill "none"
        , Sa.stroke "black"
        , Sa.strokeWidth "8"
        , Sa.points (svgPolylineStringFromPoints points)
        ]
        []
