module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Svg as S
import Svg.Attributes as Sa
import Svg.Lazy as L


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Point =
    ( Float, Float )


type alias Model =
    { iT : Pointer.DeviceType
    , tiltX : Float
    , tiltY : Float
    , pressure : Float
    , latestEvent : Maybe Pointer.Event
    , offsetPos : Point
    , pagePos : Point
    , screenPos : Point
    , lastStroke : List Point
    , strokes : List (List Point)
    }


init : ( Model, Cmd Msg )
init =
    ( { iT = Pointer.MouseType
      , tiltX = 0.0
      , tiltY = 0.0
      , pressure = 0
      , latestEvent = Nothing
      , offsetPos = ( 0, 0 )
      , pagePos = ( 0, 0 )
      , screenPos = ( 0, 0 )
      , lastStroke = []
      , strokes = [ [] ]
      }
    , Cmd.none
    )


type Msg
    = Move Pointer.Event
    | Over Pointer.Event
    | Down Pointer.Event
    | Vent Pointer.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move event ->
            if event.pointerType == Pointer.PenType then
                ( { model
                    | iT = event.pointerType
                    , tiltX = event.contactDetails.tiltX
                    , tiltY = event.contactDetails.tiltY
                    , pressure = event.contactDetails.pressure
                    , latestEvent = Just event
                    , offsetPos = event.pointer.offsetPos
                    , pagePos = event.pointer.pagePos
                    , screenPos = event.pointer.screenPos
                    , lastStroke = List.append model.lastStroke [ event.pointer.offsetPos ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Over event ->
            let
                tX =
                    event.contactDetails.tiltX

                tY =
                    event.contactDetails.tiltY
            in
            ( { model
                | tiltX = tX
                , tiltY = tY
              }
            , Cmd.none
            )

        Down event ->
            ( { model
                | strokes = model.lastStroke :: model.strokes
                , lastStroke = []
              }
            , Cmd.none
            )

        Vent event ->
            ( model, Cmd.none )


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
        + ((x2 - x1) * smoothing / c)
    , y3
        + ((y2 - y1) * smoothing / c)
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
            "M " ++ pointToString point

        subsequentPoints =
            List.map2 myS basePoints bezierControllPoints
    in
    String.join "\n" <| firstPoint :: subsequentPoints


svgPointsToPolylineString : List Point -> String
svgPointsToPolylineString points =
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
        , Sa.points (svgPointsToPolylineString points)
        ]
        []


svgCanvas : Model -> Element Msg
svgCanvas model =
    html <|
        S.svg
            [ Sa.width "500"
            , Sa.height "500"
            , Sa.viewBox "0 0 500 500"
            ]
            (List.concat
                [ [ S.polyline
                        [ Sa.fill "none"
                        , Sa.stroke "black"
                        , Sa.overlineThickness "3"
                        , Sa.points (svgPointsToPolylineString model.lastStroke)
                        ]
                        []
                  ]
                , List.map stroke model.strokes

                {--
                --, List.map svgPoint model.lastStroke
                , [ S.circle
                        [ Sa.cx (model.offsetPos |> Tuple.first |> String.fromFloat)
                        , Sa.cy (model.offsetPos |> Tuple.second |> String.fromFloat)
                        , Sa.r "5"
                        ]
                        []
                  ]
                --}
                ]
            )


myOnDown : (Pointer.Event -> msg) -> Html.Attribute msg
myOnDown =
    { stopPropagation = True, preventDefault = True }
        |> Pointer.onWithOptions "pointerdown"


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , htmlAttribute <| Html.Attributes.style "webkit-touch-callout" "none"
        , htmlAttribute <| Html.Attributes.style "webkit-user-select" "none"
        , htmlAttribute <| Html.Attributes.style "khtml-user-select" "none"
        , htmlAttribute <| Html.Attributes.style "moz-user-select" "none"
        , htmlAttribute <| Html.Attributes.style "ms-user-select" "none"
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        , padding 20
        ]
    <|
        column
            [ width fill
            , spacing 7
            ]
            [ text <| "TiltX = " ++ String.fromFloat model.tiltX
            , text <| "TiltY = " ++ String.fromFloat model.tiltY
            , text <| "Pressure = " ++ String.fromFloat model.pressure
            , text <| "DeviceType = " ++ Debug.toString model.iT
            , el
                [ Element.explain Debug.todo
                , htmlAttribute <| Pointer.onMove Move
                , htmlAttribute <| Pointer.onUp Down
                , htmlAttribute <| myOnDown Vent
                ]
                (svgCanvas model)
            , el [ width <| px 40, height <| px 40, Element.explain Debug.todo ] (text "hi")
            ]
