port module Main exposing (Model, Msg(..), Point, blockContextMenu, init, main, svgCanvas, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events as Be
import Element exposing (..)
import Element.Border as Border
import Element.Lazy as Soso
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Pointer exposing (DeviceType, Event, eventDecoder, onDown, onUp)
import Svg as S exposing (Svg)
import Svg.Attributes as Sa
import Svg.Keyed as Keyed
import Svg.Lazy as So
import Task


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


type alias Point =
    ( Float, Float )


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


blockContextMenu : Msg -> Html.Attribute Msg
blockContextMenu msg =
    Html.Events.preventDefaultOn
        "contextmenu"
        (Decode.map (\m -> ( m, True )) (Decode.succeed msg))


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


add : Point -> Point -> Point
add ( a1, a2 ) ( b1, b2 ) =
    let
        x =
            a1 + b1

        y =
            a2 + b2
    in
    ( x, y )


subtract : Point -> Point -> Point
subtract ( a1, a2 ) ( b1, b2 ) =
    let
        x =
            b1 - a1

        y =
            b2 - a2
    in
    ( x, y )


multiply : Point -> Float -> Point
multiply ( a1, a2 ) s =
    ( s * a1, s * a2 )


{-| Subtracts the second point from the first point vectorially
-}
dotProduct : Point -> Point -> Float
dotProduct ( a1, a2 ) ( b1, b2 ) =
    a1 * b1 + a2 * b2


length2 : Point -> Float
length2 ( a1, a2 ) =
    a1 ^ 2 + a2 ^ 2


length : Point -> Float
length a =
    length2 a ^ 0.5


distanceBetween : Point -> Point -> Float
distanceBetween a b =
    length <| subtract a b


{-| Finds the relative vector from a to b
-}
rel : Point -> Point -> Point
rel a b =
    subtract b a


norm : Point -> Point
norm a =
    multiply a (length a ^ -1)


norm2 : Point -> Point
norm2 a =
    multiply a (length2 a ^ -1)


{-| Project a onto b and return a vector
-}
proj : Point -> Point -> Point
proj a b =
    multiply
        b
        (dotProduct a b / length2 b)


projL : Point -> Point -> Float
projL a b =
    dotProduct a b / length b


quadradicControlPoints : Point -> Point -> Point -> Point
quadradicControlPoints pre base next =
    -- Take the bisector of the incoming and outgoing vectors
    let
        smoothing =
            0.4

        a =
            norm <| rel base pre

        b =
            norm <| rel base next

        tangent =
            subtract a b
    in
    add base (multiply tangent smoothing)


cubicControlPoints : Point -> Point -> Point -> ( Point, Point )
cubicControlPoints pre base next =
    let
        smoothing =
            0.4

        a =
            rel pre next
                |> proj (rel pre base)
                |> (\p -> multiply p smoothing)

        b =
            rel pre next
                |> proj (rel next base)
                |> (\p -> multiply p smoothing)
    in
    ( add base a, add base b )


cubicSpline : List Point -> S.Svg Msg
cubicSpline points =
    let
        basePoints =
            List.drop 1 points

        nextPoints =
            List.drop 2 points

        -- m starting point
        -- S (curve to) second point2 + c1
        -- C c2 c3 point 2
        ( point1, tailPoints ) =
            case points of
                a :: b ->
                    ( pointToString a, b )

                [] ->
                    ( "", [] )

        ( cp2, cp1 ) =
            List.map3 cubicControlPoints points basePoints nextPoints
                |> List.unzip

        c1 =
            List.take 1 points ++ cp1

        c2 =
            cp2
                |> List.reverse
                |> (++) (List.take 1 (List.reverse tailPoints))
                |> List.reverse

        pathPointsString =
            String.concat
                [ "M" ++ point1
                , "C"
                , List.map3
                    (\a b c ->
                        pointToString a
                            ++ pointToString b
                            ++ pointToString c
                    )
                    c1
                    c2
                    tailPoints
                    |> String.concat
                ]
    in
    S.path
        [ Sa.fill "none"
        , Sa.stroke "blue"
        , Sa.d pathPointsString
        ]
        []


normWithScaling : Float -> Point -> ( Point, Point ) -> Point
normWithScaling smoothing basePoint ( a, b ) =
    let
        c =
            distanceBetween a b

        ( ( x1, y1 ), ( x2, y2 ) ) =
            ( a, b )

        ( x3, y3 ) =
            basePoint
    in
    ( x3
        - ((x2 - x1) * (smoothing * c / 900))
    , y3
        - ((y2 - y1) * (smoothing * c / 900))
    )


pointToString : Point -> String
pointToString ( x1, y1 ) =
    ""
        ++ String.fromFloat x1
        ++ " "
        ++ String.fromFloat y1
        ++ " "


myS : Point -> Point -> String
myS perce control =
    "S"
        ++ pointToString control
        ++ pointToString perce


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

        lastPoint =
            case List.reverse points of
                [] ->
                    ""

                l :: _ ->
                    -- finish the path with a zero length control point
                    myS l l
    in
    [ [ firstPoint ], subsequentPoints, [ lastPoint ] ]
        |> List.concat
        |> String.join " "


smoothStroke : List Point -> S.Svg Msg
smoothStroke points =
    S.path
        [ Sa.fill "none"
        , Sa.stroke "green"
        , Sa.d (points_to_smooth_svg_path points 1)
        ]
        []


svgPolylineStringFromPoints : List Point -> String
svgPolylineStringFromPoints points =
    points
        |> List.map
            (\p ->
                (String.fromFloat (Tuple.first p) ++ ",")
                    ++ (String.fromFloat (Tuple.second p) ++ " ")
            )
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

            -- , [ stroke model.predictedStroke ]
            --, List.map svgPoint model.predictedStroke
            ]
        )



{--
    So.lazy3 Keyed.node
        "paper"
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
        ]
        (List.concat
            [ List.map svgPoint (List.concat model.strokes)
            , List.map svgPoint model.currentStroke
            ]
            |> List.indexedMap (\i p -> ( String.fromInt i, p ))
        )
--}
