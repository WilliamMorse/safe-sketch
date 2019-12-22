module Main exposing (Model, Msg(..), Point, blockContextMenu, init, main, svgCanvas, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events as Be
import Element exposing (..)
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Pointer exposing (DeviceType, onDown, onMove, onUp)
import Svg as S
import Svg.Attributes as Sa
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
    { inputType : DeviceType
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
    , controlPoints : List (List Point)
    }


init : ( Model, Cmd Msg )
init =
    ( { inputType = Pointer.Pen
      , tiltX = 0.0
      , tiltY = 0.0
      , pressure = 0
      , latestEvent = Nothing
      , pointerDown = False
      , offsetPos = ( 0, 0 )
      , lastStroke = []

      -- hadcoded some starting strokes to test the smoothing
      , strokes = [ [ ( 100, 100 ), ( 500, 500 ), ( 100, 600 ), ( 200, 600 ) ] ]
      , viewportHeight = 10
      , viewportWidth = 10
      , controlPoints = [ [] ]
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
            if event.pointerType == Pointer.Pen then
                ( { model
                    | inputType = event.pointerType
                    , tiltX = event.tiltX
                    , tiltY = event.tiltY
                    , pressure = event.pressure
                    , latestEvent = Just event
                    , offsetPos = ( event.offsetX, event.offsetY )
                    , lastStroke = List.append model.lastStroke [ ( event.offsetX, event.offsetY ) ]
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
        (D.map (\m -> ( m, True )) (D.succeed msg))


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
        , Sa.stroke "grey"
        , Sa.strokeWidth "1"
        , Sa.points (svgPolylineStringFromPoints points)
        ]
        []


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
                [ --List.map svgPoint (List.concat model.strokes)
                  [ cubicSpline model.lastStroke ]
                , List.map cubicSpline model.strokes

                {--
                , [ smoothStroke model.lastStroke ]
                , List.map smoothStroke model.strokes
                --}
                ]
            )
