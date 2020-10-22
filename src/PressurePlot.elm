port module PressurePlot exposing (Model, Msg(..), Point, blockContextMenu, decodeBundle, init, main, penMoveEvent, pressureChart, subscriptions, update, velocity, view, xyChart)

import Browser
import Element
    exposing
        ( Element
        , column
        , fill
        , height
        , html
        , htmlAttribute
        , none
        , padding
        , row
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots
import PenTilt
import Pointer
    exposing
        ( DeviceType(..)
        , Event
        , eventDecoder
        , onDown
        , onMove
        )
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)


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


type alias Model =
    { events : List Event
    , predictions : List Event
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] [], Cmd.none )


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
                    ( Model (model.events ++ bundle.events) bundle.predictions
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        , htmlAttribute <| onDown Down --Move is done with the port
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
            , column [ width fill ]
                [ pressureOrentationChart model
                , xyChart model
                , newMeathodChart model.events
                ]
            ]
        )


blockContextMenu : Msg -> Html.Attribute Msg
blockContextMenu msg =
    Html.Events.preventDefaultOn
        "contextmenu"
        (Decode.map (\m -> ( m, True )) (Decode.succeed msg))



--------------- new code -------------


pressureVector3d : Event -> Vector3
pressureVector3d { pressure, altitudeAngle, azimuthAngle } =
    { r = pressure
    , theta = (pi / 2) - altitudeAngle
    , phi = azimuthAngle
    }
        |> PenTilt.spherical_to_cartesian


{-| gives back a list of truncated events because the velocity sampling happens between datapoints.
The coalesced events come in reagularly (more so than the timestamps that they are labled with) so the base unit of time is the time between move events.
-}
velocity2d : List Event -> ( List Vector2, List Event )
velocity2d events =
    let
        p1 =
            List.map (\e -> { x = e.screenX, y = e.screenY }) events

        p2 =
            List.drop 1 p1

        p3 =
            List.drop 2 p1

        vp2 =
            List.map3
                (\a b c ->
                    Vector2.add (Vector2.rel a b) (Vector2.rel b c)
                        |> Vector2.scale (1 / 2)
                )
                p1
                p2
                p3

        curtaledEvents =
            List.drop 1 events |> List.reverse |> List.drop 1 |> List.reverse
    in
    ( vp2, curtaledEvents )


frictionCorrectedPressure : Float -> Vector2 -> Vector3 -> Vector3
frictionCorrectedPressure mu v uncorrectedPressure =
    let
        -- plane representing all possible net pressure vectors from the pressure sensor
        pressurePlane =
            Vector3.Plane uncorrectedPressure uncorrectedPressure

        -- line that is in the direction specified by the coefecent of friction cone and the velocity vector
        netForceLine =
            Vector3.Line Vector3.zero
                (PenTilt.spherical_to_cartesian
                    (PenTilt.Spherical 1 (atan mu) (Vector2.angleBetween (Vector2 1 0) v))
                )
    in
    Maybe.withDefault Vector3.zero
        (Vector3.pointFromLinePlaneIntersection netForceLine pressurePlane)


newMeathodChart : List Event -> Element msg
newMeathodChart events =
    let
        ( vel, trimedEvents ) =
            velocity2d events

        uncorrPressures =
            List.map pressureVector3d trimedEvents

        corrPressures =
            List.map2 (frictionCorrectedPressure 0.05) vel uncorrPressures

        normalForce =
            corrPressures |> List.map .z
    in
    html <|
        LineChart.view
            .x
            .y
            [ LineChart.line Colors.blue
                Dots.none
                "z Press"
                (floatIndexedMap Point normalForce)
            ]



--------------- new code above -------


rawPressure : Float -> Int -> Int -> Vector3
rawPressure corrPressure tiltX tiltY =
    let
        pressureVector =
            PenTilt.toCartesian
                (PenTilt.Tilt corrPressure
                    (degrees <| toFloat tiltX)
                    (degrees <| toFloat tiltY)
                )
    in
    pressureVector
        |> (cos << .theta << PenTilt.cartesian_to_spherical)
        |> (\sf -> Vector3.scale sf pressureVector)


assumeingMuChart : Model -> Element msg
assumeingMuChart model =
    let
        pressureVector =
            \ev -> rawPressure ev.pressure ev.tiltX ev.tiltY

        netForcePlanes =
            List.map
                (\ev ->
                    Vector3.Plane (pressureVector ev)
                        (Vector3.direction (pressureVector ev))
                )
                model.events

        vels =
            List.map (\v -> Vector3 v.x v.y 0) (velocity model.events)

        frictionPlanes =
            List.map
                (\v ->
                    Vector3.Plane
                        Vector3.zero
                        (Vector3.cross
                            v
                            Vector3.k
                        )
                )
                vels

        frictionTheta =
            atan 0.0000000001

        muPlanes =
            List.map
                (\fp ->
                    { point = Vector3.zero
                    , normal = Vector3.rotateAngleAxis frictionTheta fp.normal Vector3.k
                    }
                )
                frictionPlanes

        netForces =
            List.map5
                (\a b c v p ->
                    if Vector3.lengthSquared v > 1 then
                        Vector3.pointFromThreePlanes a b c
                            |> Maybe.withDefault Vector3.zero
                            |> .z

                    else
                        p.z
                )
                netForcePlanes
                frictionPlanes
                muPlanes
                vels
                (List.map pressureVector model.events)
    in
    html <|
        LineChart.view
            .x
            .y
            [ LineChart.line Colors.blue
                Dots.none
                "net-force?"
                (List.indexedMap (\i -> Point (toFloat i)) netForces)
            ]


pressureChart : Model -> Element msg
pressureChart model =
    html <|
        LineChart.view1
            .x
            .y
            (List.indexedMap
                (\x y -> Point (toFloat x) y.pressure)
                model.events
            )


velocityChart : Model -> Element msg
velocityChart model =
    html <|
        LineChart.view
            .x
            .y
            [ LineChart.line Colors.blue
                Dots.none
                "vel"
                (List.indexedMap
                    (\x y -> Point (toFloat x) y)
                    (List.map Vector2.length (velocity model.events))
                )
            ]


xyChart : Model -> Element msg
xyChart model =
    html <|
        LineChart.view2
            .x
            .y
            (List.indexedMap (\x y -> Point (toFloat x) y.pageX) model.events)
            (List.indexedMap (\x y -> Point (toFloat x) y.pageY) model.events)


pressureCorrWithDirection : Model -> Element Msg
pressureCorrWithDirection model =
    let
        pres =
            List.map .pressure model.events

        penVector ev =
            { r = 1
            , tiltX = ev.tiltX |> toFloat
            , tiltY = ev.tiltY |> toFloat
            }
                |> PenTilt.toCartesian

        penDirection3d ev =
            Vector3.direction <| penVector ev

        penDirection2d =
            List.map
                (\ev ->
                    { r = 1
                    , tiltX = ev.tiltX |> toFloat
                    , tiltY = ev.tiltY |> toFloat
                    }
                        |> PenTilt.toCartesian
                        |> (\o -> Vector2.direction { x = o.x, y = o.y })
                )
                model.events

        velDirection =
            List.map Vector2.direction <| velocity model.events

        dotPminus =
            List.map2
                (\a b ->
                    0.5
                        + (-1
                            * Vector2.dot a b
                          )
                )
                penDirection2d
                velDirection
    in
    html <|
        LineChart.view
            .x
            .y
            [ LineChart.line Colors.blue
                Dots.circle
                "angle"
                (List.map2 Point dotPminus pres)
            ]


pressureOrentationChart : Model -> Element msg
pressureOrentationChart model =
    let
        pressureVector =
            List.map (\ev -> pressureVectorFromTilt ev.pressure ev.tiltX ev.tiltY) model.events
                |> List.drop 1

        pressure =
            List.map .pressure model.events
                |> List.drop 1

        dotProduct =
            List.map2 dotVelPressure (velocity model.events) pressureVector

        angle =
            List.map2 (\pv3 v -> (\a -> a * 180 / pi) <| Vector2.angleBetween (Vector2 pv3.x pv3.y) v)
                pressureVector
                (velocity model.events)
    in
    html <|
        LineChart.view
            .x
            .y
            [ LineChart.line Colors.blue
                Dots.none
                "corr"
                (List.map2 Point angle pressure)
            ]


floatIndexedMap : (Float -> a -> b) -> List a -> List b
floatIndexedMap f li =
    List.indexedMap (\i a -> f (toFloat i) a) li


cosThetaChart : Model -> Element msg
cosThetaChart model =
    let
        cosPress =
            List.map
                (\ev ->
                    (*) -1.0 <|
                        .z <|
                            rawPressure ev.pressure ev.tiltX ev.tiltY
                )
                model.events

        pressure =
            List.map .pressure model.events

        cosRemovedPressure =
            List.map (\ev -> Vector3.length (rawPressure ev.pressure ev.tiltX ev.tiltY)) model.events

        pressureVector =
            List.map
                (\ev ->
                    pressureVectorFromTilt ev.pressure
                        ev.tiltX
                        ev.tiltY
                )
                model.events

        dotProduct =
            List.map2 dotVelPressure (velocity model.events) pressureVector
    in
    html <|
        LineChart.view
            .x
            .y
            [ LineChart.line Colors.blue
                Dots.none
                "z Press"
                (floatIndexedMap Point cosPress)
            , LineChart.line Colors.purple
                Dots.none
                "Raw Press"
                (floatIndexedMap Point pressure)
            , LineChart.line Colors.green
                Dots.none
                "dot"
                (floatIndexedMap Point dotProduct)
            , LineChart.line Colors.red
                Dots.none
                "cosRmPress"
                (floatIndexedMap Point cosRemovedPressure)
            ]


velocity : List Event -> List Vector2
velocity eventList =
    let
        timeStamps =
            List.map .timeStamp eventList

        t1 =
            Maybe.withDefault 0 (List.head timeStamps)

        t2 =
            Maybe.withDefault 0 <|
                List.head <|
                    List.reverse timeStamps

        p1 =
            List.map (\e -> { x = e.screenX, y = e.screenY }) eventList

        p2 =
            List.drop 1 p1

        displacment =
            List.map2 Vector2.rel p1 p2

        dt =
            (t2 - t1) / (toFloat <| List.length eventList)
    in
    List.map (\disp -> Vector2.scale (1 / dt) disp) displacment


pressureVectorFromTilt : Float -> Int -> Int -> Vector3
pressureVectorFromTilt pressure tiltX tiltY =
    PenTilt.toCartesian (PenTilt.Tilt pressure (degrees <| toFloat tiltX) (degrees <| toFloat tiltY))


dotVelPressure : Vector2 -> Vector3 -> Float
dotVelPressure vel press =
    let
        vel3 =
            Vector3 vel.x vel.y 0
    in
    Vector3.dot (Vector3.direction vel3) (Vector3.direction press)
