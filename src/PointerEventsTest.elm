module PointerEventsTest exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (Column, Element, alignRight, column, el, fill, height, htmlAttribute, padding, spacing, table, text, width)
import Element.Background as Backround
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Json.Decode as D
import Pointer exposing (CompatibilityEvent, DeviceType(..), Event, blockContextMenu, defaultEvent, onDownCompat, onMoveCompat)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Feature a
    = Disabled
    | Decoded a
    | Active a


type alias FeatureEvent =
    { pointerId : Feature Int
    , width : Feature Float
    , height : Feature Float
    , pressure : Feature Float
    , tangentialPressure : Feature Float
    , tiltX : Feature Int
    , tiltY : Feature Int
    , twist : Feature Int
    , altitudeAngle : Feature Float
    , azimuthAngle : Feature Float
    , pointerType : Feature DeviceType
    , isPrimary : Feature Bool
    , offsetX : Feature Float
    , offsetY : Feature Float
    , screenX : Feature Float
    , screenY : Feature Float
    , pageX : Feature Float
    , pageY : Feature Float
    , timeStamp : Feature Float
    }


checkFeature : a -> Maybe a -> Feature a -> Feature a
checkFeature defaultValue decodedValue modelValue =
    case modelValue of
        Active _ ->
            case decodedValue of
                Just b ->
                    Active b

                Nothing ->
                    modelValue

        Decoded _ ->
            case decodedValue of
                Just b ->
                    if b == defaultValue then
                        Decoded b

                    else
                        Active b

                Nothing ->
                    Disabled

        Disabled ->
            case decodedValue of
                Just b ->
                    if b == defaultValue then
                        Decoded b

                    else
                        Active b

                Nothing ->
                    Disabled


detectFeatures : Event -> CompatibilityEvent -> FeatureEvent -> FeatureEvent
detectFeatures ev compEv modelEv =
    { pointerId =
        checkFeature
            ev.pointerId
            compEv.pointerId
            modelEv.pointerId
    , width =
        checkFeature
            ev.width
            compEv.width
            modelEv.width
    , height =
        checkFeature
            ev.height
            compEv.height
            modelEv.height
    , pressure =
        checkFeature
            ev.pressure
            compEv.pressure
            modelEv.pressure
    , tangentialPressure =
        checkFeature
            ev.tangentialPressure
            compEv.tangentialPressure
            modelEv.tangentialPressure
    , tiltX =
        checkFeature
            ev.tiltX
            compEv.tiltX
            modelEv.tiltX
    , tiltY =
        checkFeature
            ev.tiltY
            compEv.tiltY
            modelEv.tiltY
    , twist =
        checkFeature
            ev.twist
            compEv.twist
            modelEv.twist
    , altitudeAngle =
        checkFeature
            ev.altitudeAngle
            compEv.altitudeAngle
            modelEv.altitudeAngle
    , azimuthAngle =
        checkFeature
            ev.azimuthAngle
            compEv.azimuthAngle
            modelEv.azimuthAngle
    , pointerType =
        checkFeature
            ev.pointerType
            compEv.pointerType
            modelEv.pointerType
    , isPrimary =
        checkFeature
            ev.isPrimary
            compEv.isPrimary
            modelEv.isPrimary
    , offsetX =
        checkFeature
            ev.offsetX
            compEv.offsetX
            modelEv.offsetX
    , offsetY =
        checkFeature
            ev.offsetY
            compEv.offsetY
            modelEv.offsetY
    , screenX =
        checkFeature
            ev.screenX
            compEv.screenX
            modelEv.screenX
    , screenY =
        checkFeature
            ev.screenY
            compEv.screenY
            modelEv.screenY
    , pageX =
        checkFeature
            ev.pageX
            compEv.pageX
            modelEv.pageX
    , pageY =
        checkFeature
            ev.pageY
            compEv.pageY
            modelEv.pageY
    , timeStamp =
        checkFeature
            ev.timeStamp
            compEv.timeStamp
            modelEv.timeStamp
    }


type alias Model =
    { pointerEvent : FeatureEvent
    , debugEvent : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        ({ pointerId = Disabled
         , width = Disabled
         , height = Disabled
         , pressure = Disabled
         , tangentialPressure = Disabled
         , tiltX = Disabled
         , tiltY = Disabled
         , twist = Disabled
         , altitudeAngle = Disabled
         , azimuthAngle = Disabled
         , pointerType = Disabled
         , isPrimary = Disabled
         , offsetX = Disabled
         , offsetY = Disabled
         , screenX = Disabled
         , screenY = Disabled
         , pageX = Disabled
         , pageY = Disabled
         , timeStamp = Disabled
         }
            |> detectFeatures defaultEvent (Pointer.compatibilityEventFromEvent defaultEvent)
        )
        ""
    , Cmd.none
    )


type Msg
    = NoOp
    | Pointer CompatibilityEvent


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Pointer ev ->
            ( { model
                | pointerEvent =
                    detectFeatures defaultEvent ev model.pointerEvent
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        ev =
            model.pointerEvent
    in
    Element.layout
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        , htmlAttribute <| onDownCompat Pointer
        , htmlAttribute <| onMoveCompat Pointer
        , htmlAttribute <| blockContextMenu NoOp

        --, htmlAttribute <| onUp Pointer
        ]
        (column
            [ padding 20
            , spacing 20
            , width fill
            , Font.variant Font.tabularNumbers
            , Element.alignRight
            ]
            [ text "Pointer Events v3 Test"

            -- POSITION
            , table
                [ spacing 10
                , Border.width 2
                , padding 10
                ]
                { data = position model.pointerEvent
                , columns =
                    [ Column
                        (text "")
                        fill
                        (\p -> text <| p.label)
                    , Column (text "x")
                        fill
                        (\p -> text <| p.x)
                    , Column (text "y")
                        fill
                        (\p -> text <| p.y)
                    ]
                }

            -- ORENTATION
            , table
                [ Border.width 2
                , padding 10
                , Element.spacingXY 12 0
                ]
                { data = orentation model.pointerEvent
                , columns =
                    [ Column
                        (el [ padding 5 ] <| text "")
                        fill
                        (\p -> el [ padding 5 ] <| text <| p.label)
                    , Column (tableCell ev.tiltX <| text "Tilt x")
                        fill
                        (.tiltX
                            >> String.fromInt
                            >> text
                            >> tableCell ev.tiltX
                        )
                    , Column (tableCell ev.tiltY <| text "Tilt y")
                        fill
                        (\p -> tableCell ev.tiltY <| text <| String.fromInt p.tiltY)
                    , Column (tableCell ev.twist <| text "Twist")
                        fill
                        (\p -> tableCell ev.twist <| text <| String.fromInt p.twist)

                    {--}
                    , Column
                        (tableCell ev.altitudeAngle <|
                            text "Altitude"
                        )
                        fill
                        (.altitudeAngle
                            >> toDegrees
                            >> round
                            >> String.fromInt
                            >> text
                            >> tableCell ev.altitudeAngle
                        )
                    , Column (tableCell ev.azimuthAngle <| text "Azimuth")
                        fill
                        (.azimuthAngle
                            >> toDegrees
                            >> round
                            >> String.fromInt
                            >> text
                            >> tableCell ev.azimuthAngle
                        )
                    ]
                }

            -- PRESSURE
            , table
                [ Element.spacingXY 12 0
                , Border.width 2
                , padding 10
                ]
                { data = pressure model.pointerEvent
                , columns =
                    [ Column
                        (text "")
                        fill
                        (\p -> text <| p.label)
                    , Column
                        (tableCell ev.pressure <| text "Pressure")
                        fill
                        (\p -> tableCell ev.pressure <| text <| p.press)
                    , Column
                        (tableCell ev.tangentialPressure <| text "Tangential Pressure")
                        fill
                        (\p -> tableCell ev.tangentialPressure <| text <| p.tanPress)
                    ]
                }
            , table
                [ Element.spacingXY 12 0
                , Border.width 2
                , padding 10
                ]
                { data = touch model.pointerEvent
                , columns =
                    [ Column
                        (text "")
                        fill
                        (\p -> text <| p.label)
                    , Column
                        (tableCell ev.height <| text "Height")
                        fill
                        (\p -> tableCell ev.height <| text <| p.height)
                    , Column
                        (tableCell ev.width <| text "Width")
                        fill
                        (\p -> tableCell ev.width <| text <| p.width)
                    ]
                }
            , text model.debugEvent
            ]
        )


tableCell : Feature a -> Element msg -> Element msg
tableCell m =
    case m of
        Active _ ->
            el [ padding 5, Backround.color (Element.rgb 0.5 1 0.5) ]

        Decoded _ ->
            el [ padding 5 ]

        Disabled ->
            el [ padding 5, Backround.color (Element.rgb 1 0.65 0.65) ]


maybeFromFeature : Feature a -> Maybe a
maybeFromFeature feature =
    case feature of
        Disabled ->
            Nothing

        Active a ->
            Just a

        Decoded a ->
            Just a


position : FeatureEvent -> List { label : String, x : String, y : String }
position m =
    [ { label = "Screen Position"
      , x = truncateTo 5 <| Maybe.withDefault defaultEvent.screenX <| maybeFromFeature m.screenX
      , y = truncateTo 5 <| Maybe.withDefault defaultEvent.screenY <| maybeFromFeature m.screenY
      }
    , { label = "Page Position"
      , x = truncateTo 5 <| Maybe.withDefault defaultEvent.pageX <| maybeFromFeature m.pageX
      , y = truncateTo 5 <| Maybe.withDefault defaultEvent.pageY <| maybeFromFeature m.pageY
      }
    , { label = "Offset Position"
      , x = truncateTo 5 <| Maybe.withDefault defaultEvent.offsetX <| maybeFromFeature m.offsetX
      , y = truncateTo 5 <| Maybe.withDefault defaultEvent.offsetY <| maybeFromFeature m.offsetY
      }
    ]


orentation :
    FeatureEvent
    ->
        List
            { label : String
            , tiltX : Int
            , tiltY : Int
            , twist : Int
            , altitudeAngle : Float
            , azimuthAngle : Float
            }
orentation m =
    [ { label = "Pen Orentation"
      , tiltX = Maybe.withDefault defaultEvent.tiltX <| maybeFromFeature m.tiltX
      , tiltY = Maybe.withDefault defaultEvent.tiltY <| maybeFromFeature m.tiltY
      , twist = Maybe.withDefault defaultEvent.twist <| maybeFromFeature m.twist
      , altitudeAngle = Maybe.withDefault defaultEvent.altitudeAngle <| maybeFromFeature m.altitudeAngle
      , azimuthAngle = Maybe.withDefault defaultEvent.azimuthAngle <| maybeFromFeature m.azimuthAngle
      }
    ]


pressure : FeatureEvent -> List { label : String, press : String, tanPress : String }
pressure m =
    [ { label = "Pen Force Sensors"
      , press = truncateTo 5 <| Maybe.withDefault defaultEvent.pressure <| maybeFromFeature m.pressure
      , tanPress = truncateTo 5 <| Maybe.withDefault defaultEvent.tangentialPressure <| maybeFromFeature m.tangentialPressure
      }
    ]


touch : FeatureEvent -> List { label : String, height : String, width : String }
touch m =
    [ { label = "Touch"
      , height = truncateTo 5 <| Maybe.withDefault defaultEvent.height <| maybeFromFeature m.height
      , width = truncateTo 5 <| Maybe.withDefault defaultEvent.width <| maybeFromFeature m.width
      }
    ]


truncateTo : Int -> Float -> String
truncateTo places number =
    let
        t =
            number
                |> truncate
                |> String.fromInt
                |> String.length
    in
    number
        |> String.fromFloat
        |> String.left (places + t)


toDegrees : Float -> Float
toDegrees =
    (*) (180 / pi)
