module Pointer exposing (CompatibilityEvent, DeviceType(..), Event, blockContextMenu, compatibilityEventFromEvent, defaultEvent, eventDecoder, eventDecoderWithDefault, onDown, onDownCompat, onMove, onMoveCompat, onUp, onUpCompat)

import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Extra exposing (optionalField, withDefault)


type DeviceType
    = Mouse
    | Touch
    | Pen


type alias Event =
    { pointerId : Int
    , width : Float
    , height : Float
    , pressure : Float
    , tangentialPressure : Float
    , tiltX : Int
    , tiltY : Int
    , twist : Int
    , altitudeAngle : Float
    , azimuthAngle : Float
    , pointerType : DeviceType
    , isPrimary : Bool
    , offsetX : Float
    , offsetY : Float
    , screenX : Float
    , screenY : Float
    , pageX : Float
    , pageY : Float
    , timeStamp : Float
    }


inputTypeFromString : String -> DeviceType
inputTypeFromString str =
    case str of
        "touch" ->
            Touch

        "pen" ->
            Pen

        _ ->
            Mouse


{--}
andMap : Decoder a -> Decoder (a -> value) -> Decoder value
andMap =
    Decode.map2 (|>)


andWithDefault : a -> Decoder a -> Decoder (a -> value) -> Decoder value
andWithDefault a decoder =
    andMap (withDefault a decoder)


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> andWithDefault defaultEvent.pointerId (field "pointerId" Decode.int)
        |> andWithDefault defaultEvent.width (field "width" Decode.float)
        |> andWithDefault defaultEvent.height (field "height" Decode.float)
        |> andWithDefault defaultEvent.pressure (field "pressure" Decode.float)
        |> andWithDefault defaultEvent.tangentialPressure (field "tangentialPressure" Decode.float)
        |> andWithDefault defaultEvent.tiltX (field "tiltX" Decode.int)
        |> andWithDefault defaultEvent.tiltY (field "tiltY" Decode.int)
        |> andWithDefault defaultEvent.twist (field "twist" Decode.int)
        |> andWithDefault defaultEvent.altitudeAngle (field "altitudeAngle" Decode.float)
        |> andWithDefault defaultEvent.azimuthAngle (field "azimuthAngle" Decode.float)
        |> andWithDefault defaultEvent.pointerType
            (field "pointerType"
                (Decode.map inputTypeFromString
                    Decode.string
                )
            )
        |> andWithDefault defaultEvent.isPrimary (field "isPrimary" Decode.bool)
        |> andWithDefault defaultEvent.offsetX (field "offsetX" Decode.float)
        |> andWithDefault defaultEvent.offsetY (field "offsetY" Decode.float)
        |> andWithDefault defaultEvent.screenX (field "screenX" Decode.float)
        |> andWithDefault defaultEvent.screenY (field "screenY" Decode.float)
        |> andWithDefault defaultEvent.pageX (field "pageX" Decode.float)
        |> andWithDefault defaultEvent.pageY (field "pageY" Decode.float)
        |> andWithDefault defaultEvent.timeStamp (field "timeStamp" Decode.float)
--}


defaultEvent : Event
defaultEvent =
    { pointerId = 0
    , width = 1
    , height = 1
    , pressure = 0
    , tangentialPressure = 0
    , tiltX = 0
    , tiltY = 0
    , twist = 0
    , altitudeAngle = pi / 2
    , azimuthAngle = 0
    , pointerType = Mouse
    , isPrimary = False
    , offsetX = 0
    , offsetY = 0
    , screenX = 0
    , screenY = 0
    , pageX = 0
    , pageY = 0
    , timeStamp = 0
    }


{--}
updateRecordWithDecoder : (value -> a -> value) -> Decoder a -> value -> Decoder value
updateRecordWithDecoder setter decoder valueToUpdate =
    Decode.map (setter valueToUpdate) decoder


andUpdate : (value -> a -> value) -> Decoder a -> Decoder value -> Decoder value
andUpdate setter decoder =
    Decode.andThen <| updateRecordWithDecoder setter decoder


eventDecoderWithDefault : Event -> Decoder Event
eventDecoderWithDefault =
    Decode.succeed
        >> andUpdate
            (\e a -> { e | pointerId = a })
            (field "pointerId" Decode.int)
        >> andUpdate
            (\e a -> { e | width = a })
            (field "width" Decode.float)
        >> andUpdate
            (\e a -> { e | height = a })
            (field "height" Decode.float)
        >> andUpdate
            (\e a -> { e | pressure = a })
            (field "pressure" Decode.float)
        >> andUpdate
            (\e a -> { e | tangentialPressure = a })
            (field "tangentialPressure" Decode.float)
        >> andUpdate
            (\e a -> { e | tiltX = a })
            (field "tiltX" Decode.int)
        >> andUpdate
            (\e a -> { e | tiltY = a })
            (field "tiltY" Decode.int)
        >> andUpdate
            (\e a -> { e | twist = a })
            (field "twist" Decode.int)
        >> andUpdate
            (\e a -> { e | altitudeAngle = a })
            (field "altitudeAngle" Decode.float)
        >> andUpdate
            (\e a -> { e | azimuthAngle = a })
            (field "azimuthAngle" Decode.float)
        >> andUpdate
            (\e a -> { e | pointerType = a })
            (field "pointerType"
                (Decode.map inputTypeFromString Decode.string)
            )
        >> andUpdate
            (\e a -> { e | isPrimary = a })
            (field "isPrimary" Decode.bool)
        >> andUpdate
            (\e a -> { e | offsetX = a })
            (field "offsetX" Decode.float)
        >> andUpdate
            (\e a -> { e | offsetY = a })
            (field "offsetY" Decode.float)
        >> andUpdate
            (\e a -> { e | screenX = a })
            (field "screenX" Decode.float)
        >> andUpdate
            (\e a -> { e | screenY = a })
            (field "screenY" Decode.float)
        >> andUpdate
            (\e a -> { e | pageX = a })
            (field "pageX" Decode.float)
        >> andUpdate
            (\e a -> { e | pageY = a })
            (field "pageY" Decode.float)
        >> andUpdate
            (\e a -> { e | timeStamp = a })
            (field "timeStamp" Decode.float)
--}


on : String -> (Event -> msg) -> Html.Attribute msg
on event tag =
    eventDecoder
        |> Decode.map tag
        |> Html.Events.on event


onDown : (Event -> msg) -> Html.Attribute msg
onDown =
    on "pointerdown"


onMove : (Event -> msg) -> Html.Attribute msg
onMove =
    on "pointermove"


onUp : (Event -> msg) -> Html.Attribute msg
onUp =
    on "pointerup"


blockContextMenu : msg -> Html.Attribute msg
blockContextMenu msg =
    Html.Events.preventDefaultOn
        "contextmenu"
        (Decode.map (\m -> ( m, True )) (Decode.succeed msg))


type alias CompatibilityEvent =
    { pointerId : Maybe Int
    , width : Maybe Float
    , height : Maybe Float
    , pressure : Maybe Float
    , tangentialPressure : Maybe Float
    , tiltX : Maybe Int
    , tiltY : Maybe Int
    , twist : Maybe Int
    , altitudeAngle : Maybe Float
    , azimuthAngle : Maybe Float
    , pointerType : Maybe DeviceType
    , isPrimary : Maybe Bool
    , offsetX : Maybe Float
    , offsetY : Maybe Float
    , screenX : Maybe Float
    , screenY : Maybe Float
    , pageX : Maybe Float
    , pageY : Maybe Float
    , timeStamp : Maybe Float
    }


compatibilityEventDecoder : Decoder CompatibilityEvent
compatibilityEventDecoder =
    Decode.succeed CompatibilityEvent
        |> andMap (optionalField "pointerId" Decode.int)
        |> andMap (optionalField "width" Decode.float)
        |> andMap (optionalField "height" Decode.float)
        |> andMap (optionalField "pressure" Decode.float)
        |> andMap (optionalField "tangentialPressure" Decode.float)
        |> andMap (optionalField "tiltX" Decode.int)
        |> andMap (optionalField "tiltY" Decode.int)
        |> andMap (optionalField "twist" Decode.int)
        |> andMap (optionalField "altitudeAngle" Decode.float)
        |> andMap (optionalField "azimuthAngle" Decode.float)
        |> andMap
            (optionalField "pointerType"
                (Decode.map inputTypeFromString
                    Decode.string
                )
            )
        |> andMap (optionalField "isPrimary" Decode.bool)
        |> andMap (optionalField "offsetX" Decode.float)
        |> andMap (optionalField "offsetY" Decode.float)
        |> andMap (optionalField "screenX" Decode.float)
        |> andMap (optionalField "screenY" Decode.float)
        |> andMap (optionalField "pageX" Decode.float)
        |> andMap (optionalField "pageY" Decode.float)
        |> andMap (optionalField "timeStamp" Decode.float)


compatibilityEventFromEvent : Event -> CompatibilityEvent
compatibilityEventFromEvent { pointerId, width, height, pressure, tangentialPressure, tiltX, tiltY, twist, altitudeAngle, azimuthAngle, pointerType, isPrimary, offsetX, offsetY, screenX, screenY, pageX, pageY, timeStamp } =
    CompatibilityEvent
        (Just pointerId)
        (Just width)
        (Just height)
        (Just pressure)
        (Just tangentialPressure)
        (Just tiltX)
        (Just tiltY)
        (Just twist)
        (Just altitudeAngle)
        (Just azimuthAngle)
        (Just pointerType)
        (Just isPrimary)
        (Just offsetX)
        (Just offsetY)
        (Just screenX)
        (Just screenY)
        (Just pageX)
        (Just pageY)
        (Just timeStamp)


onCompat : String -> (CompatibilityEvent -> msg) -> Html.Attribute msg
onCompat event tag =
    compatibilityEventDecoder
        |> Decode.map tag
        |> Html.Events.on event


onDownCompat : (CompatibilityEvent -> msg) -> Html.Attribute msg
onDownCompat =
    onCompat "pointerdown"


onMoveCompat : (CompatibilityEvent -> msg) -> Html.Attribute msg
onMoveCompat =
    onCompat "pointermove"


onUpCompat : (CompatibilityEvent -> msg) -> Html.Attribute msg
onUpCompat =
    onCompat "pointerup"
