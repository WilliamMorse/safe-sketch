module Pointer exposing (DeviceType(..), Event, blockContextMenu, defaultEvent, eventDecoder, onDown, onMove, onUp)

import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder, field)


type DeviceType
    = Mouse
    | Touch
    | Pen


type alias Event =
    { pointerId : Float
    , width : Float
    , height : Float
    , pressure : Float
    , tangentialPressure : Float
    , tiltX : Float
    , tiltY : Float
    , twist : Float
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


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> andMap (field "pointerId" Decode.float)
        |> andMap (field "width" Decode.float)
        |> andMap (field "height" Decode.float)
        |> andMap (field "pressure" Decode.float)
        |> andMap (field "tangentialPressure" Decode.float)
        |> andMap (field "tiltX" Decode.float)
        |> andMap (field "tiltY" Decode.float)
        |> andMap (field "twist" Decode.float)
        |> andMap
            (field "pointerType"
                (Decode.map inputTypeFromString
                    Decode.string
                )
            )
        |> andMap (field "isPrimary" Decode.bool)
        |> andMap (field "offsetX" Decode.float)
        |> andMap (field "offsetY" Decode.float)
        |> andMap (field "screenX" Decode.float)
        |> andMap (field "screenY" Decode.float)
        |> andMap (field "pageX" Decode.float)
        |> andMap (field "pageY" Decode.float)
        |> andMap (field "timeStamp" Decode.float)
--}


defaultEvent : Event
defaultEvent =
    { pointerId = -1
    , width = 0
    , height = 0
    , pressure = 0
    , tangentialPressure = 0
    , tiltX = 0
    , tiltY = 0
    , twist = 0
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
            (field "pointerId" Decode.float)
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
            (field "tiltX" Decode.float)
        >> andUpdate
            (\e a -> { e | tiltY = a })
            (field "tiltY" Decode.float)
        >> andUpdate
            (\e a -> { e | twist = a })
            (field "twist" Decode.float)
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
    eventDecoderWithDefault defaultEvent
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
