module Pointer exposing (DeviceType(..), Event, blockContextMenu, eventDecoder, onDown, onMove, onUp)

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
    , tangentialpressure : Float
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
    , timestamp : Float
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
