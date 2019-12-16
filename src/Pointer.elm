module Pointer exposing (Event, onDown, onMove, onUp)

import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder, field)


type Pointer
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
    , pointerType : Pointer
    , isPrimary : Bool
    }


inputTypeFromString : String -> Pointer
inputTypeFromString str =
    case str of
        "touch" ->
            Touch

        "pen" ->
            Pen

        _ ->
            Mouse


{--}
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
