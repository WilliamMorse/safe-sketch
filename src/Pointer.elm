module Pointer exposing (Event, on, onDown, onMove, onUp)

import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


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


type Pointer
    = Mouse
    | Touch
    | Pen


inputTypeDecoderFromString : String -> Pointer
inputTypeDecoderFromString str =
    case str of
        "touch" ->
            Touch

        "pen" ->
            Pen

        _ ->
            Mouse


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "pointerId" Decode.float
        |> required "width" Decode.float
        |> required "height" Decode.float
        |> required "pressure" Decode.float
        |> required "tangentialPressure" Decode.float
        |> required "tiltX" Decode.float
        |> required "tiltY" Decode.float
        |> required "twist" Decode.float
        |> required "pointerType" (Decode.map inputTypeDecoderFromString Decode.string)
        |> required "isPrimary" Decode.bool


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



{--
type alias Event =
    { primary : PointerInfo
    , coalesced : List PointerInfo
    }
--}
{--
coalesedEventDecoder : Decoder Event
coalesedEventDecoder =
    Decode.map2 Event
        pointerInfoDecoder
        (Decode.field "coalescedEvents" (Decode.list pointerInfoDecoder))
--}
