port module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Pointer as Pointer exposing (DeviceType, Event, eventDecoder)


port moveEvent : (Encode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = always ( "low", Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    String


type Msg
    = EventMsg Event
    | DebugMsg Value


subscriptions : Model -> Sub Msg
subscriptions model =
    moveEvent DebugMsg


unpackEvent : Value -> Maybe Event
unpackEvent val =
    case Decode.decodeValue eventDecoder val of
        Ok event ->
            Just event

        Err _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EventMsg event ->
            ( "hi", Cmd.none )

        DebugMsg val ->
            ( Debug.toString (unpackEvent val), Cmd.none )


view : Model -> Html Msg
view model =
    node "hitest"
        []
        [ div []
            [ p
                []
                [ text <| model ]
            ]
        ]
