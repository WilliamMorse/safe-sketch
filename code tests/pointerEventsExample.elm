module Main exposing (Event(..), Model, Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


main : Program () Model Msg
main =
    Browser.element
        { init = always ( { e = Nothing }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { e : Maybe Event }


type Event
    = Down Pointer.Event
    | Move Pointer.Event
    | Up Pointer.Event
    | Cancel Pointer.Event


type Msg
    = EventMsg Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EventMsg event ->
            ( { e = Just event }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ p
            [ Pointer.onUp (EventMsg << Up)
            , Pointer.onMove (EventMsg << Move)
            , Pointer.onCancel (EventMsg << Cancel)
            ]
            [ text <| Debug.toString model ]
        ]
