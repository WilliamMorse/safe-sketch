module Main exposing (Model, Msg(..), Point, init, main, subscriptions, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events as Be
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
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
    String


init : ( Model, Cmd Msg )
init =
    ( "  ", Cmd.none )


type Msg
    = NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [] [ Html.text "hi" ]
