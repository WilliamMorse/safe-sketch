module Main exposing (Model, Msg(..), Point, init, main, subscriptions, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events as Be
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Pointer exposing (Event, onDown, onMove, onUp)
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
    Maybe Event


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )


type Msg
    = NoOp
    | Pointer Event


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Pointer ev ->
            ( Just ev, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        , htmlAttribute <| onDown Pointer
        , htmlAttribute <| onMove Pointer
        , htmlAttribute <| blockContextMenu NoOp

        --, htmlAttribute <| onUp Pointer
        ]
        (column []
            [ text "Pointer Events v2 Test"
            , paragraph
                []
                [ text (Debug.toString model)
                ]
            ]
        )


blockContextMenu : Msg -> Html.Attribute Msg
blockContextMenu msg =
    Html.Events.preventDefaultOn
        "contextmenu"
        (D.map (\m -> ( m, True )) (D.succeed msg))
