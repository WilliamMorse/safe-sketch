module Main exposing (Model, Msg(..), alwaysPreventDefault, init, main, onDown, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    Bool


init : ( Model, Cmd Msg )
init =
    ( False, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = Toggle Bool
    | Down
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Toggle b ->
            ( model, Cmd.none )

        Down ->
            ( not model, Cmd.none )



-- here we're making two calls to the elm runtime


onDown : msg -> Html.Attribute msg
onDown msg =
    Html.Events.preventDefaultOn "contextmenu" (D.map alwaysPreventDefault (D.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


view model =
    layout []
        (Input.checkbox
            [ padding 20
            , htmlAttribute <| onDown Down
            ]
            { onChange = Toggle
            , icon = Input.defaultCheckbox
            , checked = model
            , label =
                Input.labelRight
                    []
                    (text "Checkbox")
            }
        )
