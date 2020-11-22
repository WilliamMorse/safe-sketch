module BubbleGrid exposing (Model, Msg, init, update, view)

import Browser
import Browser.Dom as Dom
import Element exposing (..)
import Element.Background as Background
import Element.Input exposing (button)
import Html exposing (Html)
import SmoothScroll exposing (scrollTo)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }


type alias Point =
    ( Int, Int )



-- perhaps we could make a non flat canvas with teh scroll positions to get there
-- down down right down TREE Display? TREE a List a its also funny to be doing it on a path level. so the tree could be just a path or it could be built up tree wise (its the same datastructure)


type alias Model =
    { centeredTile : Point
    , tiles : List Point
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ( 0, 0 ) [ ( 0, 0 ) ], Cmd.none )


type Msg
    = Down
    | Up
    | Left
    | Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( x, y ) =
            model.centeredTile
    in
    case msg of
        Down ->
            ( { model | centeredTile = ( x, y + 1 ) }, Cmd.none )

        Up ->
            ( { model | centeredTile = ( x, y - 1 ) }, Cmd.none )

        Right ->
            ( { model | centeredTile = ( x + 1, y ) }, Cmd.none )

        Left ->
            ( { model | centeredTile = ( x - 1, y ) }, Cmd.none )


pointToString : Point -> String
pointToString ( x, y ) =
    "( " ++ String.fromInt x ++ ", " ++ String.fromInt y ++ " )"


styledButton : Msg -> String -> Element Msg
styledButton m l =
    el [ padding 10, centerX ] <|
        button
            [ padding 5, Background.color (rgb 0.7 1 0.9) ]
            { onPress = Just m, label = text l }


cardVeiw : Element Msg
cardVeiw =
    el [ width fill, height fill ] none
        |> el [ padding 10, Background.color (rgb 0.5 0.5 0.5), width fill, height fill ]


view : Model -> Html Msg
view model =
    layout [ padding 20, width fill, height fill ] <|
        column [ width fill, height fill ]
            [ text "centered notecard: "
            , text <| pointToString model.centeredTile
            , styledButton Up "Move Up"
            , row [ centerX ]
                [ styledButton Left "Move Left"
                , styledButton Right "Move Right"
                ]
            , styledButton Down "Move Down"
            , cardVeiw
            ]
