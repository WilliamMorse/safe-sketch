-- DndTest.elm


module DndTest exposing (Model, init, main)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { dragging : Bool, array : Array String }


init : Model
init =
    { dragging = False, array = Array.fromList [ "hi", "there", "one", "two", "three" ] }


type Msg
    = NoOp
    | DragStart
    | DragEnd
    | Drop


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        DragStart ->
            { model | dragging = True }

        DragEnd ->
            { model | dragging = False }

        Drop ->
            { model | dragging = False }


stringFromBool : Bool -> String
stringFromBool b =
    if b then
        "True"

    else
        "False"


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column [ spacing 50 ]
            [ Element.table [ padding 20 ]
                { data = Array.toList model.array
                , columns =
                    [ { header = text "Sort meee:"
                      , width = fill
                      , view =
                            if model.dragging then
                                \s ->
                                    column [ padding 0 ]
                                        [ dropZone
                                        , el
                                            [ htmlAttribute <| Html.Attributes.draggable "True"
                                            , htmlAttribute <| onDragStart DragStart
                                            , htmlAttribute <| onDragEnd DragEnd
                                            ]
                                            (text s)
                                        , el
                                            [ width fill
                                            , height <| px 10
                                            , htmlAttribute <| Html.Attributes.dropzone "True"
                                            ]
                                            none
                                        ]

                            else
                                el
                                    [ padding 10
                                    , htmlAttribute <| Html.Attributes.draggable "True"
                                    , htmlAttribute <| onDragStart DragStart
                                    , htmlAttribute <| onDragEnd DragEnd
                                    ]
                                    << text
                      }
                    ]
                }
            , el [ padding 20 ] <|
                text
                    ("dragging = "
                        ++ stringFromBool model.dragging
                    )
            ]


dropZone : Element Msg
dropZone =
    el
        [ width fill
        , height <| px 10
        , htmlAttribute <| Html.Attributes.dropzone "True"
        , htmlAttribute <| onDrop Drop
        ]
        none


onDragStart : msg -> Html.Attribute msg
onDragStart msg =
    Decode.succeed msg
        |> Html.Events.on "dragstart"


onDragEnd : msg -> Html.Attribute msg
onDragEnd msg =
    Decode.succeed msg
        |> Html.Events.on "dragend"


onDrop : msg -> Html.Attribute msg
onDrop msg =
    Decode.succeed msg
        |> Html.Events.on "ondrop"
