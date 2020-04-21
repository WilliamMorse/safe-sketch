module PointerEventTests exposing (Model, Msg(..), Point, init, main, subscriptions, update, view)

import Browser
import Element exposing (Column, column, fill, height, htmlAttribute, padding, spacing, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import PenTilt as Tilt
import Pointer exposing (DeviceType(..), Event, defaultEvent, onDown, onMove)


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
    Event


init : ( Model, Cmd Msg )
init =
    ( defaultEvent, Cmd.none )


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
            ( ev, Cmd.none )


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
        (column
            [ padding 20
            , spacing 20
            , width fill
            , Font.variant Font.tabularNumbers
            , Element.alignRight
            ]
            [ text "Pointer Events v2 Test"

            -- POSITION
            , table
                [ spacing 10
                , Border.width 2
                , padding 10
                ]
                { data = position model
                , columns =
                    [ Column
                        (text "")
                        fill
                        (\p -> text <| p.label)
                    , Column (text "x")
                        fill
                        (\p -> text <| p.x)
                    , Column (text "y")
                        fill
                        (\p -> text <| p.y)
                    ]
                }

            -- ORENTATION
            , table
                [ spacing 10
                , Border.width 2
                , padding 10
                ]
                { data = orentation model
                , columns =
                    [ Column
                        (text "")
                        fill
                        (\p -> text <| p.label)
                    , Column (text "Tilt x")
                        fill
                        (\p -> text <| String.fromFloat p.tiltX)
                    , Column (text "Tilt y")
                        fill
                        (\p -> text <| String.fromFloat p.tiltY)
                    , Column (text "Twist")
                        fill
                        (\p -> text <| String.fromFloat p.twist)
                    ]
                }

            -- PRESSURE
            , table
                [ spacing 10
                , Border.width 2
                , padding 10
                ]
                { data = pressure model
                , columns =
                    [ Column
                        (text "")
                        fill
                        (\p -> text <| p.label)
                    , Column
                        (text "Pressure")
                        fill
                        (\p -> text <| p.press)
                    , Column
                        (text "Tangential Pressure")
                        fill
                        (\p -> text <| p.tanPress)
                    ]
                }
            , table
                [ spacing 10
                , Border.width 2
                , padding 10
                ]
                { data = touch model
                , columns =
                    [ Column
                        (text "")
                        fill
                        (\p -> text <| p.label)
                    , Column
                        (text "Height")
                        fill
                        (\p -> text <| p.height)
                    , Column
                        (text "Width")
                        fill
                        (\p -> text <| p.width)
                    ]
                }
            ]
        )


blockContextMenu : Msg -> Html.Attribute Msg
blockContextMenu msg =
    Html.Events.preventDefaultOn
        "contextmenu"
        (D.map (\m -> ( m, True )) (D.succeed msg))


position : Model -> List { label : String, x : String, y : String }
position m =
    [ { label = "Screen Position"
      , x = truncateTo 5 m.screenX
      , y = truncateTo 5 m.screenY
      }
    , { label = "Page Position"
      , x = truncateTo 5 m.pageX
      , y = truncateTo 5 m.pageY
      }
    , { label = "Offset Position"
      , x = truncateTo 5 m.offsetX
      , y = truncateTo 5 m.offsetY
      }
    ]


orentation :
    Event
    ->
        List
            { label : String
            , phi : String
            , theta : String
            , tiltX : Float
            , tiltY : Float
            , twist : Float
            }
orentation m =
    let
        sph =
            Tilt.toSpherical (Tilt.Tilt 1 m.tiltX m.tiltY)
    in
    [ { label = "Pen Orentation"
      , tiltX = m.tiltX
      , tiltY = m.tiltY
      , theta = truncateTo 5 sph.theta
      , phi = truncateTo 5 sph.phi
      , twist = m.twist
      }
    ]


pressure : Event -> List { label : String, press : String, tanPress : String }
pressure m =
    [ { label = "Normalized Force"
      , press = truncateTo 5 m.pressure
      , tanPress = truncateTo 5 m.tangentialPressure
      }
    ]


touch : Event -> List { label : String, height : String, width : String }
touch m =
    [ { label = "Touch"
      , height = truncateTo 5 m.height
      , width = truncateTo 5 m.width
      }
    ]


truncateTo : Int -> Float -> String
truncateTo places number =
    let
        t =
            number
                |> truncate
                |> String.fromInt
                |> String.length
    in
    number
        |> String.fromFloat
        |> String.left (places + t)
