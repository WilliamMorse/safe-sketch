module Main exposing (Model, Msg(..), Point, init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Pointer exposing (DeviceType(..), Event, onDown, onMove, onUp)
import Tilt as Tilt


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


defaultEvent : Event
defaultEvent =
    { pointerId = -1
    , width = 0
    , height = 0
    , pressure = 0
    , tangentialpressure = 0
    , tiltX = 0
    , tiltY = 0
    , twist = 0
    , pointerType = Mouse
    , isPrimary = False
    , offsetX = 0
    , offsetY = 0
    , screenX = 0
    , screenY = 0
    , pageX = 0
    , pageY = 0
    }


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
            ]
            [ text "Pointer Events v2 Test"

            -- POSITION
            , table
                [ spacing 5
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
                        (\p -> text <| String.fromFloat p.x)
                    , Column (text "y")
                        fill
                        (\p -> text <| String.fromFloat p.y)
                    ]
                }

            -- ORENTATION
            , table
                [ spacing 5
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
                [ spacing 5
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
                        (\p -> text <| String.fromFloat p.press)
                    , Column
                        (text "Tangential Pressure")
                        fill
                        (\p -> text <| String.fromFloat p.tanpress)
                    ]
                }
            , table
                [ spacing 5
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
                        (\p -> text <| String.fromFloat p.height)
                    , Column
                        (text "Width")
                        fill
                        (\p -> text <| String.fromFloat p.width)
                    ]
                }
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


position : Model -> List { label : String, x : Float, y : Float }
position m =
    [ { label = "Screen Position"
      , x = m.screenX
      , y = m.screenY
      }
    , { label = "Page Position"
      , x = m.pageX
      , y = m.pageY
      }
    , { label = "Offset Position"
      , x = m.offsetX
      , y = m.offsetY
      }
    ]


orentation m =
    let
        sph =
            Tilt.toSpherical (Tilt.Tilt 1 m.tiltX m.tiltY)
    in
    [ { label = "Pen Orentation"
      , tiltX = m.tiltX
      , tiltY = m.tiltY
      , theta = sph.theta
      , phi = sph.phi
      , twist = m.twist
      }
    ]


pressure m =
    [ { label = "Normalized Force"
      , press = m.pressure
      , tanpress = m.tangentialpressure
      }
    ]


touch m =
    [ { label = "Touch"
      , height = m.height
      , width = m.width
      }
    ]
