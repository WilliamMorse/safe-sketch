module RecFrame exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Html exposing (Html)
import Html.Attributes exposing (style)
import Pointer
import Svg as S exposing (Svg)
import Svg.Attributes as Sa
import Svg.Lazy
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Point =
    ( Float, Float )


type alias Model =
    { frameDelta : Float
    , eventBuffer : List Pointer.Event
    , strokes : List (List Point)
    , vp : Dom.Viewport
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { frameDelta = 100 / 6
      , eventBuffer = []
      , strokes = []
      , vp =
            { scene = { width = 100, height = 100 }
            , viewport = { x = 0, y = 0, width = 100, height = 100 }
            }
      }
    , Task.perform UpdatedViewport Dom.getViewport
    )


type Msg
    = AnimationFrame Float
    | UpdatedViewport Dom.Viewport
    | Resize
    | PointerDown Pointer.Event
    | PointerMove Pointer.Event
    | PointerUp Pointer.Event


evOffset : Pointer.Event -> Point
evOffset { offsetX, offsetY } =
    ( offsetX, offsetY )


flushEventBuffer : Model -> Model
flushEventBuffer model =
    { model
        | strokes =
            List.map evOffset model.eventBuffer :: model.strokes
        , eventBuffer = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame td ->
            ( { model | frameDelta = td } |> flushEventBuffer, Cmd.none )

        Resize ->
            ( model, Task.perform UpdatedViewport Dom.getViewport )

        UpdatedViewport vp ->
            ( { model | vp = vp }, Cmd.none )

        PointerDown ev ->
            ( { model | strokes = [ evOffset ev ] :: model.strokes }, Cmd.none )

        PointerMove ev ->
            ( { model
                | strokes =
                    case model.strokes of
                        [] ->
                            [ [ evOffset ev ] ]

                        h :: t ->
                            (evOffset ev :: h) :: t
              }
            , Cmd.none
            )

        PointerUp ev ->
            ( { model | eventBuffer = ev :: model.eventBuffer }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta AnimationFrame
        , Events.onResize (\_ _ -> Resize)
        ]


pointToString : Point -> String
pointToString ( x, y ) =
    (String.fromFloat x |> String.cons ' ')
        ++ (String.fromFloat y |> String.cons ',')


svgPolylineStringFromPoints : List Point -> String
svgPolylineStringFromPoints points =
    points
        |> List.map pointToString
        |> String.concat


stroke : List Point -> S.Svg Msg
stroke points =
    S.polyline
        [ Sa.fill "none"
        , Sa.stroke "black"
        , Sa.strokeWidth "2"
        , Sa.strokeLinecap "round"
        , Sa.strokeLinejoin "round"
        , Sa.points (svgPolylineStringFromPoints points)
        ]
        []


svgSketch : Model -> Html Msg
svgSketch model =
    let
        width =
            String.fromFloat <| model.vp.viewport.width - 30

        height =
            String.fromFloat <| model.vp.viewport.height - 30
    in
    S.svg
        [ Sa.width width
        , Sa.height height
        , Sa.viewBox <| "" ++ "0" ++ " " ++ "0" ++ " " ++ width ++ " " ++ height
        , Sa.id "sketchspace"
        ]
    <|
        List.map (Svg.Lazy.lazy stroke) model.strokes


view : Model -> Html Msg
view model =
    Html.div
        [ Pointer.onDown PointerDown
        , Pointer.onMove PointerMove
        , Pointer.onUp PointerUp
        , style "touch-action" "none"
        ]
        [ svgSketch model ]
