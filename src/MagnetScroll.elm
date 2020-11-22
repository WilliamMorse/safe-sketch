port module MagnetScroll exposing (Model, Msg, init, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Ease
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pointer
    exposing
        ( DeviceType
        , Event
        , blockContextMenu
        , eventDecoder
        , onDown
        , onUp
        )
import Sketch
import SmoothScroll
import Svg exposing (Svg)
import Svg.Attributes
import Swiper
import Task
import Zipper exposing (Zipper)


{-| Things to do:
make a threshold for swipe scrolling (either velocity or position or some combo) >> add anemation to pre-scroll threshold
-}
port penMoveEvent : (Encode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Point =
    ( Float, Float )


type alias Sketch =
    List (List Point)


type alias Model =
    { cards : Zipper Sketch
    , cardHeight : Int
    , vp : Dom.Viewport
    , swipeState : Swiper.SwipingState
    , pointer : DeviceType
    , currentStroke : List Point
    , predictedStroke : List Point
    }


blankSketch : Sketch
blankSketch =
    [ [] ]


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { cards = Zipper.fromHeadTail blankSketch [ blankSketch, blankSketch, blankSketch, blankSketch ]
            , cardHeight = 500
            , vp =
                { scene = { width = 100, height = 100 }
                , viewport = { x = 0, y = 0, width = 100, height = 100 }
                }
            , swipeState = Swiper.initialSwipingState
            , pointer = Pointer.Mouse
            , currentStroke = []
            , predictedStroke = []
            }
    in
    update Resize model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onResize (\_ _ -> Resize)
        , penMoveEvent PointerMove
        ]


jumpToTop : Cmd Msg
jumpToTop =
    Task.perform (always NoOp) <|
        Dom.setViewport 0 0


jumpToBottom : Model -> Cmd Msg
jumpToBottom model =
    Task.perform (always NoOp) <|
        Dom.setViewport 0 (model.vp.scene.height + model.vp.viewport.height)


scrollDown : Model -> Cmd Msg
scrollDown model =
    Task.attempt (always NoOp) <|
        SmoothScroll.scrollTo (SmoothScroll.createConfig Ease.outCubic 500)
            (model.vp.scene.height * 2 / 3 - model.vp.viewport.height)


scrollUp : Model -> Cmd Msg
scrollUp model =
    Task.attempt (always NoOp) <|
        SmoothScroll.scrollTo (SmoothScroll.createConfig Ease.outCubic 500)
            (model.vp.scene.height / 3)


scrollToFocusShifted : Model -> Cmd Msg
scrollToFocusShifted model =
    Zipper.index model.cards
        * model.cardHeight
        - (round model.vp.viewport.height - (model.cardHeight + 20))
        |> toFloat
        |> SmoothScroll.scrollTo (SmoothScroll.createConfig Ease.outCubic 500)
        |> Task.attempt (always NoOp)


scrollToFocus : Model -> Cmd Msg
scrollToFocus model =
    Zipper.index model.cards
        * model.cardHeight
        |> toFloat
        |> SmoothScroll.scrollTo (SmoothScroll.createConfig Ease.outCubic 500)
        |> Task.attempt (always NoOp)


type Msg
    = NoOp
    | UpdatedViewport Dom.Viewport
    | Resize
    | Swiped Swiper.SwipeEvent
    | PointerDown Pointer.Event
    | PointerMove Decode.Value
    | PointerUp Pointer.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdatedViewport vp ->
            ( { model | vp = vp }, Cmd.none )

        Resize ->
            ( model, Task.perform UpdatedViewport Dom.getViewport )

        Swiped ev ->
            let
                ( newStateUp, upUpUp ) =
                    Swiper.hasSwipedUp ev model.swipeState

                ( newStateDown, downDownDown ) =
                    Swiper.hasSwipedDown ev model.swipeState

                newModel =
                    if upUpUp then
                        { model
                            | swipeState = newStateDown
                            , cards = Zipper.downOrBottom model.cards
                        }

                    else if downDownDown then
                        { model
                            | swipeState = newStateUp
                            , cards = Zipper.upOrTop model.cards
                        }

                    else
                        { model | swipeState = newStateDown }
            in
            if upUpUp then
                ( newModel, scrollToFocusShifted newModel )

            else if downDownDown then
                ( newModel, scrollToFocus newModel )

            else
                ( newModel, Cmd.none )

        PointerDown event ->
            if event.pointerType == Pointer.Pen then
                ( { model
                    | pointer = event.pointerType
                    , currentStroke =
                        List.append
                            model.currentStroke
                            [ ( event.offsetX, event.offsetY ) ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        PointerMove bundeledEvent ->
            case Decode.decodeValue Sketch.decodeBundle bundeledEvent of
                Ok evb ->
                    case evb.events of
                        event :: restOfEvents ->
                            ( { model
                                | pointer = event.pointerType
                                , currentStroke =
                                    event
                                        :: restOfEvents
                                        |> List.map (\e -> ( e.offsetX, e.offsetY ))
                                        |> List.append model.currentStroke
                                , predictedStroke =
                                    evb.predictions
                                        |> List.map (\e -> ( e.offsetX, e.offsetY ))
                              }
                            , Cmd.none
                            )

                        [] ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PointerUp event ->
            if event.pointerType == Pointer.Pen then
                ( { model
                    -- Now that we have render time, update the view with the denser stroke points
                    | cards =
                        Zipper.updateItem
                            (\sk ->
                                sk
                                    ++ [ model.currentStroke
                                            ++ [ ( event.offsetX, event.offsetY ) ]
                                       ]
                            )
                            model.cards
                    , currentStroke = []
                    , predictedStroke = []
                    , pointer = event.pointerType
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


styledButton : Msg -> String -> Element Msg
styledButton m l =
    el [ padding 10, centerX ] <|
        button
            [ padding 5, Background.color (rgb 0.7 1 0.9) ]
            { onPress = Just m, label = text l }


pointToString : Point -> String
pointToString ( x, y ) =
    String.append
        (String.fromFloat x ++ ",")
        (String.fromFloat y ++ " ")


svgPolylineStringFromPoints : List Point -> String
svgPolylineStringFromPoints points =
    points
        |> List.map pointToString
        |> String.concat


stroke : List Point -> Svg Msg
stroke points =
    Svg.polyline
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        , Svg.Attributes.points (svgPolylineStringFromPoints points)
        ]
        []


canvasCardView : String -> Model -> Element Msg
canvasCardView idStr model =
    Html.canvas
        [ Html.Attributes.width <| round <| model.vp.viewport.width - 44
        , Html.Attributes.height <| round <| (model.vp.viewport.height - 44) * 0.75
        , Html.Attributes.id idStr
        ]
        []
        |> html
        |> el [ Border.width 2, Border.rounded 10 ]


svgCardView : Model -> Sketch -> Element Msg
svgCardView model sketch =
    let
        svgWidth =
            String.fromFloat <| model.vp.scene.width - 20

        svgHeight =
            String.fromInt <| model.cardHeight - 20
    in
    Svg.svg
        [ Svg.Attributes.width svgWidth
        , Svg.Attributes.height svgHeight
        , Svg.Attributes.viewBox <|
            "0 0 "
                ++ svgWidth
                ++ " "
                ++ svgHeight
        , Svg.Attributes.id "sketchspace"
        ]
        (List.concat
            [ List.map stroke sketch
            , [ stroke <| model.currentStroke ++ model.predictedStroke ]
            ]
        )
        |> html
        |> el
            [ width fill
            , height <| px <| model.cardHeight - 20
            , Border.width 2
            , padding 10
            ]


frame : Int
frame =
    20


view : Model -> Html Msg
view model =
    let
        cardSpacing =
            frame
    in
    layout
        ([ padding frame
         , width fill
         , htmlAttribute <| Html.Attributes.style "touch-action" "none"
         , htmlAttribute <| onDown PointerDown
         , htmlAttribute <| onUp PointerUp
         , htmlAttribute <| blockContextMenu NoOp
         ]
            ++ (List.map htmlAttribute <| Swiper.onSwipeEvents Swiped)
        )
    <|
        column [ spacing cardSpacing, width fill ] <|
            (model.cards
                |> Zipper.toList
                |> List.indexedMap (\i _ -> canvasCardView (String.fromInt i) model)
            )
