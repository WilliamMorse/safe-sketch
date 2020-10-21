module Friction exposing (..)

import Browser
import Html exposing (Html)
import PenTilt
import Pointer exposing (Event)
import Vector2 exposing (Vector2)
import Vector3 exposing (Vector3)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { tilts : List ( Int, Int )
    , pressure : List Float
    , tangentialPressure : List Float
    }


init : Model
init =
    { tilts = []
    , pressure = []
    , tangentialPressure = []
    }


type Msg
    = NoOp
    | Pointer Event


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Pointer ev ->
            let
                tilts =
                    ( ev.tiltX, ev.tiltY ) :: model.tilts

                pressure =
                    ev.pressure :: model.pressure

                tangentialPressure =
                    ev.tangentialPressure :: model.tangentialPressure
            in
            { model | tilts = tilts, pressure = pressure, tangentialPressure = tangentialPressure }


view : Model -> Html Msg
view model =
    Html.text "hi"


tiltToVector : ( Int, Int ) -> Vector3
tiltToVector ( tiltX, tiltY ) =
    PenTilt.toCartesian (PenTilt.Tilt 1.0 (toFloat tiltX) (toFloat tiltY))


netScreenForce : Vector3 -> Float -> Vector2 -> Vector3
netScreenForce pressure tangentalPressure screenVelocity =
    let
        screen3d =
            Vector3 screenVelocity.x screenVelocity.y 0

        frictionPlane =
            Vector3.cross screen3d Vector3.k

        lastLegDir =
            Vector3.cross frictionPlane pressure

        projectionTemplate =
            Vector3.cross lastLegDir frictionPlane

        inverseProjectionOfPressure =
            Vector3.inverseProject pressure projectionTemplate

        closestApproach =
            Vector3.rel pressure inverseProjectionOfPressure

        lastLegMag =
            sqrt (tangentalPressure ^ 2 - Vector3.lengthSquared closestApproach)

        lastLeg =
            Vector3.scale lastLegMag (Vector3.direction lastLegDir)

        check =
            Vector3.dot lastLeg screen3d

        netForce =
            if check <= 0 then
                Vector3.add inverseProjectionOfPressure lastLeg

            else
                Vector3.subtract inverseProjectionOfPressure lastLeg
    in
    netForce
