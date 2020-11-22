module Spline exposing (..)

{-| 1d cubic spline segment
-}


type alias Point =
    ( Float, Float )


cubicSplineSegment : Float -> Float -> Float -> Float -> Float -> Float
cubicSplineSegment x0 x1 x2 x3 t =
    let
        a0 =
            -x0 + x1 - x2 + x3

        a1 =
            -x2 + x3

        a2 =
            -x0 + x2

        a3 =
            x1
    in
    (a0 * t ^ 3) + (a1 * t ^ 2) + (a2 * t) + a3
