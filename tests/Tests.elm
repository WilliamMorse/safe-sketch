module Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import PenTilt exposing (Cartesian, Spherical, Tilt)
import Test exposing (..)


withinCart : FloatingPointTolerance -> String -> Cartesian -> Cartesian -> Test
withinCart tolerance testString expecting testing =
    describe testString
        [ test ("x is " ++ String.fromFloat expecting.x) <|
            \_ -> Expect.within tolerance expecting.x testing.x
        , test ("y is " ++ String.fromFloat expecting.y) <|
            \_ -> Expect.within tolerance expecting.y testing.y
        , test ("z is " ++ String.fromFloat expecting.z) <|
            \_ -> Expect.within tolerance expecting.z testing.z
        ]


withinTilt : FloatingPointTolerance -> String -> Tilt -> Tilt -> Test
withinTilt tolerance testString expecting testing =
    describe testString
        [ test ("r is " ++ String.fromFloat expecting.r) <|
            \_ -> Expect.within tolerance expecting.r testing.r
        , test ("TiltX is " ++ String.fromFloat expecting.tiltX) <|
            \_ -> Expect.within tolerance expecting.tiltX testing.tiltX
        , test ("TiltY is " ++ String.fromFloat expecting.tiltY) <|
            \_ -> Expect.within tolerance expecting.tiltY testing.tiltY
        ]


suite : Test
suite =
    describe "Pen Tilt cordinate transformations"
        [ describe "PenTilt.toSpherical >> PenTilt.fromSpherical"
            [ withinTilt (Absolute 1.0e-15)
                "{ r = 1, tiltX = degrees 2, tiltY = degrees -3}"
                { r = 1, tiltX = degrees 2, tiltY = degrees -3 }
                ({ r = 1, tiltX = degrees 2, tiltY = degrees -3 }
                    |> (PenTilt.toSpherical >> PenTilt.fromSpherical)
                )
            , withinTilt (Absolute 1.0e-15)
                "{ r = 20, tiltX = degrees -20, tiltY = degrees -45}"
                { r = 20, tiltX = degrees -20, tiltY = degrees -45 }
                ({ r = 20, tiltX = degrees -20, tiltY = degrees -45 }
                    |> (PenTilt.toSpherical >> PenTilt.fromSpherical)
                )
            ]
        , describe "PenTilt.toCartesian "
            [ describe "is the inverse of PenTilt.fromCartesan"
                [ withinTilt (Absolute 1.0e-15)
                    "{ r = 1, tiltX = degrees 2, tiltY = degrees -3}"
                    { r = 1, tiltX = degrees 2, tiltY = degrees -3 }
                    ({ r = 1, tiltX = degrees 2, tiltY = degrees -3 }
                        |> (PenTilt.toCartesian >> PenTilt.fromCartesian)
                    )
                , withinTilt (Absolute 1.0e-15)
                    "{ r = 20, tiltX = degrees -20, tiltY = degrees -45}"
                    { r = 20, tiltX = degrees -20, tiltY = degrees -45 }
                    ({ r = 20, tiltX = degrees -20, tiltY = degrees -45 }
                        |> (PenTilt.toCartesian >> PenTilt.fromCartesian)
                    )
                ]
            , describe "returns a unit vector with only one non-zero component when passed a appropreate tilt value "
                [ withinCart (Absolute 1.0e-15)
                    "x hat"
                    { x = 1, y = 0, z = 0 }
                    (PenTilt.toCartesian { r = 1, tiltX = degrees 90, tiltY = 0 })
                , withinCart (Absolute 1.0e-15)
                    "y hat"
                    { x = 0, y = 1, z = 0 }
                    (PenTilt.toCartesian { r = 1, tiltX = 0, tiltY = degrees 90 })
                , withinCart (Absolute 1.0e-15)
                    "z hat"
                    { x = 0, y = 0, z = -1 }
                    (PenTilt.toCartesian { r = 1, tiltX = 0, tiltY = 0 })
                , withinCart (Absolute 1.0e-15)
                    "z hat with flip"
                    { x = 0, y = 0, z = -1 }
                    (PenTilt.toCartesian { r = 1, tiltX = degrees 180, tiltY = degrees 180 })
                , withinCart (Absolute 1.0e-15)
                    "-x hat"
                    { x = -1, y = 0, z = 0 }
                    (PenTilt.toCartesian { r = 1, tiltX = degrees -90, tiltY = 0 })
                , withinCart (Absolute 1.0e-15)
                    "-y hat"
                    { x = 0, y = -1, z = 0 }
                    (PenTilt.toCartesian { r = 1, tiltX = 0, tiltY = degrees -90 })
                , withinCart (Absolute 1.0e-15)
                    "-z hat"
                    { x = 0, y = 0, z = 1 }
                    (PenTilt.toCartesian { r = 1, tiltX = degrees 180, tiltY = 0 })
                , withinCart (Absolute 1.0e-15)
                    "-z hat with flip"
                    { x = 0, y = 0, z = 1 }
                    (PenTilt.toCartesian { r = 1, tiltX = 0, tiltY = degrees 180 })
                ]
            , describe "some in-between angles as well"
                [ withinCart (Absolute 1.0e-15)
                    "in the x = 0 plane "
                    { x = 0, y = sqrt 3, z = -1 }
                    (PenTilt.toCartesian { r = 2, tiltX = 0, tiltY = degrees 60 })
                , withinCart (Absolute 1.0e-15)
                    "in the y = 0 plane "
                    { x = sqrt 3, y = 0, z = -1 }
                    (PenTilt.toCartesian { r = 2, tiltX = degrees 60, tiltY = 0 })
                ]
            ]
        ]
