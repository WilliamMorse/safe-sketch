module PenTilt exposing (Cartesian, Spherical, Tilt, fromCartiesian, fromSpherical, toCartesian, toSpherical)


type alias Tilt =
    { r : Float
    , tiltX : Float
    , tiltY : Float
    }


type alias Cartesian =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Spherical =
    { r : Float
    , theta : Float
    , phi : Float
    }


toCartesian : Tilt -> Cartesian
toCartesian { r, tiltX, tiltY } =
    let
        -- Take the cross product of
        -- the two normal vetors from the two
        -- tilt planes
        x =
            sin tiltX * cos tiltY

        y =
            cos tiltX * sin tiltY

        z =
            cos tiltX * cos tiltY
    in
    Cartesian (x / r) (y / r) (z / r)


fromCartiesian : Cartesian -> Tilt
fromCartiesian { x, y, z } =
    let
        r =
            sqrt (x ^ 2 + y ^ 2 + z ^ 2)

        tiltX =
            atan2 y z

        tiltY =
            atan2 x z
    in
    Tilt r tiltX tiltY


cartiesian_to_spherical : Cartesian -> Spherical
cartiesian_to_spherical { x, y, z } =
    let
        r =
            sqrt (x ^ 2 + y ^ 2 + z ^ 2)

        theta =
            atan2 (sqrt (x ^ 2 + y ^ 2)) z

        phi =
            atan2 y x
    in
    Spherical r theta phi


spherical_to_cartiesian : Spherical -> Cartesian
spherical_to_cartiesian { r, theta, phi } =
    let
        l =
            r * sin theta

        x =
            l * sin phi

        y =
            l * cos phi

        z =
            r * cos theta
    in
    Cartesian x y z


toSpherical : Tilt -> Spherical
toSpherical =
    toCartesian >> cartiesian_to_spherical


fromSpherical : Spherical -> Tilt
fromSpherical =
    spherical_to_cartiesian >> fromCartiesian
