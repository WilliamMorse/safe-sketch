module Tilt exposing (Cartesian, Spherical, Tilt, toCartesian, toSpherical)


type alias Tilt =
    { r : Float
    , x : Float
    , y : Float
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


radToDec : Float -> Float
radToDec rad =
    rad * (180 / pi)


toCartesian : Tilt -> Cartesian
toCartesian tilt =
    let
        -- These come from the cross product of
        -- the two normal vetors from the two
        -- tilt planes
        x =
            sin tilt.x * cos tilt.y

        y =
            cos tilt.x * sin tilt.y

        z =
            cos tilt.x * cos tilt.y

        norm =
            sqrt (x ^ 2 + y ^ 2 + z ^ 2)
    in
    Cartesian (x / norm) (y / norm) (z / norm)


cartiesian_to_spherical : Cartesian -> Spherical
cartiesian_to_spherical cart =
    let
        r =
            sqrt (cart.x ^ 2 + cart.y ^ 2 + cart.z ^ 2)

        theta =
            radToDec <| atan2 (sqrt (cart.x ^ 2 + cart.y ^ 2)) cart.z

        phi =
            radToDec <| atan2 cart.y cart.x
    in
    Spherical r theta phi


toSpherical : Tilt -> Spherical
toSpherical tilt =
    tilt
        |> toCartesian
        |> cartiesian_to_spherical
