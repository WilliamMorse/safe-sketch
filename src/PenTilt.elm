module PenTilt exposing (Cartesian, Spherical, Tilt, fromCartiesian, toCartesian, toSpherical)


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
    rad * 1



--(180 / pi)


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

        length =
            sqrt (x ^ 2 + y ^ 2 + z ^ 2)
    in
    Cartesian (x / length) (y / length) (z / length)


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


fromCartiesian : Cartesian -> Tilt
fromCartiesian cart =
    let
        x =
            atan2 cart.y cart.z

        y =
            atan2 cart.x cart.z

        r =
            [ cart.x, cart.y, cart.z ]
                |> List.map (\a -> a ^ 2)
                |> List.sum
                |> sqrt
    in
    Tilt r x y
