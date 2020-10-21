module PenTilt exposing (Cartesian, Spherical, Tilt, cartesian_to_spherical, fromCartesian, fromSpherical, toCartesian, toSpherical, toVector)

import Vector3 as V3


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



-- a b (sin theta)


toCartesian : Tilt -> Cartesian
toCartesian { r, tiltX, tiltY } =
    let
        -- Take the cross product of
        -- the two normal vetors from the two
        -- tilt planes
        px =
            { x = -1 * cos tiltX, y = 0, z = -1 * sin tiltX }

        py =
            { x = 0, y = -1 * cos tiltY, z = -1 * sin tiltY }

        { x, y, z } =
            V3.cross py px
                |> V3.direction
                |> V3.scale r

        {--
        {-- pure trig version --}
        x =
            r * sin tiltX

        y =
            r * sin tiltY

        l =
            sqrt (x ^ 2 + y ^ 2)

        z =
            sqrt (r ^ 2 - l ^ 2)
        --}
    in
    Cartesian x y z



{--| This is a strage function because the tiltXY cordinate system has areas of gimble lock (if both tiltX and tiltY are 90deg. In practice this is okay because the pen is never through the screen or completely flat against it, but it does pose a challange when different manufacurers choose different cordinate systems. Most screen cordinates are right handed with z into the screen but the coordinate system on the pointer events page is right handed, z out of the screen. This means that the tilt calculations will vary depending on the platform. On my chromebook the screen cordinates are right handed into the screen z so all the cartesan z values are negative in this file. It would be interesting to make a visualization to test this on other platforms.
--}


fromCartesian : Cartesian -> Tilt
fromCartesian c =
    let
        r =
            V3.length c

        tiltX =
            atan2 c.x (-1 * c.z)

        tiltY =
            atan2 c.y (-1 * c.z)
    in
    Tilt r tiltX tiltY


cartesian_to_spherical : Cartesian -> Spherical
cartesian_to_spherical { x, y, z } =
    let
        r =
            sqrt (x ^ 2 + y ^ 2 + z ^ 2)

        theta =
            atan2 (sqrt (x ^ 2 + y ^ 2)) (-1 * z)

        phi =
            atan2 y x
    in
    Spherical r theta phi


spherical_to_cartesian : Spherical -> Cartesian
spherical_to_cartesian { r, theta, phi } =
    let
        l =
            r * sin theta

        x =
            l * cos phi

        y =
            l * sin phi

        z =
            -1 * r * cos theta
    in
    Cartesian x y z


toSpherical : Tilt -> Spherical
toSpherical =
    toCartesian >> cartesian_to_spherical


fromSpherical : Spherical -> Tilt
fromSpherical =
    spherical_to_cartesian >> fromCartesian


toVector : Cartesian -> V3.Vector3
toVector =
    identity
