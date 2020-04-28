module Vector3 exposing (Vector, cross, direction, dot, length, negateVector, scale)


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


mapComponents : (Float -> Float) -> Vector -> Vector
mapComponents f { x, y, z } =
    Vector (f x) (f y) (f z)


negateVector : Vector -> Vector
negateVector =
    mapComponents Basics.negate


sumCompnents : Vector -> Float
sumCompnents { x, y, z } =
    x + y + z


lengthSquared : Vector -> Float
lengthSquared =
    mapComponents (\i -> i * i) >> sumCompnents


length : Vector -> Float
length =
    lengthSquared >> sqrt


direction : Vector -> Vector
direction v =
    mapComponents (\i -> i / length v) v


scale : Float -> Vector -> Vector
scale scaleFactor =
    mapComponents ((*) scaleFactor)


dot : Vector -> Vector -> Float
dot a b =
    0.0
        + (a.x * b.x)
        + (a.y * b.y)
        + (a.z * b.z)


cross : Vector -> Vector -> Vector
cross a b =
    { x = a.y * b.z - a.z * b.y
    , y = a.z * b.x - a.x * b.z
    , z = a.x * b.y - a.y * b.x
    }
