module Vector exposing (Point, add, distanceBetween, dotProduct, length, length2, multiply, norm, norm2, proj, projL, rel, subtract)

{-| A 2d vector module for calculating spline control points
-}


type alias Point =
    ( Float, Float )


add : Point -> Point -> Point
add ( a1, a2 ) ( b1, b2 ) =
    let
        x =
            a1 + b1

        y =
            a2 + b2
    in
    ( x, y )


subtract : Point -> Point -> Point
subtract ( a1, a2 ) ( b1, b2 ) =
    let
        x =
            b1 - a1

        y =
            b2 - a2
    in
    ( x, y )


multiply : Point -> Float -> Point
multiply ( a1, a2 ) s =
    ( s * a1, s * a2 )


{-| Subtracts the second point from the first point vectorially
-}
dotProduct : Point -> Point -> Float
dotProduct ( a1, a2 ) ( b1, b2 ) =
    a1 * b1 + a2 * b2


length2 : Point -> Float
length2 ( a1, a2 ) =
    a1 ^ 2 + a2 ^ 2


length : Point -> Float
length a =
    length2 a ^ 0.5


distanceBetween : Point -> Point -> Float
distanceBetween a b =
    length <| subtract a b


{-| Finds the relative vector from a to b
-}
rel : Point -> Point -> Point
rel a b =
    subtract a b


norm : Point -> Point
norm a =
    multiply a (length a ^ -1)


norm2 : Point -> Point
norm2 a =
    multiply a (length2 a ^ -1)


{-| Project a onto b and return a vector
-}
proj : Point -> Point -> Point
proj a b =
    multiply
        b
        (dotProduct a b / length2 b)


projL : Point -> Point -> Float
projL a b =
    dotProduct a b / length b
