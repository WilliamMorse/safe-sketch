module Vector2 exposing (..)

{-| A 2d vector module for calculating spline control points
-}


type alias Vector2 =
    { x : Float
    , y : Float
    }


i : Vector2
i =
    Vector2 1 0


j : Vector2
j =
    Vector2 0 1


zero : Vector2
zero =
    Vector2 0 0


map : (Float -> Float) -> Vector2 -> Vector2
map f v =
    Vector2 (f v.x) (f v.y)


map2 : (Float -> Float -> Float) -> Vector2 -> Vector2 -> Vector2
map2 f v1 v2 =
    Vector2 (f v1.x v2.x) (f v1.y v2.y)


sumComponents : Vector2 -> Float
sumComponents { x, y } =
    x + y


add : Vector2 -> Vector2 -> Vector2
add =
    map2 (+)


subtract : Vector2 -> Vector2 -> Vector2
subtract =
    map2 (-)


scale : Float -> Vector2 -> Vector2
scale s =
    map ((*) s)


direction : Vector2 -> Vector2
direction a =
    scale (1 / length a) a


dot : Vector2 -> Vector2 -> Float
dot a b =
    map2 (*) a b |> sumComponents


cross : Vector2 -> Vector2 -> Float
cross a b =
    a.x * b.y - a.y * b.x


lengthSquared : Vector2 -> Float
lengthSquared a =
    dot a a


length : Vector2 -> Float
length =
    lengthSquared >> sqrt


{-| Finds the relative vector from a to b
-}
rel : Vector2 -> Vector2 -> Vector2
rel a b =
    subtract b a


distanceBetween : Vector2 -> Vector2 -> Float
distanceBetween a b =
    length <| rel a b


{-| Project a onto b and return a vector
-}
project : Vector2 -> Vector2 -> Vector2
project a b =
    scale (dot a b / length b) (direction b)


projectLength : Vector2 -> Vector2 -> Float
projectLength a b =
    dot a b / length b


angleBetween : Vector2 -> Vector2 -> Float
angleBetween a b =
    let
        abCosAlpha =
            dot a b

        abSinAlpha =
            cross a b
    in
    -- Using both sine and cosine preserves sensitivity for small angles where the dot product is least sensitive to angle changes.
    atan2 abSinAlpha abCosAlpha


rotate : Float -> Vector2 -> Vector2
rotate angle vector =
    let
        angle0 =
            angleBetween (Vector2 1 0) vector

        dir =
            Vector2 (cos (angle + angle0)) (sin (angle + angle0))
    in
    scale (length vector) dir
