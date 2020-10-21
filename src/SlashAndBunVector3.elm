module SlashAndBunVector3 exposing (..)


type alias Vector3 =
    { x : Float
    , y : Float
    , z : Float
    }


zero : Vector3
zero =
    Vector3 0 0 0


map : (Float -> Float) -> Vector3 -> Vector3
map f { x, y, z } =
    Vector3 (f x) (f y) (f z)


map2 : (Float -> Float -> Float) -> Vector3 -> Vector3 -> Vector3
map2 f a b =
    Vector3 (f a.x b.x) (f a.y b.y) (f a.z b.z)


foldl : (Float -> b -> b) -> b -> Vector3 -> b
foldl f inital { x, y, z } =
    inital |> f x |> f y |> f z


sumComponents : Vector3 -> Float
sumComponents =
    foldl (+) 0


add : Vector3 -> Vector3 -> Vector3
add =
    map2 (+)


subtract : Vector3 -> Vector3 -> Vector3
subtract =
    map2 (-)


scale : Float -> Vector3 -> Vector3
scale scaleFactor =
    map ((*) scaleFactor)


direction : Vector3 -> Vector3
direction a =
    scale (1 / length a) a


dot : Vector3 -> Vector3 -> Float
dot a b =
    map2 (*) a b |> sumComponents


cross : Vector3 -> Vector3 -> Vector3
cross a b =
    { x = a.y * b.z - a.z * b.y
    , y = a.z * b.x - a.x * b.z
    , z = a.x * b.y - a.y * b.x
    }


length : Vector3 -> Float
length a =
    dot a a |> sqrt


rel : Vector3 -> Vector3 -> Vector3
rel a b =
    subtract b a



-- triple products


scalarTripleProduct : Vector3 -> Vector3 -> Vector3 -> Float
scalarTripleProduct a b c =
    dot a (cross b c)


vectorTripleProduct : Vector3 -> Vector3 -> Vector3 -> Vector3
vectorTripleProduct a b c =
    cross a (cross b c)



{--| Vector projection. 'project a b' Projects a onto b --}


project : Vector3 -> Vector3 -> Vector3
project a b =
    -- scale (dot a b / dot b b) b
    -- scale (dot a b / length b) (direction b)
    scale (dot a b / dot b b) b


reject : Vector3 -> Vector3 -> Vector3
reject a b =
    project a b
        |> subtract a


projectLength : Vector3 -> Vector3 -> Float
projectLength a b =
    dot a b / length b


inverseProject : Vector3 -> Vector3 -> Vector3
inverseProject a b =
    scale (dot a a / dot a b) b



{--| Angle between vectors --}


angleBetween : Vector3 -> Vector3 -> Float
angleBetween a b =
    let
        abCosAlpha =
            dot a b

        abSinAlpha =
            length (cross a b)
    in
    -- Using both sine and cosine preserves sensitivity for small angles where the dot product is least sensitive to angle changes.
    atan2 abSinAlpha abCosAlpha


projectVectorOntoPlane : Vector3 -> Plane -> Vector3
projectVectorOntoPlane vector plane =
    reject vector plane.normal


toDeg : Float -> Float
toDeg =
    (*) (180 / pi)


rotateAngleAxis : Float -> Vector3 -> Vector3 -> Vector3
rotateAngleAxis angle axis v =
    zero
        |> add (scale (cos angle) v)
        |> add (scale (dot axis v * (1 - cos angle)) axis)
        |> add (scale (sin angle) (cross v axis))


type alias Line =
    { point : Vector3
    , dir : Vector3
    }


type alias Plane =
    { point : Vector3
    , normal : Vector3
    }


zeroOffsetPlane : Float -> Float -> Float -> Plane
zeroOffsetPlane x y z =
    Plane (Vector3 0 0 0) (Vector3 x y z)


pierce : Plane -> Line -> Maybe Vector3
pierce plane line =
    let
        altitude =
            dot plane.normal (rel line.point plane.point)

        decentRate =
            dot plane.normal line.dir

        t =
            altitude / decentRate
    in
    if decentRate /= 0 then
        Just <| add line.point (scale t line.dir)

    else
        Nothing


lineFromPlanePlaneIntersection : Plane -> Plane -> Maybe Line
lineFromPlanePlaneIntersection a b =
    let
        intersectionLineDirection =
            cross a.normal b.normal

        lineInPlaneA =
            Line a.point (cross a.normal intersectionLineDirection)

        pointOnIntersectionLine =
            pierce b lineInPlaneA
    in
    Maybe.map (\linePoint -> Line linePoint intersectionLineDirection) pointOnIntersectionLine


pointFromThreePlanes : Plane -> Plane -> Plane -> Maybe Vector3
pointFromThreePlanes a b c =
    lineFromPlanePlaneIntersection a b
        |> Maybe.andThen (pierce c)
