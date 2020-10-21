module Matrix exposing (..)

import Array exposing (Array)
import Maybe.Extra as Maybe



{--Highly Untested and Unstructured Wrote this for fun only --}


type alias Matrix =
    List (List Float)


isSquare : Matrix -> Bool
isSquare m =
    let
        height =
            Just <| List.length m

        widths =
            List.map List.length m

        width =
            if List.maximum widths == List.minimum widths then
                Maybe.map List.length (List.head m)

            else
                Nothing
    in
    Maybe.map2 (==) width height
        |> Maybe.withDefault False


is2x2 : Matrix -> Bool
is2x2 m =
    List.length m == 2 && isSquare m


det2x2 : Matrix -> Maybe Float
det2x2 m =
    if is2x2 m then
        case m of
            [ [ a, b ], [ c, d ] ] ->
                Just <| a * d - b * c

            _ ->
                Nothing

    else
        Nothing


combineMaybeFromTuple : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combineMaybeFromTuple ( first, second ) =
    Maybe.map2 (\a b -> ( a, b )) first second



{--| gives you the array left over when you select a slice 

Other perposed names include : splice, heels, resect, dice, toptail
--}


cake : Int -> Int -> Array a -> Array a
cake start stop a =
    let
        left =
            Array.slice 0 start a

        right =
            Array.slice stop (Array.length a) a
    in
    Array.append left right



-- helper function for the determinate matrix reducing (laplace) method


chompDown : Int -> Matrix -> Maybe ( Float, Matrix )
chompDown i m =
    case m of
        h :: t ->
            ( h
                |> Array.fromList
                |> Array.get i
            , t
                |> List.map (Array.fromList >> cake i (i + 1))
                |> List.map Array.toList
                |> Just
            )
                |> combineMaybeFromTuple

        [] ->
            Nothing


{--}
det : Matrix -> Maybe Float
det =
    detHelper 1.0


isEven : Int -> Bool
isEven int =
    case modBy 2 int of
        0 ->
            True

        _ ->
            False


detHelper : Float -> Matrix -> Maybe Float
detHelper acc matrix =
    if is2x2 matrix then
        Maybe.map ((*) acc) (det2x2 matrix)

    else
        List.indexedMap (\i _ -> chompDown i matrix) matrix
            |> Maybe.combine
            |> Maybe.map
                (List.indexedMap
                    (\i ( a, m ) ->
                        if isEven i then
                            detHelper (a * acc) m

                        else
                            detHelper (a * acc * -1) m
                    )
                )
            |> Maybe.map Maybe.combine
            |> Maybe.join
            |> Maybe.map List.sum
--}
