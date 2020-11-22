module Zipper exposing
    ( Zipper(..)
    , bottom
    , down
    , downOrBottom
    , fromHeadTail
    , fromList
    , index
    , item
    , length
    , map
    , setItem
    , toList
    , top
    , up
    , upOrTop
    , updateItem
    )


type Zipper a
    = Zipper (List a) a (List a)


{-| Constructs a Zipper from a List focused on the head of the list.
-}
fromList : List a -> Maybe (Zipper a)
fromList l =
    case l of
        h :: t ->
            Just <| fromHeadTail h t

        [] ->
            Nothing


fromHeadTail : a -> List a -> Zipper a
fromHeadTail h t =
    Zipper [] h t


toList : Zipper a -> List a
toList (Zipper crumbs selected tail) =
    List.reverse crumbs ++ (selected :: tail)


map : (a -> b) -> Zipper a -> Zipper b
map f (Zipper crumbs selected tail) =
    Zipper (List.map f crumbs) (f selected) (List.map f tail)


item : Zipper a -> a
item (Zipper _ a _) =
    a


down : Zipper a -> Maybe (Zipper a)
down (Zipper crumbs selected tail) =
    case tail of
        h :: t ->
            Just (Zipper (selected :: crumbs) h t)

        [] ->
            Nothing


up : Zipper a -> Maybe (Zipper a)
up (Zipper crumbs selected tail) =
    case crumbs of
        h :: t ->
            Just (Zipper t h (selected :: tail))

        [] ->
            Nothing


top : Zipper a -> Zipper a
top z =
    case up z of
        Just zup ->
            top zup

        Nothing ->
            z


bottom : Zipper a -> Zipper a
bottom z =
    case down z of
        Just zDown ->
            bottom zDown

        Nothing ->
            z


upOrTop : Zipper a -> Zipper a
upOrTop z =
    Maybe.withDefault z (up z)


downOrBottom : Zipper a -> Zipper a
downOrBottom z =
    Maybe.withDefault z (down z)


index : Zipper a -> Int
index (Zipper crumbs selected tail) =
    List.length crumbs


length : Zipper a -> Int
length =
    toList >> List.length


setItem : Zipper a -> a -> Zipper a
setItem (Zipper crumbs _ tail) newItem =
    Zipper crumbs newItem tail


updateItem : (a -> a) -> Zipper a -> Zipper a
updateItem f z =
    setItem z (f (item z))
