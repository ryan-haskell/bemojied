module Game.Grid exposing (Grid, create, currentScore, view)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed
import List.Extra
import Random
import Random.Array


config : { size : Int, allowLuckyStart : Bool }
config =
    { size = 7
    , allowLuckyStart = True
    }


type Grid
    = Grid Internals


create : { seed : Int } -> Grid
create options =
    generateWithSeed (Random.initialSeed options.seed)


generateWithSeed : Random.Seed -> Grid
generateWithSeed seed =
    let
        ( grid, nextSeed ) =
            Random.step
                (Random.map Grid generator)
                seed
    in
    if config.allowLuckyStart || List.isEmpty (checkForScoringGroups grid) then
        grid

    else
        generateWithSeed nextSeed


type alias Indexed value =
    ( Position, value )


type alias Position =
    ( Int, Int )


view : { grid : Grid } -> Html msg
view options =
    let
        (Grid { arrays }) =
            options.grid

        listOfLists : List (List (Indexed Emoji))
        listOfLists =
            toIndexed2DList arrays

        viewRow : List (Indexed Emoji) -> Html msg
        viewRow row =
            Html.div [ Attr.class "row" ]
                (List.map viewCell row)

        scoringGroups : List Group
        scoringGroups =
            checkForScoringGroups options.grid

        scoringPositions : List Position
        scoringPositions =
            positionsFromGroups scoringGroups

        viewCell : Indexed Emoji -> Html msg
        viewCell ( ( x, y ) as position, emoji_ ) =
            let
                size =
                    12

                vmin : Int -> String
                vmin num =
                    String.fromInt num ++ "vmin"
            in
            Html.div [ Attr.class "grid__cell" ]
                [ Html.Keyed.node "div"
                    [ Attr.style "position" "absolute"
                    , Attr.style "top" (vmin (size * y))
                    , Attr.style "left" (vmin (size * x))
                    , Attr.style "width" (vmin size)
                    , Attr.style "height" (vmin size)
                    ]
                    [ ( idToString emoji_.id
                      , viewEmoji
                            { shouldPoof = List.member position scoringPositions
                            }
                            emoji_
                      )
                    ]
                ]
    in
    Html.div [ Attr.class "col gap-md center-x" ]
        [ Html.div [ Attr.class "font-subtitle" ] [ Html.text ("Score: " ++ String.fromInt (scoresFromGroups scoringGroups)) ]
        , Html.div [ Attr.class "col relative" ]
            (List.map viewRow listOfLists)
        ]


currentScore : Grid -> Int
currentScore grid =
    scoresFromGroups (checkForScoringGroups grid)



-- INTERNALS


type alias Internals =
    { arrays : Array (Array Emoji)
    }


type alias Emoji =
    { id : Id
    , style : EmojiStyle
    }


type Id
    = Id Int


idToString : Id -> String
idToString (Id num) =
    String.fromInt num


type EmojiStyle
    = Dog
    | Cat
    | Mouse
    | Pig
    | Frog
    | Fox
    | Bear


generator : Random.Generator Internals
generator =
    Random.map Internals
        (Random.Array.array config.size (Random.Array.array config.size emoji))


emoji : Random.Generator Emoji
emoji =
    Random.map2 Emoji
        id
        (Random.uniform Dog
            [ Cat
            , Mouse
            , Pig
            , Frog
            , Fox
            , Bear
            ]
        )


id : Random.Generator Id
id =
    Random.map Id
        (Random.int 0 Random.maxInt)


viewEmoji : { shouldPoof : Bool } -> Emoji -> Html msg
viewEmoji { shouldPoof } emoji_ =
    let
        image : String -> Html msg
        image name =
            Html.div
                [ Attr.class ("emoji emoji--" ++ name)
                , Attr.classList
                    [ ( "emoji--poof"
                      , shouldPoof
                      )
                    ]
                ]
                []
    in
    case emoji_.style of
        Dog ->
            image "dog"

        Cat ->
            image "cat"

        Mouse ->
            image "mouse"

        Pig ->
            image "pig"

        Frog ->
            image "frog"

        Fox ->
            image "fox"

        Bear ->
            image "bear"


checkForScoringGroups : Grid -> List Group
checkForScoringGroups (Grid internals) =
    let
        lists : List (List ( ( Int, Int ), Emoji ))
        lists =
            toIndexed2DList internals.arrays

        rows : List (List ( ( Int, Int ), Emoji ))
        rows =
            lists

        columns : List (List ( ( Int, Int ), Emoji ))
        columns =
            List.Extra.transpose lists

        sameEmoji : ( a, Emoji ) -> ( a, Emoji ) -> Bool
        sameEmoji ( _, e1 ) ( _, e2 ) =
            e1.style == e2.style

        toNormalList : ( a, List a ) -> List a
        toNormalList ( x, xs ) =
            x :: xs

        getGroupsFor : List ( ( Int, Int ), Emoji ) -> List Group
        getGroupsFor indexedEmojis =
            indexedEmojis
                |> List.Extra.groupWhile sameEmoji
                |> List.map (toNormalList >> List.map Tuple.first)
                |> List.filterMap toGroups
    in
    List.concat
        [ List.concatMap getGroupsFor rows
        , List.concatMap getGroupsFor columns
        ]


type Group
    = GroupOf3 ( Int, Int ) ( Int, Int ) ( Int, Int )
    | GroupOf4 ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int )
    | GroupOf5 ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int )
    | GroupOf6 ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int )
    | GroupOf7 ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int ) ( Int, Int )


positionsFromGroups : List Group -> List Position
positionsFromGroups groups =
    List.concatMap positionsFromGroup groups


positionsFromGroup : Group -> List Position
positionsFromGroup group =
    case group of
        GroupOf3 a b c ->
            [ a, b, c ]

        GroupOf4 a b c d ->
            [ a, b, c, d ]

        GroupOf5 a b c d e ->
            [ a, b, c, d, e ]

        GroupOf6 a b c d e f ->
            [ a, b, c, d, e, f ]

        GroupOf7 a b c d e f g ->
            [ a, b, c, d, e, f, g ]


scoresFromGroups : List Group -> Int
scoresFromGroups groups =
    let
        levelMultiplier =
            1

        individualGroupScore : Group -> Int
        individualGroupScore group =
            case group of
                GroupOf3 a b c ->
                    10

                GroupOf4 a b c d ->
                    20

                GroupOf5 a b c d e ->
                    30

                GroupOf6 a b c d e f ->
                    50

                GroupOf7 a b c d e f g ->
                    100

        comboScore : Int
        comboScore =
            case List.length groups of
                0 ->
                    0

                1 ->
                    0

                2 ->
                    10

                3 ->
                    20

                4 ->
                    30

                5 ->
                    50

                6 ->
                    70

                7 ->
                    100

                8 ->
                    150

                _ ->
                    200
    in
    levelMultiplier * (List.sum (List.map individualGroupScore groups) + comboScore)


toGroups : List ( Int, Int ) -> Maybe Group
toGroups indexes =
    case indexes of
        [] ->
            Nothing

        a :: [] ->
            Nothing

        a :: b :: [] ->
            Nothing

        a :: b :: c :: [] ->
            Just (GroupOf3 a b c)

        a :: b :: c :: d :: [] ->
            Just (GroupOf4 a b c d)

        a :: b :: c :: d :: e :: [] ->
            Just (GroupOf5 a b c d e)

        a :: b :: c :: d :: e :: f :: [] ->
            Just (GroupOf6 a b c d e f)

        a :: b :: c :: d :: e :: f :: g :: [] ->
            Just (GroupOf7 a b c d e f g)

        _ ->
            Nothing


to2DList : Array (Array item) -> List (List item)
to2DList arrays =
    arrays |> Array.map Array.toList |> Array.toList


toIndexed2DList : Array (Array item) -> List (List ( ( Int, Int ), item ))
toIndexed2DList arrays =
    arrays
        |> Array.indexedMap
            (\y ->
                Array.toList
                    << Array.indexedMap
                        (\x item -> ( ( x, y ), item ))
            )
        |> Array.toList
