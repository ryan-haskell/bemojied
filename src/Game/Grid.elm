module Game.Grid exposing
    ( Grid
    , create
    , currentPointsOnBoard
    , settle
    , swap
    , undoSwap
    , view
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Game.Position exposing (Position)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Html.Keyed
import List.Extra
import Random
import Random.Array


config : { size : Int, allowLuckyStart : Bool }
config =
    { size = 7
    , allowLuckyStart = False
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


swap : Position -> Position -> Grid -> Grid
swap p1 p2 (Grid grid) =
    Grid
        { grid
            | transforms =
                grid.transforms
                    |> Dict.insert p1 { current = p1, target = p2 }
                    |> Dict.insert p2 { current = p2, target = p1 }
        }


undoSwap : Grid -> Grid
undoSwap (Grid grid) =
    Grid
        { grid
            | transforms =
                grid.transforms
                    |> Dict.toList
                    |> List.map (\( current, transform ) -> ( current, { transform | target = transform.current } ))
                    |> Dict.fromList
        }


settle : Grid -> Grid
settle (Grid grid) =
    Grid
        { grid
            | arrays =
                List.foldl
                    (\{ current, target } arrays ->
                        case getEmojiAt current grid.arrays of
                            Just e ->
                                setEmojiAt target e arrays

                            Nothing ->
                                arrays
                    )
                    grid.arrays
                    (Dict.values grid.transforms)
            , transforms = Dict.empty
        }


setEmojiAt : Position -> Emoji -> Array (Array Emoji) -> Array (Array Emoji)
setEmojiAt ( x, y ) e array =
    case Array.get y array of
        Just inner ->
            Array.set y (Array.set x e inner) array

        Nothing ->
            array


getEmojiAt : Position -> Array (Array Emoji) -> Maybe Emoji
getEmojiAt ( x, y ) arrays =
    arrays |> Array.get y |> Maybe.andThen (Array.get x)


view :
    { onClick : Position -> msg
    , selected : Maybe Position
    , grid : Grid
    }
    -> Html msg
view options =
    let
        (Grid { arrays, transforms }) =
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
            Html.Keyed.node "div"
                [ Attr.class "grid__cell" ]
                [ ( idToString emoji_.id
                  , Html.div
                        [ Attr.class "animated__cell"
                        , Attr.style "position" "absolute"
                        , case Dict.get position transforms of
                            Just transform ->
                                toCssTransform transform

                            Nothing ->
                                Attr.style "transform" "none"
                        , Attr.style "top" (vmin (size * y))
                        , Attr.style "left" (vmin (size * x))
                        , Attr.style "width" (vmin size)
                        , Attr.style "height" (vmin size)
                        ]
                        [ viewEmoji
                            { onClick = options.onClick position
                            , isSelected = options.selected == Just position
                            , shouldPoof = List.member position scoringPositions
                            }
                            emoji_
                        ]
                  )
                ]
    in
    Html.div [ Attr.class "col gap-md center-x" ]
        [ Html.div [ Attr.class "font-subtitle" ] [ Html.text ("Score: " ++ String.fromInt (scoresFromGroups scoringGroups)) ]
        , Html.div [ Attr.class "col relative" ]
            (List.map viewRow listOfLists)
        ]


currentPointsOnBoard : Grid -> Int
currentPointsOnBoard grid =
    scoresFromGroups (checkForScoringGroups grid)



-- INTERNALS


type alias Internals =
    { arrays : Array (Array Emoji)
    , transforms : Dict Position Transform
    }


type alias Transform =
    { current : Position
    , target : Position
    }


toCssTransform : Transform -> Html.Attribute msg
toCssTransform { current, target } =
    "translate(${x}00%, ${y}00%)"
        |> String.replace "${x}" (String.fromInt (Tuple.first target - Tuple.first current))
        |> String.replace "${y}" (String.fromInt (Tuple.second target - Tuple.second current))
        |> Attr.style "transform"


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
    Random.map2 Internals
        (Random.Array.array config.size (Random.Array.array config.size emoji))
        (Random.constant Dict.empty)


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


viewEmoji :
    { onClick : msg
    , isSelected : Bool
    , shouldPoof : Bool
    }
    -> Emoji
    -> Html msg
viewEmoji { onClick, isSelected, shouldPoof } emoji_ =
    let
        image : String -> Html msg
        image name =
            Html.button
                [ Html.Events.onClick onClick
                , Attr.class ("emoji emoji--" ++ name)
                , Attr.classList
                    [ ( "emoji--poof", shouldPoof )
                    , ( "emoji--selected", isSelected )
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
                GroupOf3 _ _ _ ->
                    10

                GroupOf4 _ _ _ _ ->
                    20

                GroupOf5 _ _ _ _ _ ->
                    30

                GroupOf6 _ _ _ _ _ _ ->
                    50

                GroupOf7 _ _ _ _ _ _ _ ->
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

        _ :: [] ->
            Nothing

        _ :: _ :: [] ->
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
