module Game.Grid exposing
    ( Grid
    , clearTransforms
    , create
    , currentPointsOnBoard
    , fillInDrops
    , hasNoMoreMoves
    , settle
    , swap
    , toIndexed2DList
    , undoSwap
    , upwardLoop
    , view
    )

import Dict exposing (Dict)
import Game.Position exposing (Position)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Html.Keyed
import List.Extra
import Random


config : { size : Int, allowLuckyStart : Bool }
config =
    { size = 7
    , allowLuckyStart = False
    }


type alias Swap =
    { left : Position
    , right : Position
    }


allPossibleSwaps : List Swap
allPossibleSwaps =
    let
        toRow : (Int -> Int -> Swap) -> List Swap
        toRow toSwap =
            List.map2 toSwap
                (List.range 0 (config.size - 1))
                (List.range 1 (config.size - 1))
    in
    List.range 0 (config.size - 1)
        |> List.concatMap
            (\i ->
                List.concat
                    [ toRow (\a b -> Swap ( i, a ) ( i, b ))
                    , toRow (\a b -> Swap ( a, i ) ( b, i ))
                    ]
            )


hasNoMoreMoves : Grid -> Bool
hasNoMoreMoves grid =
    List.all (scoredNoPoints grid) allPossibleSwaps


scoredNoPoints : Grid -> Swap -> Bool
scoredNoPoints grid swap_ =
    grid
        |> swap swap_.left swap_.right
        |> settle
        |> checkForScoringGroups
        |> List.isEmpty


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
                (Random.map Grid (generator seed))
                seed
    in
    if config.allowLuckyStart || List.isEmpty (checkForScoringGroups grid) then
        grid |> setSeed nextSeed

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


clearTransforms : Grid -> Grid
clearTransforms (Grid grid) =
    Grid { grid | transforms = Dict.empty }


fillInDrops : Grid -> Grid
fillInDrops (Grid grid) =
    let
        positionsToRemove =
            positionsFromGroups (checkForScoringGroups (Grid grid))

        dictWithRemovedCells =
            List.foldl Dict.remove
                grid.dict
                positionsToRemove

        internals : Internals
        internals =
            List.foldl
                (\x initial ->
                    let
                        { counter, transforms, dict } =
                            upwardLoop initial x
                    in
                    generateNewEmojisAbove
                        { x = x
                        , count = counter
                        }
                        { grid
                            | transforms = transforms
                            , dict = dict
                            , seed = initial.seed
                        }
                )
                { grid
                    | transforms = Dict.empty
                    , dict = dictWithRemovedCells
                }
                (List.range 0 (config.size - 1))
    in
    Grid internals


generateNewEmojisAbove : { x : Int, count : Int } -> Internals -> Internals
generateNewEmojisAbove { x, count } internals =
    let
        ( newEmojis, newSeed ) =
            Random.step (Random.list count emoji) internals.seed

        newEmojiDict : Dict ( Int, Int ) Emoji
        newEmojiDict =
            List.indexedMap (\y e -> ( ( x, y ), e )) newEmojis
                |> Dict.fromList
    in
    { internals
        | seed = newSeed
        , dict = Dict.union newEmojiDict internals.dict
        , transforms =
            List.foldl (\( _, y ) t -> Dict.insert ( x, y ) { current = ( x, y - count ), target = ( x, y ) } t)
                internals.transforms
                (Dict.keys newEmojiDict)
    }


upwardLoop :
    { a
        | transforms : Dict Position Transform
        , dict : Dict Position item
    }
    -> Int
    ->
        { counter : Int
        , transforms : Dict Position Transform
        , dict : Dict Position item
        }
upwardLoop initial x =
    List.foldr
        (\y ({ counter, transforms, dict } as data) ->
            case Dict.get ( x, y ) initial.dict of
                Nothing ->
                    { data | counter = counter + 1 }

                Just item ->
                    if counter > 0 then
                        { data
                            | transforms = Dict.insert ( x, y + counter ) { current = ( x, y ), target = ( x, y + counter ) } transforms
                            , dict = Dict.insert ( x, y + counter ) item dict
                        }

                    else
                        data
        )
        { counter = 0
        , transforms = initial.transforms
        , dict = initial.dict
        }
        (List.range 0 (config.size - 1))


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
            | dict =
                List.foldl
                    (\{ current, target } dict ->
                        case Dict.get current grid.dict of
                            Just e ->
                                Dict.insert target e dict

                            Nothing ->
                                dict
                    )
                    grid.dict
                    (Dict.values grid.transforms)
            , transforms = Dict.empty
        }


view :
    { score : Int
    , onClick : Position -> msg
    , selected : Maybe Position
    , grid : Grid
    , shouldCheckForMatches : Bool
    , onNewGameClicked : msg
    , onQuitGameClicked : msg
    }
    -> Html msg
view options =
    let
        (Grid { dict, transforms }) =
            options.grid

        listOfLists : List (List (Indexed Emoji))
        listOfLists =
            toIndexed2DList dict

        viewRow : List (Indexed Emoji) -> Html msg
        viewRow row =
            Html.div [ Attr.class "col" ]
                (List.map viewCell row)

        scoringGroups : List Group
        scoringGroups =
            checkForScoringGroups options.grid

        scoringPositions : List Position
        scoringPositions =
            positionsFromGroups scoringGroups

        viewCell : Indexed Emoji -> Html msg
        viewCell ( ( x, y ) as position, emoji_ ) =
            Html.Keyed.node "div"
                [ Attr.class "grid__cell" ]
                [ ( idToString emoji_.id
                  , Html.div
                        (case Dict.get position transforms of
                            Just transform ->
                                if ( x, y ) == transform.current then
                                    [ Attr.class "animated__cell"
                                    , toCssTransform transform
                                    ]

                                else
                                    [ Attr.class "animated__cell"
                                    , toDropCssTransform transform
                                    ]

                            Nothing ->
                                [ Attr.class "animated__cell" ]
                        )
                        [ viewEmoji
                            { onClick = options.onClick position
                            , isSelected = options.selected == Just position
                            , shouldPoof = List.member position scoringPositions && options.shouldCheckForMatches
                            }
                            emoji_
                        ]
                  )
                ]
    in
    Html.div [ Attr.class "col gap-md center-x" ]
        [ Html.div [ Attr.class "font-score" ] [ Html.text (String.fromInt options.score) ]
        , Html.div [ Attr.class "row relative clip" ]
            (List.map viewRow listOfLists)
        , if options.shouldCheckForMatches && hasNoMoreMoves options.grid then
            Html.div []
                [ Html.div [ Attr.class "fixed fill popup__overlay" ] []
                , Html.div [ Attr.class "fixed center" ]
                    [ Html.div [ Attr.class "popup col gap-lg" ]
                        [ Html.div [ Attr.class "col" ]
                            [ Html.h3 [ Attr.class "popup__title" ]
                                [ Html.text "Game over" ]
                            , Html.p [ Attr.class "popup__subtitle" ]
                                [ Html.text "There are no more moves left..." ]
                            ]
                        , Html.div [ Attr.class "row gap-md" ]
                            [ Html.button [ Attr.class "popup__button", Html.Events.onClick options.onNewGameClicked ] [ Html.text "New game" ]
                            , Html.button [ Attr.class "popup__button", Html.Events.onClick options.onQuitGameClicked ] [ Html.text "Back to menu" ]
                            ]
                        ]
                    ]
                ]

          else
            Html.text ""
        ]


currentPointsOnBoard : Grid -> Int
currentPointsOnBoard grid =
    scoresFromGroups (checkForScoringGroups grid)



-- INTERNALS


type alias Internals =
    { seed : Random.Seed
    , dict : Dict Position Emoji
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


toDropCssTransform : Transform -> Html.Attribute msg
toDropCssTransform { current, target } =
    "translate(${x}00%, ${y}00%)"
        |> String.replace "${x}" (String.fromInt (Tuple.first current - Tuple.first target))
        |> String.replace "${y}" (String.fromInt (Tuple.second current - Tuple.second target))
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


generator : Random.Seed -> Random.Generator Internals
generator seed =
    Random.map2 (Internals seed)
        (Random.list (config.size * config.size) emoji
            |> Random.map
                (List.indexedMap (\i e -> ( ( modBy config.size i, i // config.size ), e )) >> Dict.fromList)
        )
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
            toIndexed2DList internals.dict

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


toIndexed2DList : Dict ( Int, Int ) item -> List (List (Indexed item))
toIndexed2DList dict =
    Dict.toList dict
        |> List.Extra.groupsOf config.size


setSeed : Random.Seed -> Grid -> Grid
setSeed seed (Grid grid) =
    Grid { grid | seed = seed }
