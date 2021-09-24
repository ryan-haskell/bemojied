module Game.Position exposing (Position, isAdjacent)


type alias Position =
    ( Int, Int )


isAdjacent : Position -> Position -> Bool
isAdjacent ( x1, y1 ) ( x2, y2 ) =
    (x1 == x2 && abs (y2 - y1) == 1)
        || (y1 == y2 && abs (x2 - x1) == 1)
