module Game.Phase exposing (Phase(..), init, next)

import Game.Grid exposing (Grid)
import Process
import Task


type Phase
    = WaitingForUser
    | AnimatingSwap
    | AnimatingUndoSwap
    | AnimatingScore
    | AnimatingDrop
    | CheckForMatchesStart
    | CheckForMatchesEnd


init : Phase
init =
    WaitingForUser


type alias Update msg =
    { phase : Phase
    , grid : Grid
    , score : Int
    , cmd : Cmd msg
    , combos : List Game.Grid.Group
    }


next :
    { animationMsg : msg
    , phase : Phase
    , grid : Grid
    , score : Int
    , combos : List Game.Grid.Group
    }
    -> Update msg
next { grid, animationMsg, score, phase, combos } =
    let
        animation =
            animate animationMsg

        settledGrid =
            Game.Grid.settle grid

        hasPointsOnBoard =
            Game.Grid.currentPointsOnBoard settledGrid > 0

        update =
            { phase = phase
            , grid = grid
            , score = score
            , cmd = Cmd.none
            , combos = combos
            }
    in
    case phase of
        WaitingForUser ->
            { update
                | phase = AnimatingSwap
                , cmd = animation
            }

        AnimatingSwap ->
            if hasPointsOnBoard then
                { update
                    | phase = AnimatingScore
                    , grid = settledGrid
                    , cmd = animation
                }

            else
                { update
                    | phase = AnimatingUndoSwap
                    , grid = Game.Grid.undoSwap grid
                    , cmd = animation
                }

        AnimatingUndoSwap ->
            { update
                | phase = WaitingForUser
                , grid = settledGrid
                , combos = []
            }

        AnimatingScore ->
            { update
                | phase = AnimatingDrop
                , grid = Game.Grid.fillInDrops grid
                , cmd = animation
                , combos = combos ++ Game.Grid.currentScoringGroups settledGrid
            }

        AnimatingDrop ->
            { update
                | phase = CheckForMatchesStart
                , grid = Game.Grid.clearTransforms grid
                , cmd = animation
            }

        CheckForMatchesStart ->
            { update
                | phase = CheckForMatchesEnd
                , cmd = animation
            }

        CheckForMatchesEnd ->
            if hasPointsOnBoard then
                { update
                    | phase = AnimatingScore
                    , grid = settledGrid
                    , cmd = animation
                }

            else
                { update
                    | phase = WaitingForUser
                    , grid = settledGrid
                    , score = score + Game.Grid.calculateScoreFrom combos
                    , combos = []
                }


animate : msg -> Cmd msg
animate msg =
    Process.sleep 500
        |> Task.perform (\_ -> msg)
