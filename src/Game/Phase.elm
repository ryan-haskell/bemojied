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
    | CheckForMatches1
    | CheckForMatches2


init : Phase
init =
    WaitingForUser


type alias Update msg =
    { phase : Phase
    , grid : Grid
    , score : Int
    , cmd : Cmd msg
    }


next :
    { animationMsg : msg
    , phase : Phase
    , grid : Grid
    , score : Int
    }
    -> Update msg
next { grid, animationMsg, score, phase } =
    let
        newScore =
            Game.Grid.currentPointsOnBoard settledGrid + score

        animation =
            animate animationMsg

        settledGrid =
            Game.Grid.settle grid

        hasPointsOnBoard =
            Game.Grid.currentPointsOnBoard settledGrid > 0

        update =
            { phase = phase
            , grid = grid
            , score = newScore
            , cmd = Cmd.none
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
            }

        AnimatingScore ->
            { update
                | phase = AnimatingDrop
                , grid = Game.Grid.fillInDrops grid
                , cmd = animation
            }

        AnimatingDrop ->
            { update
                | phase = CheckForMatches1
                , grid = Game.Grid.clearTransforms grid
                , cmd = animation
            }

        CheckForMatches1 ->
            { update
                | phase = CheckForMatches2
                , cmd = animation
            }

        CheckForMatches2 ->
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
                }


animate : msg -> Cmd msg
animate msg =
    Process.sleep 500
        |> Task.perform (\_ -> msg)
