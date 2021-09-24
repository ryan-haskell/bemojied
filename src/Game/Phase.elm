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


init : Phase
init =
    WaitingForUser


next :
    { boardHasPoints : Bool
    , animationMsg : msg
    , phase : Phase
    }
    -> ( Phase, Cmd msg )
next { boardHasPoints, animationMsg, phase } =
    case phase of
        WaitingForUser ->
            ( AnimatingSwap, animate animationMsg )

        AnimatingSwap ->
            if boardHasPoints then
                ( AnimatingScore, animate animationMsg )

            else
                ( AnimatingUndoSwap, animate animationMsg )

        AnimatingUndoSwap ->
            ( WaitingForUser, Cmd.none )

        AnimatingScore ->
            ( AnimatingDrop, animate animationMsg )

        AnimatingDrop ->
            if boardHasPoints then
                ( AnimatingScore, animate animationMsg )

            else
                ( WaitingForUser, Cmd.none )


animate : msg -> Cmd msg
animate msg =
    Process.sleep 1000
        |> Task.perform (\_ -> msg)
