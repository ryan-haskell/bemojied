module Main exposing (main)

import Browser
import Browser.Events
import Game.Grid
import Game.Phase
import Game.Position exposing (Position)
import Game.SaveData
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Json
import Ports
import Time


type alias Flags =
    { window : { width : Int, height : Int }
    , currentTime : Int
    , saveData : Json.Value
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { window : { width : Int, height : Int }
    , currentTime : Time.Posix
    , saveData : Game.SaveData.SaveData
    , screen : Screen
    }


type Screen
    = MainMenu
    | InGame State


type alias State =
    { grid : Game.Grid.Grid
    , selected : Maybe Position
    , phase : Game.Phase.Phase
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { window = flags.window
      , currentTime = Time.millisToPosix flags.currentTime
      , saveData = Game.SaveData.fromJson flags.saveData
      , screen = MainMenu
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = WindowResized Int Int
    | PlayGameClicked
    | QuitGameClicked
    | Tick Time.Posix
    | AnimationCompleted
    | ClickedEmoji Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized width height ->
            ( { model | window = { width = width, height = height } }
            , Cmd.none
            )

        PlayGameClicked ->
            let
                grid : Game.Grid.Grid
                grid =
                    Game.Grid.create
                        { seed = Time.posixToMillis model.currentTime
                        }

                saveData =
                    Game.SaveData.updateHighScore
                        { current = Game.Grid.currentPointsOnBoard grid }
                        model.saveData
            in
            ( { model
                | screen =
                    InGame
                        { grid = grid
                        , selected = Nothing
                        , phase = Game.Phase.init
                        }
                , saveData = saveData
              }
            , Ports.saveGame saveData
            )

        QuitGameClicked ->
            ( { model | screen = MainMenu }
            , Cmd.none
            )

        Tick currentTime ->
            ( { model | currentTime = currentTime }
            , Cmd.none
            )

        ClickedEmoji clickedPosition ->
            case model.screen of
                MainMenu ->
                    ( model, Cmd.none )

                InGame state ->
                    case state.phase of
                        Game.Phase.WaitingForUser ->
                            let
                                ( nextPhase, cmd ) =
                                    case cellsToSwap of
                                        Just _ ->
                                            Game.Phase.next
                                                { boardHasPoints = Game.Grid.currentPointsOnBoard state.grid > 0
                                                , animationMsg = AnimationCompleted
                                                , phase = state.phase
                                                }

                                        Nothing ->
                                            ( state.phase, Cmd.none )

                                ( selected, cellsToSwap ) =
                                    case state.selected of
                                        Just selectedPosition ->
                                            if clickedPosition == selectedPosition then
                                                ( Nothing, Nothing )

                                            else if Game.Position.isAdjacent clickedPosition selectedPosition then
                                                ( Nothing, Just ( clickedPosition, selectedPosition ) )

                                            else
                                                ( Just clickedPosition, Nothing )

                                        Nothing ->
                                            ( Just clickedPosition, Nothing )
                            in
                            ( { model
                                | screen =
                                    InGame
                                        { state
                                            | selected = selected
                                            , phase = nextPhase
                                            , grid =
                                                case cellsToSwap of
                                                    Just ( a, b ) ->
                                                        Game.Grid.swap a b state.grid

                                                    Nothing ->
                                                        state.grid
                                        }
                              }
                            , cmd
                            )

                        _ ->
                            ( model, Cmd.none )

        AnimationCompleted ->
            case model.screen of
                MainMenu ->
                    ( model, Cmd.none )

                InGame state ->
                    let
                        grid =
                            Game.Grid.settle state.grid

                        ( nextPhase, cmd ) =
                            Game.Phase.next
                                { boardHasPoints = Game.Grid.currentPointsOnBoard grid > 0
                                , animationMsg = AnimationCompleted
                                , phase = state.phase
                                }
                    in
                    ( { model
                        | screen =
                            InGame
                                { state
                                    | phase = nextPhase
                                    , grid =
                                        case ( state.phase, nextPhase ) of
                                            ( Game.Phase.AnimatingSwap, Game.Phase.AnimatingUndoSwap ) ->
                                                Game.Grid.undoSwap state.grid

                                            _ ->
                                                grid
                                }
                      }
                    , cmd
                    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Fremojied"
    , body =
        [ Html.div [ Attr.class "col fill-y" ]
            [ case model.screen of
                MainMenu ->
                    viewMainMenu model

                InGame state ->
                    Html.div [ Attr.class "col fill-y" ]
                        [ Html.text (Debug.toString state.phase)
                        , viewInGame model state
                        ]
            ]
        ]
    }


viewMainMenu : Model -> Html Msg
viewMainMenu model =
    Html.div [ Attr.class "col gap-lg center fill-y" ]
        [ Html.div [ Attr.class "col gap-md center" ]
            [ Html.h1 [ Attr.class "font-title" ] [ Html.text "Fremojied" ]
            , Html.h2 [ Attr.class "font-subtitle" ] [ Html.text "Score points and feel nice!" ]
            ]
        , Html.button [ Attr.class "button", Html.Events.onClick PlayGameClicked ]
            [ Html.text "Play"
            ]
        , case Game.SaveData.highScore model.saveData of
            Just score ->
                Html.h2 [ Attr.class "font-subtitle" ]
                    [ Html.text ("Highscore: " ++ String.fromInt score)
                    ]

            Nothing ->
                Html.text ""
        ]


viewInGame : Model -> State -> Html Msg
viewInGame model state =
    Html.div [ Attr.class "col center fill-y" ]
        [ Game.Grid.view
            { selected = state.selected
            , grid = state.grid
            , onClick = ClickedEmoji
            }
        , Html.div [ Attr.class "fixed align-bottom pad-y-lg" ]
            [ Html.button [ Attr.class "button button--danger", Html.Events.onClick QuitGameClicked ]
                [ Html.text "Quit game"
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Time.every 1000 Tick
        ]
