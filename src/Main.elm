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
    { score : Int
    , grid : Game.Grid.Grid
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
            in
            ( { model
                | screen =
                    InGame
                        { score = 0
                        , grid = grid
                        , selected = Nothing
                        , phase = Game.Phase.init
                        }
              }
            , Cmd.none
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
                                next =
                                    case cellsToSwap of
                                        Just _ ->
                                            Game.Phase.next
                                                { animationMsg = AnimationCompleted
                                                , phase = state.phase
                                                , grid = state.grid
                                                , score = state.score
                                                }

                                        Nothing ->
                                            { phase = state.phase
                                            , grid = state.grid
                                            , score = state.score
                                            , cmd = Cmd.none
                                            }

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
                                            , phase = next.phase
                                            , score = next.score
                                            , grid =
                                                case cellsToSwap of
                                                    Just ( a, b ) ->
                                                        Game.Grid.swap a b state.grid

                                                    Nothing ->
                                                        next.grid
                                        }
                              }
                            , next.cmd
                            )

                        _ ->
                            ( model, Cmd.none )

        AnimationCompleted ->
            case model.screen of
                MainMenu ->
                    ( model, Cmd.none )

                InGame state ->
                    let
                        next =
                            Game.Phase.next
                                { animationMsg = AnimationCompleted
                                , score = state.score
                                , phase = state.phase
                                , grid = state.grid
                                }

                        saveData =
                            Game.SaveData.updateHighScore
                                { current = next.score
                                }
                                model.saveData
                    in
                    ( { model
                        | screen =
                            InGame
                                { state
                                    | phase = next.phase
                                    , grid = next.grid
                                    , score = next.score
                                }
                        , saveData = saveData
                      }
                    , Cmd.batch
                        [ next.cmd
                        , Ports.saveGame saveData
                        ]
                    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Bemojied"
    , body =
        [ Html.div [ Attr.class "col fill-y" ]
            [ case model.screen of
                MainMenu ->
                    viewMainMenu model

                InGame state ->
                    viewInGame model state
            ]
        ]
    }


viewMainMenu : Model -> Html Msg
viewMainMenu model =
    Html.div [ Attr.class "col gap-lg center fill-y" ]
        [ Html.div [ Attr.class "col gap-md center" ]
            [ Html.h1 [ Attr.class "font-title" ] [ Html.text "Bemojied" ]
            , Html.h2 [ Attr.class "font-subtitle" ] [ Html.text "Score points and feel nice!" ]
            ]
        , Html.button [ Attr.class "button", Html.Events.onClick PlayGameClicked ]
            [ Html.text "Play"
            ]
        , case Game.SaveData.highScore model.saveData of
            Just score ->
                Html.h2 [ Attr.class "font-highscore" ]
                    [ Html.text (String.fromInt score)
                    ]

            Nothing ->
                Html.text ""
        ]


viewInGame : Model -> State -> Html Msg
viewInGame _ state =
    Html.div [ Attr.class "col center fill-y" ]
        [ Game.Grid.view
            { score = state.score
            , selected = state.selected
            , grid = state.grid
            , onClick = ClickedEmoji
            , shouldCheckForMatches =
                not
                    (List.member state.phase
                        [ Game.Phase.AnimatingDrop
                        , Game.Phase.CheckForMatches1
                        ]
                    )
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
