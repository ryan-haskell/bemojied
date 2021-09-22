module Main exposing (main)

import Browser
import Browser.Events
import Game.Grid
import Game.SaveData
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Json
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized width height ->
            ( { model | window = { width = width, height = height } }
            , Cmd.none
            )

        PlayGameClicked ->
            ( { model
                | screen =
                    InGame
                        { grid =
                            Game.Grid.create
                                { seed = Time.posixToMillis model.currentTime
                                }
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
                    viewInGame model state
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
        ]


viewInGame : Model -> State -> Html Msg
viewInGame model state =
    Html.div [ Attr.class "col center fill-y" ]
        [ Game.Grid.view { grid = state.grid }
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
