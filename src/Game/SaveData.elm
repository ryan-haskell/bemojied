module Game.SaveData exposing
    ( SaveData
    , highScore
    , fromJson, toJson
    , updateHighScore
    )

{-|

@docs SaveData
@docs highScore
@docs fromJson, toJson

-}

import Json.Decode as Json
import Json.Encode as Encode


{-| This represents all the data we want to persist after a player
refreshes the browser.
-}
type SaveData
    = SaveData Internals


highScore : SaveData -> Maybe Int
highScore (SaveData data) =
    data.highScore


updateHighScore : { current : Int } -> SaveData -> SaveData
updateHighScore { current } ((SaveData internals) as data) =
    case highScore data of
        Just high ->
            if current > high then
                SaveData { internals | highScore = Just current }

            else
                data

        Nothing ->
            SaveData { internals | highScore = Just current }


{-| Converts our data structure to a JSON value so we can
keep it around in localStorage in case the user closes
the tab.
-}
toJson : SaveData -> Json.Value
toJson (SaveData saveData) =
    Encode.object
        [ ( "highscore"
          , case saveData.highScore of
                Just num ->
                    Encode.int num

                Nothing ->
                    Encode.null
          )
        ]


{-| Attempts to load the saveData from a JSON value,
defaulting to an empty one if that fails.
-}
fromJson : Json.Value -> SaveData
fromJson json =
    Json.decodeValue decoder json
        |> Result.withDefault empty



-- INTERNALS


type alias Internals =
    { highScore : Maybe Int
    }


empty : SaveData
empty =
    SaveData (Internals Nothing)


decoder : Json.Decoder SaveData
decoder =
    Json.map SaveData
        (Json.map Internals
            (Json.field "highscore" (Json.maybe Json.int))
        )
