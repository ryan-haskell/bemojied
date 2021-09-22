module Game.SaveData exposing
    ( SaveData
    , fromJson, toJson
    )

{-|

@docs SaveData
@docs fromJson, toJson

-}

import Json.Decode as Json
import Json.Encode as Encode


{-| This represents all the data we want to persist after a player
refreshes the browser.
-}
type SaveData
    = SaveData {}


{-| Converts our data structure to a JSON value so we can
keep it around in localStorage in case the user closes
the tab.
-}
toJson : SaveData -> Json.Value
toJson (SaveData saveData) =
    Encode.object []


{-| Attempts to load the saveData from a JSON value,
defaulting to an empty one if that fails.
-}
fromJson : Json.Value -> SaveData
fromJson json =
    Json.decodeValue decoder json
        |> Result.withDefault empty



-- INTERNALS


empty : SaveData
empty =
    SaveData {}


decoder : Json.Decoder SaveData
decoder =
    Json.succeed (SaveData {})
