port module Ports exposing (saveGame)

import Game.SaveData exposing (SaveData)
import Json.Decode as Json


saveGame : SaveData -> Cmd msg
saveGame saveData =
    save (Game.SaveData.toJson saveData)


port save : Json.Value -> Cmd msg
