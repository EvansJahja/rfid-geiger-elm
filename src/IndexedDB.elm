module IndexedDB exposing (..)
import Json.Decode
import Json.Encode
import Task


type alias CmdMsg msg = (IndexedDB -> Cmd msg)

type alias IndexedDB  = { }

new : IndexedDB
new = { }


type alias Item = { title: String }
itemDecoder : Json.Decode.Decoder Item
itemDecoder =
    Json.Decode.map Item
        (Json.Decode.field "title" Json.Decode.string)

type IndexedDBResult = FindItemResult (Maybe Item)

receive : (String, Json.Decode.Value) -> IndexedDBResult
receive (cmd, value) = 
    case cmd of
        "findItemResult" ->
            case Json.Decode.decodeValue itemDecoder value of
                Ok item ->
                    FindItemResult (Just item)
                Err _ ->
                    FindItemResult Nothing
        _ ->
            FindItemResult Nothing

-- receive : (IndexedDB -> msg) -> IndexedDB -> Json.Decode.Value -> msg
-- receive toMsg db value =
--     -- identity function for now
--     -- db
--     toMsg db

    
send : IndexedDB -> Json.Encode.Value
send db =
    -- identity function for now
    Json.Encode.object []

type alias IndexedDbCmdArg = (String, Json.Encode.Value)

findItem : String -> IndexedDbCmdArg
findItem name =
    ( "findItem", Json.Encode.string name )


open : IndexedDbCmdArg
open =
    ( "open", Json.Encode.null )

message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed