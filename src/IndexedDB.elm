module IndexedDB exposing (..)
import Json.Decode
import Json.Encode
import Task
import Dict exposing (Dict)

type DBStatus = StatusNone | StatusOpened


type alias CmdMsg msg = (IndexedDB -> Cmd msg)

type alias IndexedDB  = { }

new : IndexedDB
new = { }


type alias Item = { title: String }

itemEncoder : Item -> Json.Encode.Value
itemEncoder item =
    Json.Encode.object 
        [ ("title", Json.Encode.string item.title) 
        ]

itemDecoder : Json.Decode.Decoder Item
itemDecoder =
    Json.Decode.map Item
        (Json.Decode.field "title" Json.Decode.string)


{--
    KeywordCount is a json object with dynamic keys (the keywords) and integer values (the counts).
--} 

type alias KeywordCount = Dict String Int
keywordCountDecoder : Json.Decode.Decoder KeywordCount
keywordCountDecoder =
    Json.Decode.dict Json.Decode.int


type IndexedDBResult = FindItemResult (Maybe Item)
                     | ListItemKeywordsResult KeywordCount
                     | OpenResult DBStatus
                     | UnknownResult

receive : (String, Json.Decode.Value) -> IndexedDBResult
receive (cmd, value) = 
    case cmd of
        "openResult" ->
            OpenResult StatusOpened
        "findItemResult" ->
            case Json.Decode.decodeValue itemDecoder value of
                Ok item ->
                    FindItemResult (Just item)
                Err _ ->
                    FindItemResult Nothing
        "listItemKeywordsResult" ->
            case Json.Decode.decodeValue keywordCountDecoder value of
                Ok keywords ->
                    ListItemKeywordsResult keywords
                Err _ ->
                    ListItemKeywordsResult Dict.empty
        _ -> UnknownResult

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

listItemKeywords : IndexedDbCmdArg
listItemKeywords =
    ( "listItemKeywords", Json.Encode.null )

addItem : Item -> IndexedDbCmdArg
addItem item =
    ( "addItem", itemEncoder item )

open : IndexedDbCmdArg
open =
    ( "open", Json.Encode.null )

message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed