module IndexedDB exposing (..)
import Json.Decode
import Json.Encode
import Task
import Dict exposing (Dict)
import Item exposing (Item)

type DBStatus = StatusNone | StatusOpened


type alias CmdMsg msg = (IndexedDB -> Cmd msg)

type alias IndexedDB  = { }

new : IndexedDB
new = { }

{--
    KeywordCount is a json object with dynamic keys (the keywords) and integer values (the counts).
--} 

type alias KeywordCount = Dict String Int
keywordCountDecoder : Json.Decode.Decoder KeywordCount
keywordCountDecoder =
    Json.Decode.dict Json.Decode.int

commandStringDict : Dict String Command
commandStringDict =
    Dict.fromList
        [ ("findItem", FindItem)
        , ("listItemKeywords", ListItemKeywords)
        , ("listItems", ListItems)
        , ("open", Open)
        , ("addItem", AddItem)
        , ("deleteDB", DeleteDB)
        ]

type Command = FindItem
             | ListItemKeywords
             | ListItems
             | Open
             | AddItem
             | DeleteDB
             | Unknown
            
commandFromString : String -> Command
commandFromString str =
    Dict.get str commandStringDict
        |> Maybe.withDefault Unknown
    
commandToString : Command -> String
commandToString cmd =
    Dict.foldl (\k v acc -> if v == cmd then k else acc) "unknown" commandStringDict


type IndexedDBResult = FindItemResult (Maybe Item)
                     | ListItemKeywordsResult KeywordCount
                     | ListItemsResult (List Item)
                     | OpenResult DBStatus
                     | AddItemResult
                     | UnknownResult
type IndexedDBError = UnknownError
                    | StatusError String
                    | CommandError Command String

receive : (String, Json.Decode.Value) -> Result IndexedDBError IndexedDBResult
receive (cmd, value) = 
    -- cmd looks something like "openResult", "findItemResult", "findItemError", "addItemError"
    -- so it will always end with "Result" or "Error"
    -- let's first split it by "Result" or "Error"
    let
        ( baseCmd, status ) =
            if String.endsWith "Result" cmd then
                ( String.dropRight 6 cmd, "Result" )
            else if String.endsWith "Error" cmd then
                ( String.dropRight 5 cmd, "Error" )
            else
                ( cmd, "Unknown" )
    in
        case status of
            "Error" ->
                Err <| CommandError (commandFromString baseCmd) (Json.Decode.decodeValue Json.Decode.string value |> Result.withDefault "Unknown error")
            "Result" ->
                Ok <| case baseCmd of
                    "open" ->
                        OpenResult StatusOpened
                    "findItem" ->
                        case Json.Decode.decodeValue Item.decoder value of
                            Ok item ->
                                FindItemResult (Just item)
                            Err _ ->
                                FindItemResult Nothing
                    "listItemKeywords" ->
                        case Json.Decode.decodeValue keywordCountDecoder value of
                            Ok keywords ->
                                ListItemKeywordsResult keywords
                            Err _ ->
                                ListItemKeywordsResult Dict.empty
                    "listItems" ->
                        case Json.Decode.decodeValue (Json.Decode.list Item.decoder) value of
                            Ok items ->
                                ListItemsResult items
                            Err _ ->
                                ListItemsResult []
                    "addItem" ->
                        AddItemResult
                    _ ->
                        UnknownResult
            _ ->
                UnknownError |> Err -- should not happen


    
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

listItems : IndexedDbCmdArg
listItems =
    ( "listItems", Json.Encode.null )

addItem : Item -> IndexedDbCmdArg
addItem item =
    ( "addItem", Item.encoder item )

open : IndexedDbCmdArg
open =
    ( "open", Json.Encode.null )

deleteDB : IndexedDbCmdArg
deleteDB =
    ( "deleteDB", Json.Encode.null )

message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed