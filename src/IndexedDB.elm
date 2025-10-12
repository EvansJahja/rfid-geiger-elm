module IndexedDB exposing (..)
import Json.Decode
import Json.Encode
import Task
import Dict exposing (Dict)
import Item exposing (Item)
import EPC exposing (EPC)

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
        [ ("getItem", GetItem)
        , ("listItemKeywords", ListItemKeywords)
        , ("listItems", ListItems)
        , ("open", Open)
        , ("addItem", AddItem)
        , ("putItem", PutItem)
        , ("deleteItem", DeleteItem)
        , ("deleteDB", DeleteDB)
        , ("getPartialKeywords", GetPartialKeywords)
        ]

type Command = GetItem
             | ListItemKeywords
             | ListItems
             | Open
             | AddItem
             | PutItem
             | DeleteItem
             | DeleteDB
             | GetPartialKeywords
             | Unknown
            
commandFromString : String -> Command
commandFromString str =
    Dict.get str commandStringDict
        |> Maybe.withDefault Unknown
    
commandToString : Command -> String
commandToString cmd =
    Dict.foldl (\k v acc -> if v == cmd then k else acc) "unknown" commandStringDict


type IndexedDBResult = GetItemResult (Maybe Item)
                     | ListItemKeywordsResult KeywordCount
                     | ListItemsResult (List Item)
                     | OpenResult DBStatus
                     | AddItemResult
                     | PutItemResult
                     | DeleteItemResult
                     | GetPartialKeywordsResult KeywordCount
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
                 case baseCmd of
                    "open" ->
                        Ok (OpenResult StatusOpened)
                    "getItem" ->
                        case Json.Decode.decodeValue Item.decoder value of
                            Ok item ->
                                Ok (GetItemResult (Just item))
                            Err _ ->
                                Err (CommandError GetItem "Failed to decode item")
                    "listItemKeywords" ->
                        case Json.Decode.decodeValue keywordCountDecoder value of
                            Ok keywords ->
                                Ok (ListItemKeywordsResult keywords)
                            Err _ ->
                                Err (CommandError ListItemKeywords "Failed to decode keyword counts")
                    "listItems" ->
                        case Json.Decode.decodeValue (Json.Decode.list Item.decoder) value of
                            Ok items ->
                                Ok (ListItemsResult items)
                            Err _ ->
                                Err (CommandError ListItems "Failed to decode item list")
                    "addItem" ->
                        Ok AddItemResult
                    "putItem" ->
                        Ok PutItemResult
                    "deleteItem" ->
                        Ok DeleteItemResult
                    "getPartialKeywords" ->
                        case Json.Decode.decodeValue keywordCountDecoder value of
                            Ok keywordCount ->
                                Ok (GetPartialKeywordsResult keywordCount)
                            Err _ ->
                                Err (CommandError GetPartialKeywords "Failed to decode partial keywords")
                    _ ->
                        Err UnknownError
            _ ->
                UnknownError |> Err -- should not happen


    
send : IndexedDB -> Json.Encode.Value
send db =
    -- identity function for now
    Json.Encode.object []

type alias IndexedDbCmdArg = (String, Json.Encode.Value)

getItem : String -> IndexedDbCmdArg
getItem name =
    ( "getItem", Json.Encode.string name )

deleteItem : EPC -> IndexedDbCmdArg
deleteItem epc =
    ( "deleteItem", Json.Encode.string (EPC.epcStringPrism.reverseGet epc) )

listItemKeywords : IndexedDbCmdArg
listItemKeywords =
    ( "listItemKeywords", Json.Encode.null )

listItems : IndexedDbCmdArg
listItems =
    ( "listItems", Json.Encode.null )

addItem : Item -> IndexedDbCmdArg
addItem item =
    ( "addItem", Item.encoder item )

putItem : Item -> IndexedDbCmdArg
putItem item =
    ( "putItem", Item.encoder item )

open : IndexedDbCmdArg
open =
    ( "open", Json.Encode.null )

deleteDB : IndexedDbCmdArg
deleteDB =
    ( "deleteDB", Json.Encode.null )

getPartialKeywords : String -> IndexedDbCmdArg
getPartialKeywords partial =
    ( "getPartialKeywords", Json.Encode.string partial )

message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed