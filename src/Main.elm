port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Parser as UrlParser exposing (Parser, (</>), int, oneOf, s, string)
import Html exposing (Html, button, div, input, text, select, option, h1, ul, li, label, p)
import Html.Events exposing (onClick, onInput, onCheck, on)
import Html.Attributes exposing (..)
import Html.Attributes as Attrs
import Svg exposing (svg, circle, line, polyline, path, rect)
import Svg.Attributes as SvgAttrs
import Json.Decode
import Json.Encode
import VH88.WorkingParameters exposing (WorkingParameters)
import VH88.Command exposing (Command(..))
import VH88.Packet exposing (Packet(..))
import VH88.Error exposing (Error(..), ErrorCode(..), errorCodeFromInt, errorCodeToString)
import VH88
import Device exposing (Device, encodeDevice)
import Fifo exposing (Fifo)
import Time
import Process
import Task
import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as Decode
import VH88.Command as Command
import VH88.Packet as Packet
import Platform.Cmd as Cmd
import BytesHelper
import VH88.WorkingParameters as WorkingParameters
import Hex
import Html exposing (span)
import IndexedDB exposing (IndexedDB)
import IndexedDB exposing (KeywordCount)
import Item
import Html.Events exposing (onBlur)
import IndexedDB exposing (IndexedDBError)

-- STATE MANAGEMENT TYPES 

type alias PendingCommand = Dict String PendingCommandItem
type alias PendingCommandItem =
    { command: Command
    , sentTime: Time.Posix
    }

newPendingCommand : PendingCommand
newPendingCommand = Dict.empty

addPendingCommand :  PendingCommand -> Time.Posix -> Command -> PendingCommand
addPendingCommand dict time cmd =
    let
        key = Debug.toString cmd
    in
    Dict.insert key { command = cmd, sentTime = time } dict

removePendingCommand : PendingCommand -> Command -> PendingCommand
removePendingCommand dict cmd =
    let
        key = Debug.toString cmd
    in
    Dict.remove key dict

type alias ErrorCommand = Dict String PendingErrorItem
type alias PendingErrorItem =
    { command: Command
    , sentTime: Time.Posix
    , errorCode: ErrorCode
    }

newErrorCommand : ErrorCommand
newErrorCommand = Dict.empty

addErrorCommand :  ErrorCommand -> Time.Posix -> Command -> ErrorCode -> ErrorCommand
addErrorCommand dict time cmd errCode =
    let
        key = Debug.toString cmd
    in
    Dict.insert key { command = cmd, sentTime = time, errorCode = errCode } dict

removeErrorCommand : ErrorCommand -> Command -> ErrorCommand
removeErrorCommand dict cmd =
    let
        key = Debug.toString cmd
    in
    Dict.remove key dict

type alias EPC = (List Int)
epcToString : EPC -> String
epcToString nums =
    nums
        |> List.map (Hex.toString >> String.pad 2 '0')
        |> String.join ""

type alias InventoryItem =
    { epc : EPC
    , lastSeen : Time.Posix
    }

type alias Inventory = Dict EPC InventoryItem

-- ROUTING

routeParser : Parser (Page -> a) a
routeParser =
  oneOf
    [ UrlParser.map PageCategories   (s "categories")
    , UrlParser.map PageItems       (s "items")
    , UrlParser.map PageLocation    (s "location")
    , UrlParser.map PageSettings    (s "settings")
    ]



type Page = PageCategories
          | PageItems
          | PageLocation
          | PageSettings
          | PageTODO

pageSpecificCmds : Page -> List (Cmd Msg)
pageSpecificCmds page =
    case page of
        PageCategories ->
            [ indexedDbCmd (IndexedDB.listItemKeywords) ]
        PageItems ->
            [ indexedDbCmd (IndexedDB.listItems) ]
        _ ->
            []

-- MODEL

type alias Model =
    { key: Nav.Key
    , url : Url.Url
    , time : Time.Posix
    , receivedData : String
    , textToSend : String
    , deviceList : List Device
    , counter : Int
    , selectedDevice : Maybe Device
    , recvBuffer : Fifo.Fifo Int -- FIFO for incoming serial data
    , pendingCommand: PendingCommand
    , errorCommand : ErrorCommand
    , showDebug : Bool
    , powerLevel : Int
    , workingParameters : Maybe WorkingParameters
    , editWorkingParameters : Maybe WorkingParameters
    , serialStatus : SerialStatus
    , platform : String
    , inventory : Inventory
    , epcFilter : Set EPC
    , latestEPC : Maybe EPC
    , indexedDBStatus : IndexedDB.DBStatus
    , itemKeywordCounts : Maybe KeywordCount
    , itemForm : Item.Form
    , itemFormValidationErrors : Item.ValidationErrors
    , items : Maybe (List Item.Item)

    , page : Page
    }

type alias DataUrl = String

init : (Json.Decode.Value) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        platform = Decode.decodeValue (Decode.field "platform" Decode.string) flags |> Result.withDefault "unknown"
        page = UrlParser.parse routeParser url |> Maybe.withDefault (PageSettings)
        initCmd = case platform of
            "web" ->
                Cmd.batch
                (
                    [ registerListener ()
                    -- , requestDeviceList () -- don't request device list automatically on web, need user gesture
                    , Time.now |> Task.perform Tick
                    , indexedDbCmd IndexedDB.open
                    ]
                    ++ pageSpecificCmds page
                )
            _ -> 
                Cmd.batch
                (
                    [ registerListener ()
                    , requestDeviceList ()
                    , Time.now |> Task.perform Tick
                    , indexedDbCmd IndexedDB.open
                    ] 
                )
        
    in
        (   { key = key
            , url = url
            , time = Time.millisToPosix 0
            , receivedData = "No data yet."
            , textToSend = ""
            , deviceList = []
            , counter = 0
            , selectedDevice = Nothing
            , recvBuffer = Fifo.empty
            , pendingCommand = newPendingCommand
            , errorCommand = newErrorCommand
            , showDebug = False
            , powerLevel = 0
            , workingParameters = Nothing
            , editWorkingParameters = Nothing
            , serialStatus = SerialNothing
            , platform = platform
            , inventory = Dict.empty
            , epcFilter = Set.empty
            , latestEPC = Nothing
            , indexedDBStatus = IndexedDB.StatusNone
            , itemKeywordCounts = Nothing
            , itemForm = Item.defaultForm
            , itemFormValidationErrors = Item.emptyValidationErrors
            , items = Nothing
            , page = page
            }
        , initCmd
        )

-- MESSAGES


type Msg
    = ReceiveData (List Int)
    | ReceivePacket Packet
    | ReceiveResponse VH88.Command.CommandWithArgs
    | ReceiveSerialStatus (Maybe SerialStatus)
    | ReceiveDeviceList (List Device)
    | RequestDeviceList
    | SetEditWorkingParameters (Maybe WorkingParameters)
    | UpdateWorkingParameters WorkingParameters
    | SendCommand Packet 
    | IncrementCounter
    | DebugCmd String
    | DebugToggle Bool
    | DeviceSelected (Maybe Device)
    | Connect Device
    | Tick Time.Posix
    | SetPowerLevel Int
    | ClearInventory
    | EPCFilter EPCFilterOperation
    | TakePicture
    | PictureResult DataUrl
    | FindItem String
    | ListItemKeywords
    | IndexedDBResult (Result IndexedDB.IndexedDBError IndexedDB.IndexedDBResult)
    | IndexedDBCommand IndexedDB.IndexedDbCmdArg 
    | PageChange Page (List (Cmd Msg))
    | AddItemForm ItemFormField
    | AddItemFormSubmit 
    | AddItemFormOnBlur String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

type ItemFormField = ItemFormTitle String

type EPCFilterOperation = Add EPC | Remove EPC

type SerialStatus
    = SerialNothing
    | SerialWaitingForUser
    | SerialConnected
    | SerialConnecting
    | SerialError String
serialStatusToString : SerialStatus -> String
serialStatusToString status =
    case status of
        SerialNothing ->
            "No Status"

        SerialWaitingForUser ->
            "Waiting for user to select device"

        SerialConnected ->
            "Connected"

        SerialConnecting ->
            "Connecting..."

        SerialError msg ->
            "Error: " ++ msg

-- HELPER FUNCTIONS

{-| Recursively parse all complete packets from the buffer
Returns (list of packets, remaining buffer)
-}
parseAllPackets : Fifo Int -> List Packet -> (List Packet, Fifo Int)
parseAllPackets buffer accumulator =
    case VH88.fifoBytesToPacket buffer of
        (Ok packet, remainingBuffer) ->
            -- Found a packet, continue parsing
            parseAllPackets remainingBuffer (packet :: accumulator)
        
        (Err _, finalBuffer) ->
            -- No more complete packets, return what we found (in correct order)
            (List.reverse accumulator, finalBuffer)

{-| Process all packets by calling update with ReceivePacket for each one
-}
processAllPackets : List Packet -> Model -> (Model, Cmd Msg)
processAllPackets packets model =
    case packets of
        [] ->
            (model, Cmd.none)
        bunchOfPackets ->
            let
                batchProcessPackets = bunchOfPackets |> List.map (\packet -> message (ReceivePacket packet))
            in
                (model, Cmd.batch batchProcessPackets)


decodeSerialStatus : List String -> Maybe SerialStatus
decodeSerialStatus list =
    case list of
        ["serial_waiting_for_user"] ->
            Just SerialWaitingForUser
        
        ["serial_connected"] ->
            Just SerialConnected
        
        ["serial_connecting"] ->
            Just SerialConnecting

        ["serial_error", errorMessage] ->
            Just (SerialError errorMessage)
        
        _ ->
            Nothing

-- PORTS

port requestDeviceList : () -> Cmd msg
port debugPort : String -> Cmd msg
port deviceConnect : (Json.Encode.Value) -> Cmd msg
port registerListener : () -> Cmd msg

port serialSend : (List Int) -> Cmd msg

port serialData : (List Int -> msg) -> Sub msg

port serialStatus : (List String -> msg) -> Sub msg

port deviceList : (Json.Decode.Value -> msg) -> Sub msg

port takePicture : () -> Cmd msg

port pictureResult : (String -> msg) -> Sub msg

port indexedDbCmd : (String , Json.Encode.Value) -> Cmd msg
port indexedDbSub : ((String, Json.Decode.Value) -> msg) -> Sub msg


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveData byteList ->
            let
                appendedBuffer = List.foldl Fifo.insert model.recvBuffer byteList
                (packets, finalBuffer) = parseAllPackets appendedBuffer []

                recvPacketCmds = packets |> List.map (\packet -> message (ReceivePacket packet))
            in
                ( { model | recvBuffer = finalBuffer }, Cmd.batch recvPacketCmds )


        ReceivePacket packet ->
            let
                (updatedModel, newCmd) = case packet of
                        
                    Response (Command.CommandWithArgs (cmd, args)) ->
                        ( { model 
                        | pendingCommand = removePendingCommand model.pendingCommand cmd 
                        , errorCommand = removeErrorCommand model.errorCommand cmd -- remove from error as well
                        }, message (ReceiveResponse (Command.CommandWithArgs (cmd, args))) )
                        
                    
                    Error (Command.CommandWithArgs (cmd, params)) ->
                        let
                            errorCode = List.head params |> Maybe.withDefault 0x20 |> errorCodeFromInt
                        in
                            (
                                { model 
                                | pendingCommand = removePendingCommand model.pendingCommand cmd 
                                , errorCommand = addErrorCommand model.errorCommand model.time cmd errorCode
                                }
                                , Cmd.none
                            )
                    Status _ ->
                        (model, Cmd.none)  -- Don't remove pending commands for status packets

                    Request _ ->
                        -- Requests are outgoing, we shouldn't receive them
                        (model, Cmd.none)
            in
                ( updatedModel , newCmd )

        ReceiveResponse response ->
            case response of
                VH88.Command.CommandWithArgs (VH88.Command.ReadWorkingParameters, args) ->
                    let
                        decoder = VH88.WorkingParameters.toDecoder
                        maybeWorkingParams = BytesHelper.decodeListInt decoder args
                    in
                        ( { model | workingParameters = maybeWorkingParams, receivedData = "Working parameters updated." }, Cmd.none )
                VH88.Command.CommandWithArgs (VH88.Command.HostComputerCardReading, args) ->
                    let
                        epc : EPC
                        epc = args


                        itemForm =
                            let 
                                modelItemForm = model.itemForm
                            in
                                if
                                    model.page == PageItems || model.showDebug
                                then
                                    { modelItemForm | epc = epcToString epc}
                                else
                                    modelItemForm

                        -- we may be filtering this epc
                        filteredEpc = (Set.isEmpty model.epcFilter) || (Set.member epc model.epcFilter)
                        inventoryItem = { epc = epc, lastSeen = model.time }
                        newInventory = 
                            if filteredEpc then
                                Dict.insert epc inventoryItem model.inventory
                            else
                                model.inventory
                        
                    in
                        ( { model | inventory = newInventory, latestEPC = Just epc, itemForm = itemForm }, Cmd.none)
                VH88.Command.CommandWithArgs (unk, args) ->
                    ( { model | receivedData = "Received unknown response: " ++ (Debug.toString (unk, args)) }, Cmd.none )
        
        RequestDeviceList ->
            ( model, requestDeviceList () )
        ReceiveDeviceList devices ->
            let
                firstDeviceCmd =
                    case devices of
                        firstDevice :: _ ->
                            Just firstDevice
                        [] ->
                            Nothing
            in
            ( { model | deviceList = devices , selectedDevice = firstDeviceCmd }, Cmd.none )

        SetEditWorkingParameters maybeParams ->
            ( { model | editWorkingParameters = maybeParams }, Cmd.none )
        UpdateWorkingParameters params ->
            ( { model | workingParameters = Just params, editWorkingParameters = Nothing}, sendPacket (VH88.setWorkingParameters params) )
        ReceiveSerialStatus maybeStatus ->
            case maybeStatus of
                Just status ->
                    let
                        newModel = { model | serialStatus = status }
                        cmd =
                            if status == SerialConnected then
                                sendPacket VH88.readWorkingParameters
                            else
                                Cmd.none
                    in
                        (newModel, cmd)
                
                Nothing ->
                    ( { model | receivedData = "Status: Unknown status received." }, Cmd.none )
        IncrementCounter ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        DeviceSelected device ->
            ( { model | selectedDevice = device }, Cmd.none )
        Connect device ->
            ( model, Cmd.batch [deviceConnect (encodeDevice device), registerListener ()] )
        DebugCmd debugMsg ->
            (model, debugPort debugMsg)
        DebugToggle isChecked ->
            ( { model | showDebug = isChecked }, Cmd.none )
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none)
        SendCommand packet ->
            let
                serialSendCmd = sendPacket packet
                (cmd, _) = Packet.packetContents packet
                pendingCommand = addPendingCommandToModel model cmd

            in
                ( { model | pendingCommand = pendingCommand }, serialSendCmd)

        SetPowerLevel powerLevel ->
            ( { model | powerLevel = powerLevel }, Cmd.none )
        ClearInventory ->
            ( { model | inventory = Dict.empty }, Cmd.none )
        EPCFilter operation ->
            case operation of
                Add epc ->
                    ( { model | epcFilter = Set.insert epc model.epcFilter }, Cmd.none )
                Remove epc ->
                    ( { model | epcFilter = Set.remove epc model.epcFilter }, Cmd.none )
        TakePicture ->
            ( model, takePicture () )
        PictureResult dataUrl ->
            ( { model | receivedData = "Received picture data URL of length: " ++ String.fromInt (String.length dataUrl) }, Cmd.none )
        PageChange newPage cmds->
            ( { model | page = newPage }, Cmd.batch cmds )
        FindItem name -> 
            (model, indexedDbCmd (IndexedDB.findItem name))
        ListItemKeywords ->
            (model, indexedDbCmd (IndexedDB.listItemKeywords))
        IndexedDBCommand cmdArg ->
            (model, indexedDbCmd cmdArg)
        IndexedDBResult result ->
            case result of
                Err (IndexedDB.StatusError err) ->
                    ( { model | receivedData = "IndexedDB error: " ++ err }, Cmd.none )
                Err (IndexedDB.CommandError IndexedDB.AddItem err) ->
                    let
                        modelItemFormValidationErrors = model.itemFormValidationErrors
                        itemFormValidationErrors = { modelItemFormValidationErrors | formErrors = ["IndexedDB error: " ++ err] }
                    in
                        ( { model | itemFormValidationErrors = itemFormValidationErrors }, Cmd.none )
                    
                Err (IndexedDB.CommandError cmd err) ->
                    ( { model | receivedData = "IndexedDB command " ++ IndexedDB.commandToString cmd ++ " error: " ++ err }, Cmd.none )
                Err _ -> 
                    ( { model | receivedData = "Unhandled IndexedDB error." }, Cmd.none )
                Ok res ->
                    case res of
                        IndexedDB.OpenResult status ->
                            ( { model | indexedDBStatus = status }, Cmd.none )
                        IndexedDB.FindItemResult maybeItem ->
                            case maybeItem of
                                Just item ->
                                    ( { model | receivedData = "Found item: " ++ item.title }, Cmd.none )
                                Nothing ->
                                    ( { model | receivedData = "Item not found." }, Cmd.none )
                        IndexedDB.ListItemKeywordsResult keywordCounts ->
                            let
                                keywordsString = 
                                    keywordCounts
                                        |> Dict.toList
                                        |> List.map (\(keyword, count) -> keyword ++ " (" ++ String.fromInt count ++ ")")
                                        |> String.join ", "
                            in
                                ( { model | itemKeywordCounts = Just keywordCounts, receivedData = "Keywords: " ++ keywordsString }, Cmd.none )
                        IndexedDB.ListItemsResult items ->
                            ( { model | items = Just (Debug.log "Found items: " items) }, Cmd.none )
                        
                        IndexedDB.AddItemResult ->
                            ( { model | itemFormValidationErrors = Item.emptyValidationErrors }, indexedDbCmd IndexedDB.listItems )

                        IndexedDB.UnknownResult ->
                            ( { model | receivedData = "Received unknown IndexedDB result." }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Debug.log "Navigating to internal URL" (Url.toString url)) )

                Browser.External href ->
                    ( model, Nav.load (Debug.log "Navigating to external URL" href) )

        UrlChanged url ->
            let
                newPage = UrlParser.parse routeParser url |> Maybe.withDefault (PageSettings)
                
            in
            
            ( { model | url = (Debug.log "URL changed" url), page = newPage }
            , message (PageChange newPage (pageSpecificCmds newPage))
            )
        AddItemForm (ItemFormTitle title) ->
            let
                itemForm = model.itemForm

            in
            ( { model | itemForm = { itemForm | title = title } }, Cmd.none )
        AddItemFormSubmit ->
            let
                itemForm = model.itemForm

                cmdMaybeAddItem = 
                    case Item.decodeForm itemForm of
                        Ok item ->
                            indexedDbCmd (IndexedDB.addItem item)
                        Err _ ->
                            Cmd.none

                
            in
                (model, cmdMaybeAddItem)
        AddItemFormOnBlur fieldName ->
            let
                itemForm = model.itemForm
                modelItemFormValidationErrors = model.itemFormValidationErrors

                itemFormValidationErrors = 
                    case fieldName of
                        "title" ->
                            { modelItemFormValidationErrors
                                | title = Item.itemFormError .title itemForm.title
                            }
                        _ ->
                            modelItemFormValidationErrors
    

            in
                ( { model | itemFormValidationErrors = itemFormValidationErrors }, Cmd.none )


addPendingCommandToModel : Model -> Command -> PendingCommand
addPendingCommandToModel model cmd =
    addPendingCommand model.pendingCommand model.time cmd

sendPacket : Packet -> Cmd Msg
sendPacket packet =
    packet |> VH88.Packet.packetToBytes |> serialSend

-- Tasks and Cmd helper
-- See https://discourse.elm-lang.org/t/when-does-elm-not-update-the-ui/901/4

message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed


delay : Float -> msg -> Cmd msg
delay sleepMs msg =
    Process.sleep sleepMs
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
        

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 500 Tick
        , serialData ReceiveData
        , serialStatus (\rawList ->
            ReceiveSerialStatus (decodeSerialStatus rawList)
          )
        , deviceList (\json -> 
            case Json.Decode.decodeValue Device.decodeDeviceList json of
                Ok devices -> 
                    ReceiveDeviceList devices
                Err _ ->
                    Debug.todo "Implement error handling for device list decoding"
          )
        , pictureResult PictureResult
        , indexedDbSub (IndexedDB.receive >> IndexedDBResult)

        ]

-- VIEW

{--
<div class="dock">
  <button>
    <svg class="size-[1.2em]" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><g fill="currentColor" stroke-linejoin="miter" stroke-linecap="butt"><polyline points="1 11 12 2 23 11" fill="none" stroke="currentColor" stroke-miterlimit="10" stroke-width="2"></polyline><path d="m5,13v7c0,1.105.895,2,2,2h10c1.105,0,2-.895,2-2v-7" fill="none" stroke="currentColor" stroke-linecap="square" stroke-miterlimit="10" stroke-width="2"></path><line x1="12" y1="22" x2="12" y2="18" fill="none" stroke="currentColor" stroke-linecap="square" stroke-miterlimit="10" stroke-width="2"></line></g></svg>
    <span class="dock-label">Home</span>
  </button>
  
  <button class="dock-active">
    <svg class="size-[1.2em]" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><g fill="currentColor" stroke-linejoin="miter" stroke-linecap="butt"><polyline points="3 14 9 14 9 17 15 17 15 14 21 14" fill="none" stroke="currentColor" stroke-miterlimit="10" stroke-width="2"></polyline><rect x="3" y="3" width="18" height="18" rx="2" ry="2" fill="none" stroke="currentColor" stroke-linecap="square" stroke-miterlimit="10" stroke-width="2"></rect></g></svg>
    <span class="dock-label">Inbox</span>
  </button>
  
  <button>
    <svg class="size-[1.2em]" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><g fill="currentColor" stroke-linejoin="miter" stroke-linecap="butt"><circle cx="12" cy="12" r="3" fill="none" stroke="currentColor" stroke-linecap="square" stroke-miterlimit="10" stroke-width="2"></circle><path d="m22,13.25v-2.5l-2.318-.966c-.167-.581-.395-1.135-.682-1.654l.954-2.318-1.768-1.768-2.318.954c-.518-.287-1.073-.515-1.654-.682l-.966-2.318h-2.5l-.966,2.318c-.581.167-1.135.395-1.654.682l-2.318-.954-1.768,1.768.954,2.318c-.287.518-.515,1.073-.682,1.654l-2.318.966v2.5l2.318.966c.167.581.395,1.135.682,1.654l-.954,2.318,1.768,1.768,2.318-.954c.518.287,1.073.515,1.654.682l.966,2.318h2.5l.966-2.318c.581-.167,1.135-.395,1.654-.682l2.318.954,1.768-1.768-.954-2.318c.287-.518.515-1.073.682-1.654l2.318-.966Z" fill="none" stroke="currentColor" stroke-linecap="square" stroke-miterlimit="10" stroke-width="2"></path></g></svg>
    <span class="dock-label">Settings</span>
  </button>
</div>

--}

viewDock : Model -> Html Msg
viewDock model = 
    div [ class "dock " ]
        [ button [ ]
            [   svg [ SvgAttrs.class "size-[1.2em]",  SvgAttrs.viewBox "0 0 24 24" ]
                [ Svg.polyline [ SvgAttrs.points "1 11 12 2 23 11", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.path [ SvgAttrs.d "m5,13v7c0,1.105.895,2,2,2h10c1.105,0,2-.895,2-2v-7", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.line [ SvgAttrs.x1 "12", SvgAttrs.y1 "22", SvgAttrs.x2 "12", SvgAttrs.y2 "18", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                ]
            ,   span [ class "dock-label" ] [ text "Home" ]
            ]
        , button [ class "dock-active" ]
            [ svg [ SvgAttrs.class "size-[1.2em]",  SvgAttrs.viewBox "0 0 24 24" ]
                [ Svg.polyline [ SvgAttrs.points "3 14 9 14 9 17 15 17 15 14 21 14", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.rect [ SvgAttrs.x "3", SvgAttrs.y "3", SvgAttrs.width "18", SvgAttrs.height "18", SvgAttrs.rx "2", SvgAttrs.ry "2", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                ]
            , span [ class "dock-label" ] [ text "Inbox" ]
            ]
        , button []
            [   svg [ SvgAttrs.class "size-[1.2em]",  SvgAttrs.viewBox "0 0 24 24" ]
                [ Svg.circle [ SvgAttrs.cx "12", SvgAttrs.cy "12", SvgAttrs.r "3", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.path [ SvgAttrs.d "m22,13.25v-2.5l-2.318-.966c-.167-.581-.395-1.135-.682-1.654l.954-2.318-1.768-1.768-2.318.954c-.518-.287-1.073-.515-1.654-.682l-.966-2.318h-2.5l-.966,2.318c-.581.167-1.135.395-1.654.682l-2.318-.954-1.768,1.768.954,2.318c-.287.518-.515,1.073-.682,1.654l-2.318.966v2.5l2.318.966c.167.581.395,1.135.682,1.654l-.954,2.318,1.768,1.768,2.318-.954c.518.287,1.073.515,1.654.682l.966,2.318h2.5l.966-2.318c.581-.167,1.135-.395,1.654-.682l2.318.954,1.768-1.768-.954-2.318c.287-.518.515-1.073+.682-1.654l2.318-.966Z", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                ]
            ,   span [ class "dock-label" ] [ text "Settings" ]
            ]
        ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    let
        isActive page =
            if model.page == page then
                "tab-active "
            else
                ""


        link : String -> Page -> String -> List (Cmd Msg) -> Html Msg
        link href page label cmds =
            -- Html.a [ Attrs.attribute "role" "tab", Attrs.href label , class (isActive page ++ "text-xs tab "), onClick (PageChange page cmds) ] [ text label ]
            Html.a [ Attrs.attribute "role" "tab", Attrs.href href , class (isActive page ++ "text-xs tab ")] [ text label ]
    in
    
    div [ Attrs.attribute "role" "tablist",  class " tabs tabs-border shadow-sm justify-center pb-8" ]
        [ link "/categories" PageCategories "Categories" [ message ListItemKeywords]
        , link "/items" PageItems "Items" []
        , link "/location" PageLocation "Location" []
        , link "/settings" PageSettings "Settings" []
        , Html.a [ Attrs.attribute "role" "tab", class (isActive PageTODO ++ "text-xs tab") ] [ text "Ownership" ]
        , Html.a [ Attrs.attribute "role" "tab", class (isActive PageTODO ++ "text-xs tab") ] [ text "All" ]
        ]


listItem : String -> String -> Html Msg
listItem name subtitle =
    li [ class "list-row" ]
        [ div [] [ Html.img [ class "size-10 rounded-box", src "https://img.daisyui.com/images/profile/demo/1@94.webp" ] [] ]
        , div [ class "flex justify-between"]
            [ div [] [ text name ]
            , div [ class "text-xs uppercase font-semibold opacity-60" ] [ text subtitle ]
            ]
        ]

viewItemList : Model -> List Item.Item -> Html Msg
viewItemList model items = 
    let
        listItems =
            items
                |> List.map (\item -> listItem item.title "blah")
    in
        ul [ class "list bg-base-100 rounded-box shadow-md" ]
            listItems

viewKeywordCountList : Model -> KeywordCount -> Html Msg
viewKeywordCountList model keywordCounts = 
    let
        listItems =
            keywordCounts
                |> Dict.toList
                |> List.map (\(keyword, count) -> listItem keyword (String.fromInt count))
    in
        ul [ class "list bg-base-100 rounded-box shadow-md" ]
            listItems
        

viewHeading : Html Msg
viewHeading =
    div
        [ class "flex justify-center w-full bg-[url('overlay.jpg')] bg-cover bg-center bg-clip-text" ] -- Utility class to center content horizontally
        [ h1
            [ class "text-4xl text-neutral font-extrabold text-base-content py-2  text-transparent" ] -- Stylize the h1
            [ text "Inventory." ]
        ]

viewDoc : Model -> Browser.Document Msg
viewDoc model =
    { title = "Inventory"
    , body = [view model]
    }

view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-base-200 text-base-content" ]
    [  case model.page of
        PageCategories ->
            pageCategories model
        PageLocation ->
            pageLocation model
        PageSettings ->
            pageSettings model
        PageItems ->
            pageItems model
        rest  ->  Debug.todo ("Implement other pages: " ++ (Debug.toString rest))
    ]

pageCategories : Model -> Html Msg
pageCategories model =
    let
        maybeKeywordCounts = model.itemKeywordCounts
        panelContents =
            case maybeKeywordCounts of
                Just keywordCounts ->
                    if Dict.isEmpty keywordCounts then
                        p [] [ text "No keywords found." ]
                    else
                        viewKeywordCountList model keywordCounts
                Nothing ->
                    p [] [ text "Loading..." ]
    in
    
    div[ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar model
        , viewPanel ""
            [ button [ class "btn", onClick (FindItem "Yarn")] [ text "Search"]
            , button [ class "btn", onClick (ListItemKeywords)] [ text "Get keywords" ]
            , panelContents
            ]
        ]


pageLocation : Model -> Html Msg
pageLocation model =
    div[ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar model
        , p [] [ text "Location page content goes here." ]
        ]

pageSettings : Model -> Html Msg
pageSettings model =
    div[ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar model
        , view2 model
        ]
    
pageItems : Model -> Html Msg
pageItems model =
    let
        maybeItems = model.items
        panelItems =
            case maybeItems of
                Just items ->
                    if List.isEmpty items then
                        p [] [ text "No items found." ]
                    else
                        viewItemList model items
                Nothing ->
                    p [] [ text "Loading..." ]
        titleErrors =
            model.itemFormValidationErrors.title
                |> List.map (\err -> div [ class "text-sm text-warning" ] [ text err ])
                |> Html.div []
        formErrors =
            model.itemFormValidationErrors.formErrors
                |> List.map (\err -> div [ class "text-sm text-error" ] [ text err ])
                |> Html.div []

    in
        div[ class "flex flex-col" ]
            [ viewHeading
            , viewNavbar model
            , viewPanel "Add Item"
                [ input [ placeholder "Enter item name..." , class "input input-bordered w-full max-w-xs", onInput (\title -> AddItemForm (ItemFormTitle title)), onBlur (AddItemFormOnBlur "title"), value model.itemForm.title ] []
                , titleErrors

                , label [ for "scanned-epc" ] [ text "Scanned EPC: "]
                , input [ id "scanned-epc", class "input", disabled True, value model.itemForm.epc ] []
                -- , epcErrors -- shouldn't be any errors here since it's read-only

                , formErrors
                , button [ class "btn btn-secondary", onClick AddItemFormSubmit ] [ text "Add Item" ]
                ]
            , panelItems
            ]


view2 model =
    div [class "bg-base flex flex-col p-4  gap-2"]
        [ viewPanel ""
            [ p [] [text ("Platform: " ++ model.platform)]
            , button [class "btn btn-primary"] [ text "DaisyUI Button"]
            , div [] [button [ onClick TakePicture ] [ text "Take picture" ]]
            , div [] [button [ onClick RequestDeviceList ] [ text "Connect to Serial Port" ]]
            , div [] [ input [ placeholder "Enter text..." ] [] ]
            , div [] [ text <| "Received: " ++ model.receivedData ]
            , div [] [ text <| "Counter: " ++ String.fromInt (model.counter) ]
            , div [] [button [ onClick IncrementCounter] [text "Increment Counter"]]
            , div []
                [ input [ id "debug-checkbox", type_ "checkbox", onCheck (DebugToggle), class "w-4 h-4 text-blue-600 bg-gray-100 border-gray-300 rounded-sm focus:ring-blue-500 dark:focus:ring-blue-600 dark:ring-offset-gray-800 focus:ring-2 dark:bg-gray-700 dark:border-gray-600"] []
                , label [ for "debug-checkbox", class "ms-2 text-sm font-medium text-gray-900 dark:text-gray-300"] [ text "Enable Debugging" ]]
            ]
        , htmlIf model.showDebug (viewDebugCmd model) (div [][])
        , viewPanelConnection model
        , viewPanelRfidPower model
        , case model.editWorkingParameters of
            Just ewp ->
                viewPanelWorkingParameter ewp
            Nothing ->
                button [onClick (SetEditWorkingParameters model.workingParameters), class "btn btn-secondary"] [ text "Edit Working Parameters"]
        , viewPanelFilter model
        , viewPanelInventory model
        ]

viewPanel : String -> (List (Html msg)) -> Html msg
viewPanel title contents = 
    div [class "p-4 border-base-300 rounded bg-base-100"]
    [ h1 [] [text title]
    , div [class "flex flex-col gap-2"] contents
    ]

viewPanelConnection : Model -> Html Msg
viewPanelConnection model =
        let
            attr =
                disabled (model.selectedDevice == Nothing)
                    :: (case model.selectedDevice of
                            Just device ->
                                [ onClick (Connect device), class "btn btn-primary" ]

                            Nothing ->
                                [ class "btn btn-primary" ]
                    )
        in
            viewPanel "Connection"
            [ p [] [text ("Connection status: " ++ serialStatusToString model.serialStatus)]
            , htmlIf (List.isEmpty model.deviceList)
                            (Html.h1 [class "font-bold text-warning"] [ text "No Bluetooth Devices Found" ])
                            (div [] 
                                [text "Available Bluetooth Devices:"
                                , select [onInput ((Device.deviceByAddr model.deviceList >> DeviceSelected) )]
                                    (List.map (\device -> option [value device.address] [ text device.name ]) model.deviceList)
                                ]
                            )
            , button attr [ text "Connect" ]
            ]
viewPanelRfidPower : Model -> Html Msg
viewPanelRfidPower model =
    let
        powerLevelCommand = (VH88.setRFIDPower model.powerLevel)
                          |> Result.toMaybe
                          |> Maybe.map SendCommand

        onpointerup = 
            case powerLevelCommand of
                Just cmd -> [on "pointerup" (Decode.succeed cmd)]
                Nothing -> []

    in
        viewPanel ("Power level: " ++ (String.fromInt model.powerLevel))
        [ input (onpointerup ++ [type_ "range", Attrs.min "0", Attrs.max "33", Attrs.value (String.fromInt model.powerLevel), onInput (SetPowerLevel << Maybe.withDefault 0 << String.toInt)]) []

        ]

htmlIf : Bool -> Html msg -> Html msg -> Html msg
htmlIf condition htmlTrue htmlFalse  =
    if condition then
        htmlTrue
    else
        htmlFalse

viewDebugCmd : Model -> Html Msg
viewDebugCmd model =
    div [] 
    [ viewPanel "Debug Commands"
        [ button [ class "btn", onClick (ReceiveDeviceList [Device "Device A" "00:00:00:00:00:01", Device "Device B" "00:00:00:00:00:02"]) ] [text "Debug get device list"]
        , button [ class "btn", onClick (ReceiveData [0xf0, 0x03, 0x04, 0x00, 0x09] ) ] [text "Receive successful cmd 0x4"]
        , button [ class "btn", onClick (ReceiveData [0xf4, 0x03, 0x04, 0x11, 0xf4] ) ] [text "Receive error cmd 0x4"]
        , button [ class "btn", onClick (SendCommand VH88.startListingTags) ] [text "Debug start listing tags"]
        , button [ class "btn", onClick (ReceiveResponse (VH88.Command.CommandWithArgs (VH88.Command.HostComputerCardReading, [0xe2, 0x0, 0x47, 0x18, 0xb8, 0x30, 0x64, 0x26, 0x7b, 0xc2, 0x1, 0xc])))] [ text "Pretend scan EPC"]
        , button [ class "btn btn-error", onClick (IndexedDBCommand IndexedDB.deleteDB) ] [ text "Delete database"]
        ]
    , viewPanel "Debug Info"
        [ viewPendingCommands model
        , viewErrorCommands model
        , ul [] (model.recvBuffer |> Fifo.toList |> List.map (\b -> Html.li [] [ text (String.fromInt b) ]))
        ]
    ]

viewPendingCommands : Model -> Html Msg
viewPendingCommands model =
    div []
        [ h1 [] [ text "Pending Commands" ]
        , ul []
            (Dict.values model.pendingCommand
                |> List.map
                    (\cmd ->
                        Html.li [] [ text ("Command: " ++ Debug.toString cmd.command ++ ", Pending for: " ++ humanTimeDifference cmd.sentTime model.time) ]
                    )
            )
        ]

viewErrorCommands : Model -> Html Msg
viewErrorCommands model =
    div []
        [ h1 [] [ text "Error Commands" ]
        , ul []
            (Dict.values model.errorCommand
                |> List.map
                    (\cmd ->
                        Html.li [] [ text ("Command: " ++ Debug.toString cmd.command ++ ", Error code: " ++ errorCodeToString cmd.errorCode ++ ", Error at: " ++ humanTimeDifference cmd.sentTime model.time ++ " ago" )]
                    )
            )
        ]
viewPanelWorkingParameter : WorkingParameters -> Html Msg
viewPanelWorkingParameter wp =
    let
        setRealTimeOutput : String -> Msg
        setRealTimeOutput realTimeOutputStr = 
            let
                realTimeOutput = 
                    case realTimeOutputStr of
                        "OutputHost_StoreUSB" -> WorkingParameters.OutputHost_StoreUSB
                        "NoOutputHost_StoreUSB" -> WorkingParameters.NoOutputHost_StoreUSB
                        "OutputHost_NoStoreUSB" -> WorkingParameters.OutputHost_NoStoreUSB
                        "NoOutputHost_NoStoreUSB" -> WorkingParameters.NoOutputHost_NoStoreUSB
                        _ -> wp.realTimeOutput -- default to current if unrecognized
            in
                UpdateWorkingParameters { wp | realTimeOutput = realTimeOutput }

    in
        viewPanel "Working Parameters"
        [ select [id "realTimeOutput", onInput setRealTimeOutput]

            [ option [value "OutputHost_StoreUSB" , selected (wp.realTimeOutput == WorkingParameters.OutputHost_StoreUSB)] [ text "OutputHost_StoreUSB" ]
            , option [value "NoOutputHost_StoreUSB" , selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_StoreUSB) ] [ text "NoOutputHost_StoreUSB" ]
            , option [value "OutputHost_NoStoreUSB" , selected (wp.realTimeOutput == WorkingParameters.OutputHost_NoStoreUSB) ] [ text "OutputHost_NoStoreUSB" ]
            , option [value "NoOutputHost_NoStoreUSB" , selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_NoStoreUSB) ] [ text "NoOutputHost_NoStoreUSB" ]

            -- [ option [ selected (wp.realTimeOutput == WorkingParameters.OutputHost_StoreUSB), onInput (setRealTimeOutput WorkingParameters.OutputHost_StoreUSB) ] [ text "OutputHost_StoreUSB" ]
            -- , option [ selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_StoreUSB), onInput (setRealTimeOutput WorkingParameters.NoOutputHost_StoreUSB) ] [ text "NoOutputHost_StoreUSB" ]
            -- , option [ selected (wp.realTimeOutput == WorkingParameters.OutputHost_NoStoreUSB), onInput (setRealTimeOutput WorkingParameters.OutputHost_NoStoreUSB) ] [ text "OutputHost_NoStoreUSB" ]
            -- , option [ selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_NoStoreUSB), onInput (setRealTimeOutput WorkingParameters.NoOutputHost_NoStoreUSB) ] [ text "NoOutputHost_NoStoreUSB" ]
            ]
        , label [for "realTimeOutput"] [ text "Real Time Output" ]
    ]

viewPanelFilter : Model -> Html Msg
viewPanelFilter model =
    let
       epcToLi epc =
            Html.li []
                [ span [] 
                    [ text ("EPC: " ++ (epcToString epc) )
                    , button [class "btn", onClick (EPCFilter (Remove epc)) ] [ text "Remove From Filter" ]
                    ]
                ]     
    in
        viewPanel "EPC Filter"
        [ h1 [] [ text ("Total tags in filter: " ++ String.fromInt (Set.size model.epcFilter)) ]
        , ul []
            (model.epcFilter
                |> Set.toList
                |> List.map epcToLi
            )
        ]
viewPanelInventory : Model -> Html Msg
viewPanelInventory model =
    let
       itemToLi item =
            Html.li []
                [ span [] 
                    [ text ("EPC: " ++ (epcToString item.epc) ++ ", Last Seen: " ++ humanTimeDifference item.lastSeen model.time ++ " ago")
                    , button  [class "btn", onClick (EPCFilter (Add item.epc))] [ text "Add to Filter" ]
                    ]
                ]     
    in
    
        viewPanel "Inventory"
            [ h1 [] [ text ("Total unique tags: " ++ String.fromInt (Dict.size model.inventory)) ]
            , button [ class "btn", onClick ClearInventory ] [ text "Clear Inventory" ]
            , ul []
                (Dict.values model.inventory
                    |> List.map itemToLi
                )
            ]


humanTimeDifference : Time.Posix -> Time.Posix -> String
humanTimeDifference earlier later =
    let
        diffMillis = Time.posixToMillis later - Time.posixToMillis earlier
        inSeconds = Time.millisToPosix diffMillis |> Time.toSecond Time.utc

        diffSecondsStr = String.fromInt inSeconds ++ "s"
    in
        diffSecondsStr
    
-- MAIN

main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = viewDoc
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }