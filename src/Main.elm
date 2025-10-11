port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import BytesHelper
import Device exposing (Device, encodeDevice)
import Dict exposing (Dict)
import EPC exposing (EPC)
import Fifo exposing (Fifo)
import Hex
import Html exposing (Html, a, button, div, h1, img, input, label, li, option, p, select, span, text, ul)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (on, onBlur, onCheck, onClick, onInput)
import IndexedDB exposing (IndexedDB, IndexedDBError, KeywordCount)
import Item exposing (Item)
import Json.Decode as Decode exposing (index)
import Json.Encode as Encode
import Monocle.Compose
import Monocle.Iso exposing (Iso)
import Monocle.Lens exposing (Lens)
import Monocle.Prism exposing (Prism)
import Platform.Cmd as Cmd
import Process
import Result.Extra as Result
import Set exposing (Set)
import Svg exposing (circle, line, path, polyline, rect, svg)
import Svg.Attributes as SvgAttrs
import Task
import Time
import Url
import Url.Parser as UrlParser exposing ((</>), Parser, int, oneOf, s, string)
import VH88
import VH88.Command as Command
import VH88.Error exposing (Error(..), ErrorCode(..), errorCodeFromInt, errorCodeToString)
import VH88.Packet as Packet exposing (Packet(..))
import VH88.WorkingParameters as WorkingParameters exposing (WorkingParameters)
import Form
import VH88.WorkingParameters exposing (DataOutputMode(..))



-- STATE MANAGEMENT TYPES


type alias PendingCommand =
    Dict String PendingCommandItem


type alias PendingCommandItem =
    { command : Command.Command
    , sentTime : Time.Posix
    }


newPendingCommand : PendingCommand
newPendingCommand =
    Dict.empty


addPendingCommand : PendingCommand -> Time.Posix -> Command.Command -> PendingCommand
addPendingCommand dict time cmd =
    let
        key =
            Command.toString cmd
    in
    Dict.insert key { command = cmd, sentTime = time } dict


removePendingCommand : PendingCommand -> Command.Command -> PendingCommand
removePendingCommand dict cmd =
    let
        key =
            Command.toString cmd
    in
    Dict.remove key dict


type alias ErrorCommand =
    Dict String PendingErrorItem


type alias PendingErrorItem =
    { command : Command.Command
    , sentTime : Time.Posix
    , errorCode : ErrorCode
    }


newErrorCommand : ErrorCommand
newErrorCommand =
    Dict.empty


addErrorCommand : ErrorCommand -> Time.Posix -> Command.Command -> ErrorCode -> ErrorCommand
addErrorCommand dict time cmd errCode =
    let
        key =
            Command.toString cmd
    in
    Dict.insert key { command = cmd, sentTime = time, errorCode = errCode } dict


removeErrorCommand : ErrorCommand -> Command.Command -> ErrorCommand
removeErrorCommand dict cmd =
    let
        key =
            Command.toString cmd
    in
    Dict.remove key dict


type alias InventoryItem =
    { epc : EPC
    , lastSeen : Time.Posix
    }


type alias Inventory =
    Dict EPC InventoryItem



-- ROUTING


routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ UrlParser.map PageCategories (s "categories")
        , UrlParser.map PageItems (s "items")
        , UrlParser.map PageLocation (s "location")
        , UrlParser.map PageSettings (s "settings")
        , UrlParser.map PageAddItems (s "add-item")
        ]


type Page
    = PageCategories
    | PageItems
    | PageAddItems
    | PageLocation
    | PageSettings


pageSpecificCmds : Page -> List (Cmd Msg)
pageSpecificCmds page =
    case page of
        PageCategories ->
            [ indexedDbCmd IndexedDB.listItemKeywords ]

        PageItems ->
            [ indexedDbCmd IndexedDB.listItems ]

        _ ->
            []



-- MODEL


type alias Model = 
        { key : Nav.Key
        , url : Url.Url
        , time : Time.Posix
        , receivedData : String
        , textToSend : String
        , deviceList : List Device
        , counter : Int
        , selectedDevice : Maybe Device
        , recvBuffer : Fifo.Fifo Int -- FIFO for incoming serial data
        , pendingCommand : PendingCommand
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
        , formModel : Form.Model
        , items : Maybe (List Item.Item)
        , takePictureMsg : Maybe (DataUrl -> Msg)
        , imageDataUrl : Maybe DataUrl
        , addItemsSubmitErrors : List String
        , addItemsFormSubmitting : Bool
        , page : Page
        }


type alias DataUrl =
    String


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        platform =
            Decode.decodeValue (Decode.field "platform" Decode.string) flags |> Result.withDefault "unknown"

        page =
            UrlParser.parse routeParser url |> Maybe.withDefault PageSettings

        initCmd =
            case platform of
                "web" ->
                    Cmd.batch
                        ([ registerListener ()

                         -- , requestDeviceList () -- don't request device list automatically on web, need user gesture
                         , Time.now |> Task.perform Tick
                         , indexedDbCmd IndexedDB.open
                         ]
                            ++ pageSpecificCmds page
                        )

                _ ->
                    Cmd.batch
                        [ registerListener ()
                        , requestDeviceList ()
                        , Time.now |> Task.perform Tick
                        , indexedDbCmd IndexedDB.open
                        ]
    in
    ( 
        { key = key
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
        , formModel = Form.init
        , items = Nothing
        , takePictureMsg = Nothing
        , imageDataUrl = Nothing
        , addItemsSubmitErrors = []
        , addItemsFormSubmitting = False
        , page = page
        }
    , initCmd
    )



-- MESSAGES


type Msg
    = ReceiveData (List Int)
    | ReceivePacket Packet
    | ReceiveResponse Command.CommandWithArgs
    | ReceiveSerialStatus (Maybe SerialStatus)
    | ReceiveDeviceList (List Device)
    | RequestDeviceList
    | SetEditWorkingParameters (Maybe WorkingParameters)
    | UpdateWorkingParameters WorkingParameters
    | SendCommand Packet
    | IncrementCounter
    | DebugCmd String
    | DebugToggle Bool
    | CreateMockData
    | DeviceSelected (Maybe Device)
    | Connect Device
    | Tick Time.Posix
    | SetPowerLevel Int
    | ClearInventory
    | EPCFilter EPCFilterOperation
    | TakePicture (DataUrl -> Msg)
    | TakePictureResult DataUrl
    | FindItem String
    | ListItemKeywords
    | IndexedDBResult (Result IndexedDB.IndexedDBError IndexedDB.IndexedDBResult)
    | IndexedDBCommand IndexedDB.IndexedDbCmdArg
    | PageChange Page (List (Cmd Msg))
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | FormMsg (Form.Msg Msg)
    | OnItemFormSubmit (Form.Validated String Item)
    | AddItemImage DataUrl


type ItemFormField
    = ItemFormTitle String


type EPCFilterOperation
    = Add EPC
    | Remove EPC


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
parseAllPackets : Fifo Int -> List Packet -> ( List Packet, Fifo Int )
parseAllPackets buffer accumulator =
    case VH88.fifoBytesToPacket buffer of
        ( Ok packet, remainingBuffer ) ->
            -- Found a packet, continue parsing
            parseAllPackets remainingBuffer (packet :: accumulator)

        ( Err _, finalBuffer ) ->
            -- No more complete packets, return what we found (in correct order)
            ( List.reverse accumulator, finalBuffer )


{-| Process all packets by calling update with ReceivePacket for each one
-}
processAllPackets : List Packet -> Model -> ( Model, Cmd Msg )
processAllPackets packets ( model) =
    case packets of
        [] ->
            (  model, Cmd.none )

        bunchOfPackets ->
            let
                batchProcessPackets =
                    bunchOfPackets |> List.map (\packet -> message (ReceivePacket packet))
            in
            (  model, Cmd.batch batchProcessPackets )


decodeSerialStatus : List String -> Maybe SerialStatus
decodeSerialStatus list =
    case list of
        [ "serial_waiting_for_user" ] ->
            Just SerialWaitingForUser

        [ "serial_connected" ] ->
            Just SerialConnected

        [ "serial_connecting" ] ->
            Just SerialConnecting

        [ "serial_error", errorMessage ] ->
            Just (SerialError errorMessage)

        _ ->
            Nothing



-- PORTS


port requestDeviceList : () -> Cmd msg


port debugPort : String -> Cmd msg


port deviceConnect : Encode.Value -> Cmd msg


port registerListener : () -> Cmd msg


port serialSend : List Int -> Cmd msg


port serialData : (List Int -> msg) -> Sub msg


port serialStatus : (List String -> msg) -> Sub msg


port deviceList : (Decode.Value -> msg) -> Sub msg


port takePicture : () -> Cmd msg


port pictureResult : (String -> msg) -> Sub msg


port indexedDbCmd : ( String, Encode.Value ) -> Cmd msg


port indexedDbSub : (( String, Decode.Value ) -> msg) -> Sub msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( model) =
    case msg of
        ReceiveData byteList ->
            let
                appendedBuffer =
                    List.foldl Fifo.insert model.recvBuffer byteList

                ( packets, finalBuffer ) =
                    parseAllPackets appendedBuffer []

                recvPacketCmds =
                    packets |> List.map (\packet -> message (ReceivePacket packet))
            in
            ( { model | recvBuffer = finalBuffer }, Cmd.batch recvPacketCmds )

        ReceivePacket packet ->
            let
                ( updatedModel, newCmd ) =
                    case packet of
                        Response (Command.CommandWithArgs ( cmd, args )) ->
                            ( 
                                { model
                                    | pendingCommand = removePendingCommand model.pendingCommand cmd
                                    , errorCommand = removeErrorCommand model.errorCommand cmd -- remove from error as well
                                }
                            , message (ReceiveResponse (Command.CommandWithArgs ( cmd, args )))
                            )

                        Error (Command.CommandWithArgs ( cmd, params )) ->
                            let
                                errorCode =
                                    List.head params |> Maybe.withDefault 0x20 |> errorCodeFromInt
                            in
                            ( 
                                { model
                                    | pendingCommand = removePendingCommand model.pendingCommand cmd
                                    , errorCommand = addErrorCommand model.errorCommand model.time cmd errorCode
                                }
                            , Cmd.none
                            )

                        Status _ ->
                            (  model, Cmd.none )

                        -- Don't remove pending commands for status packets
                        Request _ ->
                            -- Requests are outgoing, we shouldn't receive them
                            (  model, Cmd.none )
            in
            ( updatedModel, newCmd )

        ReceiveResponse response ->
            case response of
                Command.CommandWithArgs ( Command.ReadWorkingParameters, args ) ->
                    let
                        decoder =
                            WorkingParameters.toDecoder

                        maybeWorkingParams =
                            BytesHelper.decodeListInt decoder args
                    in
                    (  { model | workingParameters = maybeWorkingParams, receivedData = "Working parameters updated." }, Cmd.none )

                Command.CommandWithArgs ( Command.HostComputerCardReading, args ) ->
                    let
                        epc : EPC
                        epc =
                            args

                        -- itemForm =
                        --     let
                        --         modelItemForm =
                        --             model.itemForm
                        --     in
                        --     if model.page == PageAddItems || model.showDebug then
                        --         { modelItemForm | epc = EPC.epcStringPrism.reverseGet epc }

                        --     else
                        --         modelItemForm

                        -- we may be filtering this epc
                        filteredEpc =
                            Set.isEmpty model.epcFilter || Set.member epc model.epcFilter

                        inventoryItem =
                            { epc = epc, lastSeen = model.time }

                        newInventory =
                            if filteredEpc then
                                Dict.insert epc inventoryItem model.inventory

                            else
                                model.inventory
                    in
                    (  { model | inventory = newInventory, latestEPC = Just epc }, Cmd.none )

                Command.CommandWithArgs ( unk, args ) ->
                    (  { model | receivedData = "Received unknown response: " }, Cmd.none )

        RequestDeviceList ->
            (  model, requestDeviceList () )

        ReceiveDeviceList devices ->
            let
                firstDeviceCmd =
                    case devices of
                        firstDevice :: _ ->
                            Just firstDevice

                        [] ->
                            Nothing
            in
            ( { model | deviceList = devices, selectedDevice = firstDeviceCmd }, Cmd.none )

        SetEditWorkingParameters maybeParams ->
            (  { model | editWorkingParameters = maybeParams }, Cmd.none )

        UpdateWorkingParameters params ->
            (  { model | workingParameters = Just params, editWorkingParameters = Nothing }, sendPacket (VH88.setWorkingParameters params) )

        ReceiveSerialStatus maybeStatus ->
            case maybeStatus of
                Just status ->
                    let
                        newModel =
                             { model | serialStatus = status }

                        cmd =
                            if status == SerialConnected then
                                sendPacket VH88.readWorkingParameters

                            else
                                Cmd.none
                    in
                    ( newModel, cmd )

                Nothing ->
                    (  { model | receivedData = "Status: Unknown status received." }, Cmd.none )

        IncrementCounter ->
            (  { model | counter = model.counter + 1 }, Cmd.none )

        DeviceSelected device ->
            (  { model | selectedDevice = device }, Cmd.none )

        Connect device ->
            (  model, Cmd.batch [ deviceConnect (encodeDevice device), registerListener () ] )

        DebugCmd debugMsg ->
            (  model, debugPort debugMsg )

        DebugToggle isChecked ->
            (  { model | showDebug = isChecked }, Cmd.none )

        Tick newTime ->
            (  { model | time = newTime }, Cmd.none )

        SendCommand packet ->
            let
                serialSendCmd =
                    sendPacket packet

                ( cmd, _ ) =
                    Packet.packetContents packet

                pendingCommand =
                    addPendingCommandToModel ( model) cmd
            in
            (  { model | pendingCommand = pendingCommand }, serialSendCmd )

        SetPowerLevel powerLevel ->
            (  { model | powerLevel = powerLevel }, Cmd.none )

        ClearInventory ->
            (  { model | inventory = Dict.empty }, Cmd.none )

        EPCFilter operation ->
            case operation of
                Add epc ->
                    (  { model | epcFilter = Set.insert epc model.epcFilter }, Cmd.none )

                Remove epc ->
                    (  { model | epcFilter = Set.remove epc model.epcFilter }, Cmd.none )

        TakePicture dataUrlMsg ->
            (  { model | takePictureMsg = Just dataUrlMsg } , takePicture () )

        TakePictureResult dataUrl ->
            case model.takePictureMsg of
                Just takePictureMsg ->
                    (   { model | takePictureMsg = Nothing } , message (takePictureMsg dataUrl ) )

                Nothing ->
                    (  model, Cmd.none )

        PageChange newPage cmds ->
            ( { model | page = newPage }, Cmd.batch cmds )

        FindItem name ->
            ( model, indexedDbCmd (IndexedDB.findItem name) )

        ListItemKeywords ->
            ( model, indexedDbCmd IndexedDB.listItemKeywords )

        IndexedDBCommand cmdArg ->
            ( model, indexedDbCmd cmdArg )

        IndexedDBResult result ->
            case result of
                Err (IndexedDB.StatusError err) ->
                    (  { model | receivedData = "IndexedDB error: " ++ err }, Cmd.none )

                Err (IndexedDB.CommandError IndexedDB.AddItem err) ->
                    (  model, Cmd.none )

                Err (IndexedDB.CommandError cmd err) ->
                    (  { model | receivedData = "IndexedDB command " ++ IndexedDB.commandToString cmd ++ " error: " ++ err }, Cmd.none )

                Err _ ->
                    (  { model | receivedData = "Unhandled IndexedDB error." }, Cmd.none )

                Ok res ->
                    case res of
                        IndexedDB.OpenResult status ->
                            (  { model | indexedDBStatus = status }, Cmd.none )

                        IndexedDB.FindItemResult maybeItem ->
                            case maybeItem of
                                Just item ->
                                    (  { model | receivedData = "Found item: " ++ item.title }, Cmd.none )

                                Nothing ->
                                    (  { model | receivedData = "Item not found." }, Cmd.none )

                        IndexedDB.ListItemKeywordsResult keywordCounts ->
                            let
                                keywordsString =
                                    keywordCounts
                                        |> Dict.toList
                                        |> List.map (\( keyword, count ) -> keyword ++ " (" ++ String.fromInt count ++ ")")
                                        |> String.join ", "
                            in
                            (  { model | itemKeywordCounts = Just keywordCounts, receivedData = "Keywords: " ++ keywordsString }, Cmd.none )

                        IndexedDB.ListItemsResult items ->
                            (  { model | items = Just items }, Cmd.none )

                        IndexedDB.AddItemResult ->
                            (  model, indexedDbCmd IndexedDB.listItems )

                        IndexedDB.PutItemResult ->
                            (  { model | addItemsFormSubmitting = False }, indexedDbCmd IndexedDB.listItems )

                        IndexedDB.DeleteItemResult ->
                            (  model, indexedDbCmd IndexedDB.listItems )

                        IndexedDB.UnknownResult ->
                            (  { model | receivedData = "Received unknown IndexedDB result." }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (  model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    (  model, Nav.load href )

        UrlChanged url ->
            let
                newPage =
                    UrlParser.parse routeParser url |> Maybe.withDefault PageSettings
            in
            (  { model | url = url, page = newPage }
            , message (PageChange newPage (pageSpecificCmds newPage))
            )

        FormMsg formMsg ->
            let
                ( updatedFormModel, cmd ) =
                    Form.update formMsg model.formModel
            in
            (  { model | formModel = updatedFormModel }, cmd )

        OnItemFormSubmit parsed ->
            case parsed of
                Form.Valid item ->
                    (  { model | addItemsFormSubmitting = True }, indexedDbCmd (IndexedDB.putItem item) )
                Form.Invalid someParsed errors ->
                    (  { model | addItemsFormSubmitting = False }, Cmd.none )
        AddItemImage dataUrl ->
            ( { model | imageDataUrl = Just dataUrl, takePictureMsg = Nothing }, Cmd.none )
        CreateMockData ->
            let
                mockItems : List Item.Item
                mockItems =
                    [ Item.Item "Sewing Needle Set" (EPC.debugMustEPC "e2004718b83064267bc20551") ["sewing", "needle", "craft"] (Just "https://placeholders.io/200/200/needle%2520and%2520thread")
                    , Item.Item "Detail Paintbrush" (EPC.debugMustEPC "e2004718b83064267bc20662") ["faceup", "art", "brush", "craft"] (Just "https://placeholders.io/200/200/fine%2520tip%2520paintbrush")
                    , Item.Item "BJD Glass Eye" (EPC.debugMustEPC "e2004718b83064267bc20773") ["bjd", "doll", "glass", "craft"] (Just "https://placeholders.io/200/200/miniature%2520glass%2520doll%2520eye")
                    , Item.Item "Wig Hair Tinsel Fiber" (EPC.debugMustEPC "e2004718b83064267bc20884") ["wig", "hair", "craft"] (Just "https://placeholders.io/200/200/synthetic%2520hair%2520fiber")
                    , Item.Item "Miniature Bead Jar" (EPC.debugMustEPC "e2004718b83064267bc20995") ["beads", "art", "craft"] (Just "https://placeholders.io/200/200/small%2520bead%2520storage%2520box")
                    , Item.Item "Assorted Resistors" (EPC.debugMustEPC "e2004718b83064267bc2100A") ["resistor", "electronics", "arduino"] (Just "https://placeholders.io/200/200/tiny%2520electronic%2520resistor")
                    , Item.Item "Jumper Wires (Set)" (EPC.debugMustEPC "e2004718b83064267bc2111B") ["jumper", "wire", "arduino", "electronics"] (Just "https://placeholders.io/200/200/colorful%2520jumper%2520wires")
                    , Item.Item "Micro USB Connector" (EPC.debugMustEPC "e2004718b83064267bc2122C") ["usb", "cable", "connector", "electronics"] (Just "https://placeholders.io/200/200/micro%2520usb%2520connector")
                    , Item.Item "LED Diode (5mm)" (EPC.debugMustEPC "e2004718b83064267bc2133D") ["led", "diode", "electronics", "arduino"] (Just "https://placeholders.io/200/200/small%2520red%2520led%2520diode")
                    , Item.Item "Mini Solder Spool" (EPC.debugMustEPC "e2004718b83064267bc2144E") ["solder", "electronics", "tool"] (Just "https://placeholders.io/200/200/mini%2520solder%2520spool")
                    , Item.Item "Sewing Pins (Box)" (EPC.debugMustEPC "e2004718b83064267bc2155F") ["sewing", "pins", "craft", "storage"] (Just "https://placeholders.io/200/200/box%2520of%2520sewing%2520pins")
                    , Item.Item "Thimble" (EPC.debugMustEPC "e2004718b83064267bc2166A") ["sewing", "tool", "finger%2520guard"] (Just "https://placeholders.io/200/200/metal%2520sewing%2520thimble")
                    , Item.Item "Fabric Chalk Pen" (EPC.debugMustEPC "e2004718b83064267bc2177B") ["sewing", "marking", "tool"] (Just "https://placeholders.io/200/200/fabric%2520chalk%2520marker")
                    , Item.Item "Micro Seam Ripper" (EPC.debugMustEPC "e2004718b83064267bc2188C") ["sewing", "tool", "unpick"] (Just "https://placeholders.io/200/200/tiny%2520seam%2520ripper")
                    , Item.Item "BJD Wig Cap (Small)" (EPC.debugMustEPC "e2004718b83064267bc2199D") ["bjd", "wig", "cap", "doll"] (Just "https://placeholders.io/200/200/small%2520doll%2520wig%2520cap")
                    , Item.Item "Wig Styling Clips" (EPC.debugMustEPC "e2004718b83064267bc2200E") ["wig", "styling", "clips", "tool"] (Just "https://placeholders.io/200/200/mini%2520hair%2520styling%2520clips")
                    , Item.Item "Acrylic Paint Set (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2211F") ["faceup", "art", "paint", "acrylic"] (Just "https://placeholders.io/200/200/mini%2520acrylic%2520paint%2520tubes")
                    , Item.Item "Matte Varnish Spray" (EPC.debugMustEPC "e2004718b83064267bc2222A") ["faceup", "varnish", "spray", "sealant"] (Just "https://placeholders.io/200/200/small%2520matte%2520sealer%2520can")
                    , Item.Item "Pastel Powder Pigments" (EPC.debugMustEPC "e2004718b83064267bc2233B") ["faceup", "pastel", "pigment", "art"] (Just "https://placeholders.io/200/200/box%2520of%2520soft%2520pastel%2520chalks")
                    , Item.Item "Micron Pen (005)" (EPC.debugMustEPC "e2004718b83064267bc2244C") ["art", "sketch", "pen", "detail"] (Just "https://placeholders.io/200/200/ultra%2520fine%2520micron%2520pen")
                    , Item.Item "Precision Tweezers" (EPC.debugMustEPC "e2004718b83064267bc2255D") ["tool", "craft", "assembly"] (Just "https://placeholders.io/200/200/bent%2520tip%2520precision%2520tweezers")
                    , Item.Item "Jewelry Pliers (Nose)" (EPC.debugMustEPC "e2004718b83064267bc2266E") ["craft", "tool", "jewelry"] (Just "https://placeholders.io/200/200/small%2520round%2520nose%2520pliers")
                    , Item.Item "Wire Cutter (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2277F") ["tool", "craft", "electronics", "cutter"] (Just "https://placeholders.io/200/200/mini%2520diagonal%2520wire%2520cutter")
                    , Item.Item "Small Hot Glue Sticks" (EPC.debugMustEPC "e2004718b83064267bc2288A") ["glue", "craft", "supply"] (Just "https://placeholders.io/200/200/mini%2520hot%2520glue%2520sticks")
                    , Item.Item "Plastic Safety Eyes" (EPC.debugMustEPC "e2004718b83064267bc2299B") ["doll", "sewing", "craft", "eyes"] (Just "https://placeholders.io/200/200/black%2520plastic%2520safety%2520eyes")
                    , Item.Item "Ceramic Capacitors (Assorted)" (EPC.debugMustEPC "e2004718b83064267bc2300C") ["capacitor", "electronics", "component"] (Just "https://placeholders.io/200/200/tiny%2520ceramic%2520capacitors")
                    , Item.Item "DIP Switch (8-pin)" (EPC.debugMustEPC "e2004718b83064267bc2311D") ["switch", "electronics", "arduino"] (Just "https://placeholders.io/200/200/8%2520pin%2520dip%2520switch")
                    , Item.Item "Tactile Push Button" (EPC.debugMustEPC "e2004718b83064267bc2322E") ["button", "switch", "electronics"] (Just "https://placeholders.io/200/200/small%2520tactile%2520push%2520button")
                    , Item.Item "Potentiometer (10k Ohm)" (EPC.debugMustEPC "e2004718b83064267bc2333F") ["potentiometer", "electronics", "control"] (Just "https://placeholders.io/200/200/10k%2520ohm%2520potentiometer")
                    , Item.Item "5V Regulator (LM7805)" (EPC.debugMustEPC "e2004718b83064267bc2344A") ["regulator", "electronics", "power"] (Just "https://placeholders.io/200/200/lm7805%2520voltage%2520regulator")
                    , Item.Item "Microcontroller (ATmega)" (EPC.debugMustEPC "e2004718b83064267bc2355B") ["microcontroller", "chip", "arduino"] (Just "https://placeholders.io/200/200/atmega328p%2520microcontroller%2520chip")
                    , Item.Item "Header Pins (Strip)" (EPC.debugMustEPC "e2004718b83064267bc2366C") ["pins", "connector", "electronics"] (Just "https://placeholders.io/200/200/breakaway%2520header%2520pins")
                    , Item.Item "Breadboard (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2377D") ["breadboard", "prototype", "arduino"] (Just "https://placeholders.io/200/200/mini%2520white%2520breadboard")
                    , Item.Item "Ferrite Bead" (EPC.debugMustEPC "e2004718b83064267bc2388E") ["ferrite", "noise", "electronics", "usb"] (Just "https://placeholders.io/200/200/small%2520black%2520ferrite%2520bead")
                    , Item.Item "USB-C Shell Crimp" (EPC.debugMustEPC "e2004718b83064267bc2399F") ["usb%2520c", "crimp", "cable", "connector"] (Just "https://placeholders.io/200/200/metal%2520usb%2520c%2520connector%2520shell")
                    , Item.Item "Heat Shrink Tubing (Small)" (EPC.debugMustEPC "e2004718b83064267bc2400A") ["heat%2520shrink", "cable", "tool"] (Just "https://placeholders.io/200/200/assorted%2520heat%2520shrink%2520tubing")
                    , Item.Item "Cable Braiding Paracord" (EPC.debugMustEPC "e2004718b83064267bc2411B") ["cable", "braid", "paracord", "usb"] (Just "https://placeholders.io/200/200/small%2520bundle%2520of%2520paracord")
                    , Item.Item "Wire Stripper (Pocket)" (EPC.debugMustEPC "e2004718b83064267bc2422C") ["tool", "wire", "stripper", "electronics"] (Just "https://placeholders.io/200/200/pocket%2520wire%2520stripper")
                    , Item.Item "Small DC Motor" (EPC.debugMustEPC "e2004718b83064267bc2433D") ["motor", "electronics", "arduino", "actuator"] (Just "https://placeholders.io/200/200/tiny%2520dc%2520hobby%2520motor")
                    , Item.Item "OLED Display (0.96 inch)" (EPC.debugMustEPC "e2004718b83064267bc2444E") ["display", "oled", "arduino", "screen"] (Just "https://placeholders.io/200/200/0.96%2520inch%2520oled%2520display")
                    , Item.Item "CR2032 Battery (Coin)" (EPC.debugMustEPC "e2004718b83064267bc2455F") ["battery", "power", "electronics"] (Just "https://placeholders.io/200/200/cr2032%2520coin%2520cell%2520battery")
                    , Item.Item "Photoresistor (LDR)" (EPC.debugMustEPC "e2004718b83064267bc2466A") ["sensor", "light", "arduino", "electronics"] (Just "https://placeholders.io/200/200/small%2520photoresistor%2520ldr")
                    , Item.Item "Infrared Receiver (IR)" (EPC.debugMustEPC "e2004718b83064267bc2477B") ["sensor", "ir", "electronics", "remote"] (Just "https://placeholders.io/200/200/small%2520infrared%2520receiver%2520module")
                    , Item.Item "Mini Speaker (8 Ohm)" (EPC.debugMustEPC "e2004718b83064267bc2488C") ["speaker", "audio", "electronics"] (Just "https://placeholders.io/200/200/tiny%25208%2520ohm%2520speaker")
                    , Item.Item "Shift Register (74HC595)" (EPC.debugMustEPC "e2004718b83064267bc2499D") ["chip", "electronics", "logic"] (Just "https://placeholders.io/200/200/shift%2520register%2520ic%2520chip")
                    , Item.Item "Rotary Encoder" (EPC.debugMustEPC "e2004718b83064267bc2500E") ["encoder", "input", "arduino", "control"] (Just "https://placeholders.io/200/200/rotary%2520encoder%2520with%2520button")
                    , Item.Item "UV Resin (Small Bottle)" (EPC.debugMustEPC "e2004718b83064267bc2511F") ["resin", "bjd", "art", "craft"] (Just "https://placeholders.io/200/200/small%2520bottle%2520uv%2520resin")
                    , Item.Item "Dotting Tools (Set)" (EPC.debugMustEPC "e2004718b83064267bc2522A") ["faceup", "art", "tools", "nail%2520art"] (Just "https://placeholders.io/200/200/set%2520of%2520dotting%2520tools")
                    , Item.Item "Wig Weft Clip" (EPC.debugMustEPC "e2004718b83064267bc2533B") ["wig", "hair", "craft", "clip"] (Just "https://placeholders.io/200/200/mini%2520wig%2520weft%2520clip")
                    , Item.Item "Doll Eyelash Strips" (EPC.debugMustEPC "e2004718b83064267bc2544C") ["bjd", "faceup", "eyelash", "doll"] (Just "https://placeholders.io/200/200/tiny%2520doll%2520eyelash%2520strips")
                    , Item.Item "Tiny Snap Buttons" (EPC.debugMustEPC "e2004718b83064267bc2555D") ["sewing", "fastener", "craft", "notion"] (Just "https://placeholders.io/200/200/small%2520snap%2520buttons")
                    , Item.Item "Glue B-7000 (Small Tube)" (EPC.debugMustEPC "e2004718b83064267bc2566E") ["glue", "craft", "bjd", "electronics"] (Just "https://placeholders.io/200/200/small%2520tube%2520b-7000%2520glue")
                    , Item.Item "Slide Switch (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2577F") ["switch", "electronics", "component"] (Just "https://placeholders.io/200/200/mini%2520slide%2520switch")
                    , Item.Item "Dupont Connector Pins" (EPC.debugMustEPC "e2004718b83064267bc2588A") ["dupont", "connector", "wire", "electronics"] (Just "https://placeholders.io/200/200/dupont%2520connector%2520pins")
                    , Item.Item "Servo Motor (9g)" (EPC.debugMustEPC "e2004718b83064267bc2599B") ["servo", "motor", "arduino", "actuator"] (Just "https://placeholders.io/200/200/mini%25209g%2520servo%2520motor")
                    , Item.Item "Sewing Machine Oil" (EPC.debugMustEPC "e2004718b83064267bc2600C") ["sewing", "tool", "maintenance"] (Just "https://placeholders.io/200/200/small%2520bottle%2520sewing%2520oil")
                    , Item.Item "UV LED Torch" (EPC.debugMustEPC "e2004718b83064267bc2611D") ["uv", "led", "resin", "tool"] (Just "https://placeholders.io/200/200/small%2520uv%2520led%2520torch")
                    , Item.Item "Fine Sandpaper (Strips)" (EPC.debugMustEPC "e2004718b83064267bc2622E") ["sandpaper", "art", "faceup", "tool"] (Just "https://placeholders.io/200/200/fine%2520grit%2520sandpaper")
                    , Item.Item "Fiber Optic Strands" (EPC.debugMustEPC "e2004718b83064267bc2633F") ["fiber", "optic", "electronics", "lighting"] (Just "https://placeholders.io/200/200/thin%2520fiber%2520optic%2520strands")
                    , Item.Item "Small Toggle Switch" (EPC.debugMustEPC "e2004718b83064267bc2644A") ["switch", "electronics", "component"] (Just "https://placeholders.io/200/200/mini%2520toggle%2520switch")
                    ]


                
                createItemsCmd = Cmd.batch (List.map (indexedDbCmd << IndexedDB.putItem) mockItems)

                in
                
                (  model, createItemsCmd )


addPendingCommandToModel : Model -> Command.Command -> PendingCommand
addPendingCommandToModel ( model) cmd =
    addPendingCommand model.pendingCommand model.time cmd


sendPacket : Packet -> Cmd Msg
sendPacket packet =
    packet |> Packet.packetToBytes |> serialSend



-- Tasks and Cmd helper
-- See https://discourse.elm-lang.org/t/when-does-elm-not-update-the-ui/901/4


message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed


delay : Float -> msg -> Cmd msg
delay sleepMs msg =
    Process.sleep sleepMs
        |> Task.map (always <| msg)
        |> Task.perform identity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ serialData ReceiveData
        -- , Time.every 500 Tick
        , serialStatus
            (\rawList ->
                ReceiveSerialStatus (decodeSerialStatus rawList)
            )
        , deviceList
            (\json ->
                case Decode.decodeValue Device.decodeDeviceList json of
                    Ok devices ->
                        ReceiveDeviceList devices

                    Err _ ->
                        ReceiveDeviceList []
            )
        , pictureResult TakePictureResult
        , indexedDbSub (IndexedDB.receive >> IndexedDBResult)
        ]



-- VIEW


viewDock : Model -> Html Msg
viewDock model =
    div [ class "dock " ]
        [ button []
            [ svg [ SvgAttrs.class "size-[1.2em]", SvgAttrs.viewBox "0 0 24 24" ]
                [ Svg.polyline [ SvgAttrs.points "1 11 12 2 23 11", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.path [ SvgAttrs.d "m5,13v7c0,1.105.895,2,2,2h10c1.105,0,2-.895,2-2v-7", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.line [ SvgAttrs.x1 "12", SvgAttrs.y1 "22", SvgAttrs.x2 "12", SvgAttrs.y2 "18", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                ]
            , span [ class "dock-label" ] [ text "Home" ]
            ]
        , button [ class "dock-active" ]
            [ svg [ SvgAttrs.class "size-[1.2em]", SvgAttrs.viewBox "0 0 24 24" ]
                [ Svg.polyline [ SvgAttrs.points "3 14 9 14 9 17 15 17 15 14 21 14", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.rect [ SvgAttrs.x "3", SvgAttrs.y "3", SvgAttrs.width "18", SvgAttrs.height "18", SvgAttrs.rx "2", SvgAttrs.ry "2", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                ]
            , span [ class "dock-label" ] [ text "Inbox" ]
            ]
        , button []
            [ svg [ SvgAttrs.class "size-[1.2em]", SvgAttrs.viewBox "0 0 24 24" ]
                [ Svg.circle [ SvgAttrs.cx "12", SvgAttrs.cy "12", SvgAttrs.r "3", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                , Svg.path [ SvgAttrs.d "m22,13.25v-2.5l-2.318-.966c-.167-.581-.395-1.135-.682-1.654l.954-2.318-1.768-1.768-2.318.954c-.518-.287-1.073-.515-1.654-.682l-.966-2.318h-2.5l-.966,2.318c-.581.167-1.135.395-1.654.682l-2.318-.954-1.768,1.768.954,2.318c-.287.518-.515,1.073-.682,1.654l-2.318.966v2.5l2.318.966c.167.581.395,1.135.682,1.654l-.954,2.318,1.768,1.768,2.318-.954c.518.287,1.073.515,1.654.682l.966,2.318h2.5l.966-2.318c.581-.167,1.135-.395,1.654-.682l2.318.954,1.768-1.768-.954-2.318c.287-.518.515-1.073+.682-1.654l2.318-.966Z", SvgAttrs.fill "none", SvgAttrs.stroke "currentColor", SvgAttrs.strokeLinecap "square", SvgAttrs.strokeMiterlimit "10", SvgAttrs.strokeWidth "2" ] []
                ]
            , span [ class "dock-label" ] [ text "Settings" ]
            ]
        ]


viewNavbar : Model -> Html Msg
viewNavbar ( model) =
    let
        isActive page =
            if model.page == page then
                "tab-active "

            else
                ""

        link : String -> Page -> String -> List (Cmd Msg) -> Html Msg
        link href page label cmds =
            Html.a [ Attrs.attribute "role" "tab", Attrs.href href, class (isActive page ++ "text-xs tab ") ] [ text label ]
    in
    div [ Attrs.attribute "role" "tablist", class " tabs tabs-border shadow-sm justify-center pb-8" ]
        [ link "/categories" PageCategories "Categories" [ message ListItemKeywords ]
        , link "/items" PageItems "Items" []
        , link "/add-item" PageAddItems "Add Item" []
        , link "/location" PageLocation "Location" []
        , link "/settings" PageSettings "Settings" []
        -- , Html.a [ Attrs.attribute "role" "tab", class (isActive PageTODO ++ "text-xs tab") ] [ text "Ownership" ]
        -- , Html.a [ Attrs.attribute "role" "tab", class (isActive PageTODO ++ "text-xs tab") ] [ text "All" ]
        ]


viewItemList : Model -> List Item.Item -> Html Msg
viewItemList model items =
    let
        listItem : Item.Item -> Html Msg
        listItem { epc, title, keywords, imageDataUrl } =
            let
                itemImage =
                    imageDataUrl |> Maybe.withDefault "https://img.daisyui.com/images/profile/demo/1@94.webp"
            in
            li [ class "list-row" ]
                [ div [] [ Html.img [ class "size-10 rounded-box", src itemImage ] [] ]
                , div [ class "flex flex-col" ]
                    [ div [ class "flex justify-between" ]
                        [ div [] [ text title ]
                        , div [ class "text-xs uppercase font-semibold opacity-60" ] [ text (String.join ", " keywords) ]
                        ]
                    , div [ class "flex self-end gap-4" ]
                        [ button [ class "btn btn-sm btn-ghost text-error max-w-xs", onClick (IndexedDBCommand (IndexedDB.deleteItem epc)) ] [ text "Delete item" ]
                        , button [ class "btn btn-sm max-w-xs", onClick (EPCFilter (Add epc)) ] [ text "Add to filter" ]
                        ]
                    ]
                ]

        listItems =
            items
                |> List.map listItem
    in
    ul [ class "list bg-base-100 rounded-box shadow-md" ]
        listItems


viewKeywordCountList : Model -> KeywordCount -> Html Msg
viewKeywordCountList model keywordCounts =
    let
        listItem : String -> String -> Html Msg
        listItem keyword count =
            li [ class "list-row" ]
                [ div [] [ Html.img [ class "size-10 rounded-box", src "https://img.daisyui.com/images/profile/demo/1@94.webp" ] [] ]
                , div [ class "flex justify-between" ]
                    [ div [] [ text keyword ]
                    , div [ class "text-xs uppercase font-semibold opacity-60" ] [ text count ]
                    ]
                ]

        listItems =
            keywordCounts
                |> Dict.toList
                |> List.map (\( keyword, count ) -> listItem keyword (String.fromInt count))
    in
    ul [ class "list bg-base-100 rounded-box shadow-md" ]
        listItems


viewHeading : Html Msg
viewHeading =
    div
        [ class "flex justify-center w-full bg-[url('overlay.jpg')] bg-cover bg-center bg-clip-text" ]
        -- Utility class to center content horizontally
        [ h1
            [ class "text-4xl text-neutral font-extrabold text-base-content py-2  text-transparent" ]
            -- Stylize the h1
            [ text "Inventory." ]
        ]


viewDoc : Model -> Browser.Document Msg
viewDoc model =
    { title = "Inventory"
    , body = [ view model ]
    }


view : Model -> Html Msg
view ( model) =
    div [ class "min-h-screen bg-base-200 text-base-content" ]
        [ case model.page of
            PageCategories ->
                pageCategories ( model)

            PageLocation ->
                pageLocation ( model)

            PageSettings ->
                pageSettings ( model)

            PageItems ->
                pageItems ( model)

            PageAddItems ->
                pageAddItems ( model)
        ]


pageCategories : Model -> Html Msg
pageCategories ( model) =
    let
        maybeKeywordCounts =
            model.itemKeywordCounts

        panelContents =
            case maybeKeywordCounts of
                Just keywordCounts ->
                    if Dict.isEmpty keywordCounts then
                        p [] [ text "No keywords found." ]

                    else
                        viewKeywordCountList ( model) keywordCounts

                Nothing ->
                    p [] [ text "Loading..." ]
    in
    div [ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar ( model)
        , viewPanel ""
            [ button [ class "btn", onClick (FindItem "Yarn") ] [ text "Search" ]
            , button [ class "btn", onClick ListItemKeywords ] [ text "Get keywords" ]
            , panelContents
            ]
        ]


pageLocation : Model -> Html Msg
pageLocation ( model) =
    div [ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar ( model)
        , p [] [ text "Location page content goes here." ]
        ]


pageSettings : Model -> Html Msg
pageSettings ( model) =
    div [ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar ( model)
        , view2 model
        ]


pageItems : Model -> Html Msg
pageItems ( model) =
    let
        maybeItems =
            model.items

        panelItems =
            case maybeItems of
                Just items ->
                    if List.isEmpty items then
                        p [] [ text "No items found." ]

                    else
                        viewItemList ( model) items

                Nothing ->
                    p [] [ text "Loading..." ]
    in
    div [ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar ( model)
        , panelItems
        ]


pageAddItems : Model -> Html Msg
pageAddItems ( model) =
    let
        hiddenFields : Item.FormHiddenFields
        hiddenFields =
            { epc =  model.latestEPC |> Maybe.map EPC.epcStringPrism.reverseGet |> Maybe.withDefault ""
            , keywords = ""
            , imageDataUrl = model.imageDataUrl |> Maybe.withDefault ""
            }
        msg =
            { formMsg = FormMsg
            , onSubmit = OnItemFormSubmit
            , takePicture = TakePicture AddItemImage
            }
        errors = model.addItemsSubmitErrors
    in
    div [ class "flex flex-col" ]
        [ viewHeading
        , viewNavbar ( model)
        , Item.myRenderedForm msg hiddenFields errors model.addItemsFormSubmitting model.formModel
        ]
    -- Html.div []
    -- [ text "Add Items page content goes here."]
    -- let
    --     titleErrors =
    --         model.itemFormValidationErrors.title
    --             |> List.map (\err -> div [ class "text-sm text-warning" ] [ text err ])
    --             |> Html.div []

    --     formErrors =
    --         model.itemFormValidationErrors.formErrors
    --             |> List.map (\err -> div [ class "text-sm text-error" ] [ text err ])
    --             |> Html.div []

    --     formImageDataUrlLens : Lens Item.Form (Maybe DataUrl)
    --     formImageDataUrlLens =
    --         Lens .imageDataUrl (\b a -> { a | imageDataUrl = b })

    --     modelFormLens : Lens Model Item.Form
    --     modelFormLens =
    --         Lens (\(Model m) -> m.itemForm) (\b (Model a) -> Model { a | itemForm = b })

    --     modelImageDataUrlLens : Lens Model (Maybe DataUrl)
    --     modelImageDataUrlLens =
    --         modelFormLens |> Monocle.Compose.lensWithLens formImageDataUrlLens
    -- in
    -- div [ class "flex flex-col" ]
    --     [ viewHeading
    --     , viewNavbar (Model model)
    --     -- , viewPanel "Add Item"
    --     --     [ input [ placeholder "Enter item name...", class "input input-bordered w-full max-w-xs", onInput (\title -> AddItemForm (ItemFormTitle title)), onBlur (AddItemFormOnBlur "title"), value model.itemForm.title ] []
    --     --     , titleErrors
    --     --     , label [ for "scanned-epc" ] [ text "Scanned EPC: " ]
    --     --     , input [ id "scanned-epc", class "input", disabled True, value model.itemForm.epc ] []

    --     --     -- , epcErrors -- shouldn't be any errors here since it's read-only
    --     --     -- take picture
    --     --     , label [ for "item-picture" ] [ text "Item Picture:" ]
    --     --     , case model.itemForm.imageDataUrl of
    --     --         Just dataUrl ->
    --     --             img [ id "item-picture", class "rounded-xl max-w-xs self-center", src dataUrl ] []

    --     --         Nothing ->
    --     --             p [] [ text "No picture taken." ]
    --     --     , button [ class "btn btn-secondary", onClick (TakePicture modelImageDataUrlLens) ] [ text "Take picture" ]
    --     --     , formErrors
    --     --     , button [ class "btn btn-secondary", onClick AddItemFormSubmit ] [ text "Put Item" ]
    --     --     ]
    --     ]


view2 model =
    div [ class "bg-base flex flex-col p-4  gap-2" ]
        [ viewPanel ""
            [ p [] [ text ("Platform: " ++ model.platform) ]
            , button [ class "btn btn-primary" ] [ text "DaisyUI Button" ]

            -- , div [] [button [ onClick TakePicture ] [ text "Take picture" ]]
            , div [] [ button [ onClick RequestDeviceList ] [ text "Connect to Serial Port" ] ]
            , div [] [ input [ placeholder "Enter text..." ] [] ]
            , div [] [ text <| "Received: " ++ model.receivedData ]
            , div [] [ text <| "Counter: " ++ String.fromInt model.counter ]
            , div [] [ button [ onClick IncrementCounter ] [ text "Increment Counter" ] ]
            , div []
                [ input [ id "debug-checkbox", type_ "checkbox", onCheck DebugToggle, class "w-4 h-4 text-blue-600 bg-gray-100 border-gray-300 rounded-sm focus:ring-blue-500 dark:focus:ring-blue-600 dark:ring-offset-gray-800 focus:ring-2 dark:bg-gray-700 dark:border-gray-600" ] []
                , label [ for "debug-checkbox", class "ms-2 text-sm font-medium text-gray-900 dark:text-gray-300" ] [ text "Enable Debugging" ]
                ]
            ]
        , htmlIf model.showDebug (viewDebugCmd ( model)) (div [] [])
        , viewPanelConnection ( model)
        , viewPanelRfidPower ( model)
        , case model.editWorkingParameters of
            Just ewp ->
                viewPanelWorkingParameter ewp

            Nothing ->
                button [ onClick (SetEditWorkingParameters model.workingParameters), class "btn btn-secondary" ] [ text "Edit Working Parameters" ]
        , viewPanelFilter ( model)
        , viewPanelInventory ( model)
        ]


viewPanel : String -> List (Html msg) -> Html msg
viewPanel title contents =
    div [ class "p-4 border-base-300 rounded bg-base-100" ]
        [ h1 [] [ text title ]
        , div [ class "flex flex-col gap-2" ] contents
        ]


viewPanelConnection : Model -> Html Msg
viewPanelConnection ( model) =
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
        [ p [] [ text ("Connection status: " ++ serialStatusToString model.serialStatus) ]
        , htmlIf (List.isEmpty model.deviceList)
            (Html.h1 [ class "font-bold text-warning" ] [ text "No Bluetooth Devices Found" ])
            (div []
                [ text "Available Bluetooth Devices:"
                , select [ onInput (Device.deviceByAddr model.deviceList >> DeviceSelected) ]
                    (List.map (\device -> option [ value device.address ] [ text device.name ]) model.deviceList)
                ]
            )
        , button attr [ text "Connect" ]
        ]


viewPanelRfidPower : Model -> Html Msg
viewPanelRfidPower ( model) =
    let
        powerLevelCommand =
            VH88.setRFIDPower model.powerLevel
                |> Result.toMaybe
                |> Maybe.map SendCommand

        onpointerup =
            case powerLevelCommand of
                Just cmd ->
                    [ on "pointerup" (Decode.succeed cmd) ]

                Nothing ->
                    []
    in
    viewPanel ("Power level: " ++ String.fromInt model.powerLevel)
        [ input (onpointerup ++ [ type_ "range", Attrs.min "0", Attrs.max "33", Attrs.value (String.fromInt model.powerLevel), onInput (SetPowerLevel << Maybe.withDefault 0 << String.toInt) ]) []
        ]


htmlIf : Bool -> Html msg -> Html msg -> Html msg
htmlIf condition htmlTrue htmlFalse =
    if condition then
        htmlTrue

    else
        htmlFalse


viewDebugCmd : Model -> Html Msg
viewDebugCmd ( model) =
    div []
        [ viewPanel "Debug Commands"
            [ button [ class "btn", onClick (ReceiveDeviceList [ Device "Device A" "00:00:00:00:00:01", Device "Device B" "00:00:00:00:00:02" ]) ] [ text "Debug get device list" ]
            , button [ class "btn", onClick (ReceiveData [ 0xF0, 0x03, 0x04, 0x00, 0x09 ]) ] [ text "Receive successful cmd 0x4" ]
            , button [ class "btn", onClick (ReceiveData [ 0xF4, 0x03, 0x04, 0x11, 0xF4 ]) ] [ text "Receive error cmd 0x4" ]
            , button [ class "btn", onClick (SendCommand VH88.startListingTags) ] [ text "Debug start listing tags" ]
            , button [ class "btn", onClick (ReceiveResponse (Command.CommandWithArgs ( Command.HostComputerCardReading, [ 0xE2, 0x00, 0x47, 0x18, 0xB8, 0x30, 0x64, 0x26, 0x7B, 0xC2, 0x01, 0x0C ] ))) ] [ text "Pretend scan EPC" ]
            , button [ class "btn btn-error", onClick (IndexedDBCommand IndexedDB.deleteDB) ] [ text "Delete database" ]
            , button [ class "btn", onClick CreateMockData ] [ text "Create mock data" ]
            ]
        -- , viewPanel "Debug Info"
        --     [ viewPendingCommands (Model model)
        --     , viewErrorCommands (Model model)
        --     , ul [] (model.recvBuffer |> Fifo.toList |> List.map (\b -> Html.li [] [ text (String.fromInt b) ]))
        --     ]
        ]


-- viewPendingCommands : Model -> Html Msg
-- viewPendingCommands (Model model) =
--     div []
--         [ h1 [] [ text "Pending Commands" ]
--         , ul []
--             (Dict.values model.pendingCommand
--                 |> List.map
--                     (\cmd ->
--                         Html.li [] [ text ("Command: " ++ Command.toString cmd.command ++ ", Pending for: " ++ humanTimeDifference cmd.sentTime model.time) ]
--                     )
--             )
--         ]


-- viewErrorCommands : Model -> Html Msg
-- viewErrorCommands (Model model) =
--     div []
--         [ h1 [] [ text "Error Commands" ]
--         , ul []
--             (Dict.values model.errorCommand
--                 |> List.map
--                     (\cmd ->
--                         Html.li [] [ text ("Command: " ++ Command.toString cmd.command ++ ", Error code: " ++ errorCodeToString cmd.errorCode ++ ", Error at: " ++ humanTimeDifference cmd.sentTime model.time ++ " ago") ]
--                     )
--             )
--         ]


viewPanelWorkingParameter : WorkingParameters -> Html Msg
viewPanelWorkingParameter wp =
    let
        setRealTimeOutput : String -> Msg
        setRealTimeOutput realTimeOutputStr =
            let
                realTimeOutput =
                    case realTimeOutputStr of
                        "OutputHost_StoreUSB" ->
                            WorkingParameters.OutputHost_StoreUSB

                        "NoOutputHost_StoreUSB" ->
                            WorkingParameters.NoOutputHost_StoreUSB

                        "OutputHost_NoStoreUSB" ->
                            WorkingParameters.OutputHost_NoStoreUSB

                        "NoOutputHost_NoStoreUSB" ->
                            WorkingParameters.NoOutputHost_NoStoreUSB

                        _ ->
                            wp.realTimeOutput

                -- default to current if unrecognized
            in
            UpdateWorkingParameters { wp | realTimeOutput = realTimeOutput }
    in
    viewPanel "Working Parameters"
        [ select [ id "realTimeOutput", onInput setRealTimeOutput ]
            [ option [ value "OutputHost_StoreUSB", selected (wp.realTimeOutput == WorkingParameters.OutputHost_StoreUSB) ] [ text "OutputHost_StoreUSB" ]
            , option [ value "NoOutputHost_StoreUSB", selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_StoreUSB) ] [ text "NoOutputHost_StoreUSB" ]
            , option [ value "OutputHost_NoStoreUSB", selected (wp.realTimeOutput == WorkingParameters.OutputHost_NoStoreUSB) ] [ text "OutputHost_NoStoreUSB" ]
            , option [ value "NoOutputHost_NoStoreUSB", selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_NoStoreUSB) ] [ text "NoOutputHost_NoStoreUSB" ]

            -- [ option [ selected (wp.realTimeOutput == WorkingParameters.OutputHost_StoreUSB), onInput (setRealTimeOutput WorkingParameters.OutputHost_StoreUSB) ] [ text "OutputHost_StoreUSB" ]
            -- , option [ selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_StoreUSB), onInput (setRealTimeOutput WorkingParameters.NoOutputHost_StoreUSB) ] [ text "NoOutputHost_StoreUSB" ]
            -- , option [ selected (wp.realTimeOutput == WorkingParameters.OutputHost_NoStoreUSB), onInput (setRealTimeOutput WorkingParameters.OutputHost_NoStoreUSB) ] [ text "OutputHost_NoStoreUSB" ]
            -- , option [ selected (wp.realTimeOutput == WorkingParameters.NoOutputHost_NoStoreUSB), onInput (setRealTimeOutput WorkingParameters.NoOutputHost_NoStoreUSB) ] [ text "NoOutputHost_NoStoreUSB" ]
            ]
        , label [ for "realTimeOutput" ] [ text "Real Time Output" ]
        ]


viewPanelFilter : Model -> Html Msg
viewPanelFilter ( model) =
    let
        epcToLi epc =
            Html.li []
                [ span []
                    [ text ("EPC: " ++ EPC.epcStringPrism.reverseGet epc)
                    , button [ class "btn", onClick (EPCFilter (Remove epc)) ] [ text "Remove From Filter" ]
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
viewPanelInventory ( model) =
    let
        itemToLi item =
            Html.li []
                [ span []
                    [ text ("EPC: " ++ EPC.epcStringPrism.reverseGet item.epc ++ ", Last Seen: " ++ humanTimeDifference item.lastSeen model.time ++ " ago")
                    , button [ class "btn", onClick (EPCFilter (Add item.epc)) ] [ text "Add to Filter" ]
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
        diffMillis =
            Time.posixToMillis later - Time.posixToMillis earlier

        inSeconds =
            Time.millisToPosix diffMillis |> Time.toSecond Time.utc

        diffSecondsStr =
            String.fromInt inSeconds ++ "s"
    in
    diffSecondsStr



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = viewDoc
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
