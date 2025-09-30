port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, select, option, h1, ul, label, p)
import Html.Events exposing (onClick, onInput, onCheck, on)
import Html.Attributes exposing (..)
import Html.Attributes as Attrs
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
import Json.Decode as Decode
import VH88.Command as Command
import VH88.Packet as Packet
import Platform.Cmd as Cmd
import BytesHelper
import VH88.WorkingParameters as WorkingParameters

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

type EPC =
    EPC (List Int)

type alias Inventory =
    { epc : EPC
    , lastSeen : Time.Posix
    }

-- MODEL

type alias Model =
    { time : Time.Posix
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
    }


init : (Json.Decode.Value) -> ( Model, Cmd Msg )
init flags =
    let
        platform = Decode.decodeValue (Decode.field "platform" Decode.string) flags |> Result.withDefault "unknown"
        initCmd = case platform of
            "web" ->
                Cmd.batch
                    [ registerListener ()
                    -- , requestDeviceList () -- don't request device list automatically on web, need user gesture
                    , Time.now |> Task.perform Tick
                    ]
            _ -> 
                Cmd.batch
                    [ registerListener ()
                    , requestDeviceList ()
                    , Time.now |> Task.perform Tick
                    ]
        
    in
        (   { time = Time.millisToPosix 0
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
                        epc = EPC args
                        
                    in
                        (model, Cmd.none)
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
subscriptions _ =
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
        ]

-- VIEW

view : Model -> Html Msg
view model =
    div [class "bg-teal-900 flex flex-col min-h-screen text-white p-4"]
        [ Html.p [] [text ("Platform: " ++ model.platform)]
        , div [] [button [ onClick RequestDeviceList ] [ text "Connect to Serial Port" ]]
        , div [] [ input [ placeholder "Enter text..." ] [] ]
        , div [] [ text <| "Received: " ++ model.receivedData ]
        , div [] [ text <| "Counter: " ++ String.fromInt (model.counter) ]
        , div [] [button [ onClick IncrementCounter] [text "Increment Counter"]]
        , div []
            [ input [ id "debug-checkbox", type_ "checkbox", onCheck (DebugToggle), class "w-4 h-4 text-blue-600 bg-gray-100 border-gray-300 rounded-sm focus:ring-blue-500 dark:focus:ring-blue-600 dark:ring-offset-gray-800 focus:ring-2 dark:bg-gray-700 dark:border-gray-600"] []
            , label [ for "debug-checkbox", class "ms-2 text-sm font-medium text-gray-900 dark:text-gray-300"] [ text "Enable Debugging" ]]
        , htmlIf model.showDebug (viewDebugCmd model) (div [][])
        , viewPanelConnection model
        , viewPanelRfidPower model
        , case model.editWorkingParameters of
            Just ewp ->
                viewPanelWorkingParameter ewp
            Nothing ->
                button [onClick (SetEditWorkingParameters model.workingParameters)] [ text "Edit Working Parameters"]
        ]

viewPanel : String -> (List (Html msg)) -> Html msg
viewPanel title contents = 
    div [class "my-4 p-4 border border-gray-500 rounded-xl bg-teal-800"]
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
                                [ onClick (Connect device) ]

                            Nothing ->
                                []
                    )
        in
            viewPanel "Connection"
            [ p [] [text ("Connection status: " ++ serialStatusToString model.serialStatus)]
            , htmlIf (List.isEmpty model.deviceList)
                            (Html.h1 [class "text-gray-300 text-3xl font-bold underline"] [ text "No Bluetooth Devices Found" ])
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
        [ button [ onClick (ReceiveDeviceList [Device "Device A" "00:00:00:00:00:01", Device "Device B" "00:00:00:00:00:02"]) ] [text "Debug get device list"]
        , button [ onClick (ReceiveData [0xf0, 0x03, 0x04, 0x00, 0x09] ) ] [text "Receive successful cmd 0x4"]
        , button [ onClick (ReceiveData [0xf4, 0x03, 0x04, 0x11, 0xf4] ) ] [text "Receive error cmd 0x4"]
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
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }