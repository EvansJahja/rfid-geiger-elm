port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, select, option, h1)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Html.Attributes as Attrs
import Json.Decode
import Json.Encode
import VH88
import Device exposing (Device)
import Device exposing (encodeDevice)
import Fifo exposing (Fifo)
import Html exposing (p)
import Html exposing (ul)
import Time
import Task
import Dict exposing (Dict)
import Dict exposing (diff)
import Html exposing (label)
import Html.Events exposing (onCheck)
import Html.Events exposing (onMouseUp)
import Html.Events exposing (onBlur)
import Html.Events exposing (on)
import Json.Decode as Decode

-- MODEL

type alias Model =
    { time : Time.Posix
    , receivedData : String
    , textToSend : String
    , deviceList : List Device
    , counter : Int
    , selectedDevice : Maybe Device
    , recvBuffer : Fifo.Fifo Int -- FIFO for incoming serial data
    , pendingCommand: VH88.PendingCommand
    , errorCommand : VH88.ErrorCommand
    , showDebug : Bool
    , powerLevel : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    (   { time = Time.millisToPosix 0
        , receivedData = "No data yet."
        , textToSend = ""
        , deviceList = []
        , counter = 0
        , selectedDevice = Nothing
        , recvBuffer = Fifo.empty
        , pendingCommand = VH88.newPendingCommand
        , errorCommand = VH88.newErrorCommand
        , showDebug = False
        , powerLevel = 0
         }
    , Cmd.batch 
        [ registerListener ()
        , requestDeviceList ()
        , Time.now |> Task.perform Tick
        ]
    )

-- MESSAGES

type Msg
    = RequestPort
    | ReceiveData (List Int)
    | ReceivePacket VH88.VH88Packet
    | ReceiveSerialStatus (Maybe SerialStatus)
    | ReceiveDeviceList (List Device)
    | RequestDeviceList
    | SendDummy
    | SendCommand VH88.Command 
    | IncrementCounter
    | DebugCmd String
    | DebugToggle Bool
    | DeviceSelected (Maybe Device)
    | Connect Device
    | Tick Time.Posix
    | SetPowerLevel Int

type SerialStatus
    = SerialWaitingForUser
    | SerialConnected
    | SerialConnecting
    | SerialError String

-- HELPER FUNCTIONS

{-| Recursively parse all complete packets from the buffer
Returns (list of packets, remaining buffer)
-}
parseAllPackets : Fifo Int -> List VH88.VH88Packet -> (List VH88.VH88Packet, Fifo Int)
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
processAllPackets : List VH88.VH88Packet -> Model -> (Model, Cmd Msg)
processAllPackets packets model =
    case packets of
        [] ->
            (model, Cmd.none)
        
        packet :: remainingPackets ->
            let
                (newModel, cmd) = update (ReceivePacket packet) model
                (finalModel, finalCmd) = processAllPackets remainingPackets newModel
            in
                (finalModel, Cmd.batch [cmd, finalCmd])

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

port requestPort : () -> Cmd msg

port serialSend : (List Int) -> Cmd msg

port serialData : (List Int -> msg) -> Sub msg

port serialStatus : (List String -> msg) -> Sub msg

port deviceList : (Json.Decode.Value -> msg) -> Sub msg


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestPort ->
            ( model, requestPort () )

        ReceiveData byteList ->
            let
                appendedBuffer = List.foldl Fifo.insert model.recvBuffer byteList
                (packets, finalBuffer) = parseAllPackets appendedBuffer []
            in
                case packets of
                    [] ->
                        -- No complete packets found, keep the buffer unchanged
                        ( { model | recvBuffer = finalBuffer }, Cmd.none )
                    
                    _ ->
                        -- Process all packets sequentially
                        processAllPackets packets { model | recvBuffer = finalBuffer }

        ReceivePacket packet ->
            let
                updatedModel = case packet of
                    VH88.Response commandData ->
                        { model 
                        | pendingCommand = VH88.removePendingCommand model.pendingCommand commandData.command 
                        , errorCommand = VH88.removeErrorCommand model.errorCommand commandData.command -- remove from error as well
                        }
                    
                    VH88.Error commandData ->
                        let
                            errorCode = List.head commandData.params |> Maybe.withDefault 0x20 |> VH88.errorCodeFromInt
                        in
                            { model 
                            | pendingCommand = VH88.removePendingCommand model.pendingCommand commandData.command 
                            , errorCommand = VH88.addErrorCommand model.errorCommand model.time commandData.command errorCode
                            }

                    VH88.Status _ ->
                        model  -- Don't remove pending commands for status packets
            in
                ( { updatedModel | receivedData = "Received valid packet with contents: " ++ (Debug.toString packet) }, Cmd.none )
        
        SendDummy ->
            case VH88.setRfidPower 5 of
                Ok bytesToSend ->
                    let 
                        pendingCommand = addPendingCommand model VH88.cmdSetRFIDPower 
                    in
                        ( { model | pendingCommand = pendingCommand }, serialSend bytesToSend )
                
                Err errorMsg ->
                    ( { model | receivedData = "Command error: " ++ errorMsg }, Cmd.none )
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

        ReceiveSerialStatus maybeStatus ->
            case maybeStatus of
                Just status ->
                    case status of
                        SerialWaitingForUser ->
                            ( { model | receivedData = "Status: Waiting for user." }, Cmd.none )
                        
                        SerialConnected ->
                            ( { model | receivedData = "Status: Connected!" }, Cmd.none )
                        
                        SerialConnecting ->
                            ( { model | receivedData = "Status: Connecting..." }, Cmd.none )

                        SerialError errMsg ->
                            ( { model | receivedData = "Status: Error - " ++ errMsg }, Cmd.none )
                
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
        SendCommand command ->
            case command of 
                VH88.SetRfidPower powerLevel ->
                    case VH88.commandToBytes command of
                        Ok cmdBytes ->
                            let
                                pendingCommand = addPendingCommand model VH88.cmdSetRFIDPower
                            in
                                ( { model | pendingCommand = pendingCommand, powerLevel = powerLevel }, serialSend cmdBytes )
                
                        Err errorMsg ->
                            ( { model | receivedData = "Command error: " ++ errorMsg }, Cmd.none )
                _ -> Debug.todo "Implement other commands as needed"
        SetPowerLevel powerLevel ->
            ( { model | powerLevel = powerLevel }, Cmd.none )

                

addPendingCommand : Model -> VH88.CommandByte -> VH88.PendingCommand
addPendingCommand model cmdByte =
    VH88.addPendingCommand model.pendingCommand model.time cmdByte
        

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
                Err err ->
                    Debug.todo "Implement error handling for device list decoding"
          )
        ]

-- VIEW

view : Model -> Html Msg
view model =
    div [class "bg-teal-900 flex flex-col min-h-screen text-white p-4"]
        [ div [] [button [ onClick RequestPort ] [ text "Connect to Serial Port" ]]
        , div [] [button [ onClick SendDummy] [text "Send Dummy Command"]]
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
            [ htmlIf (List.isEmpty model.deviceList)
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
    viewPanel ("Power level: " ++ (String.fromInt model.powerLevel))
    [ input [type_ "range", Attrs.min "0", Attrs.max "33", Attrs.value (String.fromInt model.powerLevel), on "pointerup" (Decode.succeed (SendCommand (VH88.SetRfidPower model.powerLevel))), onInput (SetPowerLevel << Maybe.withDefault 0 << String.toInt)] []

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
        , button [ onClick (SendDummy ) ] [text "Send "]
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
                        Html.li [] [ text ("Command Byte: " ++ String.fromInt cmd.commandByte ++ ", Pending for: " ++ humanTimeDifference cmd.sentTime model.time) ]
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
                        Html.li [] [ text ("Command Byte: " ++ String.fromInt cmd.commandByte ++ ", Error code: " ++ VH88.errorCodeToString cmd.errorCode ++ ", Error at: " ++ humanTimeDifference cmd.sentTime model.time ++ " ago" )]
                    )
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

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }