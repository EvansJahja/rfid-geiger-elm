port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, class)
import Json.Decode
import Json.Encode
import VH88
import Html.Attributes exposing (name)
import Html exposing (Attribute)
import Html.Attributes exposing (disabled)
import Device exposing (Device)
import Device exposing (encodeDevice)
import Fifo exposing (Fifo)

-- MODEL


type alias Model =
    { receivedData : String 
    , textToSend : String
    , deviceList : List Device
    , counter : Int
    , selectedDevice : Maybe Device
    , recvBuffer : Fifo.Fifo Int -- FIFO for incoming serial data
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( {
         receivedData = "No data yet."
        , textToSend = ""
        , deviceList = []
        , counter = 0
        , selectedDevice = Nothing
        , recvBuffer = Fifo.empty
         }
    , Cmd.batch 
        [ registerListener ()
        , requestDeviceList ()
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
    | IncrementCounter
    | DebugCmd String
    | DeviceSelected (Maybe Device)
    | Connect Device

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
            ( { model | receivedData = "Received valid packet with contents: " ++ (Debug.toString packet) }, Cmd.none )
        
        SendDummy ->
            case VH88.setRfidPower 5 of
                Ok bytesToSend ->
                    ( model, serialSend bytesToSend )
                
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
            ( model, debugPort debugMsg )
        

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ serialData ReceiveData
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
    div [class "bg-teal-900 min-h-screen text-white p-4"]
        [ div [] [button [ onClick RequestPort ] [ text "Connect to Serial Port" ]]
        , div [] [button [ onClick SendDummy] [text "Send Dummy Command"]]
        , div [] [ input [ placeholder "Enter text..." ] [] ]
        , div [] [ text <| "Received: " ++ model.receivedData ]
        , div [] [ text <| "Counter: " ++ String.fromInt (model.counter) ]
        , div [] [button [ onClick IncrementCounter] [text "Increment Counter"]]
        , div [] [button [ onClick (DebugCmd "GetDeviceList") ] [text "Debug get device list"]]
        , htmlIf (List.isEmpty model.deviceList)
            (Html.h1 [class "text-gray-300 text-3xl font-bold underline"] [ text "No Bluetooth Devices Found" ])
            (div [] 
                [text "Available Bluetooth Devices:"
                , select [onInput ((Device.deviceByAddr model.deviceList >> DeviceSelected) )]
                    (List.map (\device -> option [value device.address] [ text device.name ]) model.deviceList)
                ]
            )
        , 
        
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
        div []
            [ button attr [ text "Connect" ]
            ]
        ]

htmlIf : Bool -> Html msg -> Html msg -> Html msg
htmlIf condition htmlTrue htmlFalse  =
    if condition then
        htmlTrue
    else
        htmlFalse

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }