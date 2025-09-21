port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value)
import SerDe

-- MODEL

type alias Model =
    { receivedData : String 
    , textToSend : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { receivedData = "No data yet.", textToSend = "" }
    , Cmd.none
    )

-- MESSAGES

type Msg
    = RequestPort
    | ReceiveData (List Int)
    | ReceiveSerialStatus (Maybe SerialStatus)
    | SendDummy

type SerialStatus
    = SerialWaitingForUser
    | SerialConnected
    | SerialConnecting
    | SerialError String

-- HELPER FUNCTIONS

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

port requestPort : () -> Cmd msg

port serialSend : (List Int) -> Cmd msg

port serialData : (List Int -> msg) -> Sub msg

port serialStatus: (List String -> msg) -> Sub msg

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestPort ->
            ( model, requestPort () )

        ReceiveData byteList ->
            case SerDe.bytesToEnvelope byteList of
                Ok stringData ->
                    ( { model | receivedData = Debug.toString stringData }, Cmd.none )
                
                Err SerDe.ErrDummyA ->
                    ( { model | receivedData = "Error: Invalid bytes received" }, Cmd.none )
                
                Err SerDe.ErrDummyB ->
                    ( { model | receivedData = "Error: Other dummy error" }, Cmd.none )
        
        SendDummy ->
            let
                dummyCommand = SerDe.CmdSetRFIDPower 5
                dummyPacket = SerDe.Command dummyCommand
                bytesToSend = SerDe.packetToSerial dummyPacket
            in
            ( model, serialSend bytesToSend )

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
        

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ serialData ReceiveData
        , serialStatus (\rawList ->
            ReceiveSerialStatus (decodeSerialStatus rawList)
          )
        ]

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [] [button [ onClick RequestPort ] [ text "Connect to Serial Port" ]]
        , div [] [button [ onClick SendDummy] [text "Send Dummy Command"]]
        , div [] [ text <| "Received: " ++ model.receivedData ]
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }