port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MODEL

type alias Model =
    { receivedData : String }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { receivedData = "No data yet." }
    , Cmd.none
    )

-- MESSAGES

type Msg
    = RequestPort
    | ReceiveData String
    | ReceiveSerialStatus (Maybe SerialStatus)

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

port serialData : (String -> msg) -> Sub msg

port serialStatus: (List String -> msg) -> Sub msg

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestPort ->
            ( model, requestPort () )

        ReceiveData data ->
            ( { model | receivedData = data }, Cmd.none )
        
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
subscriptions model =
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
        [ button [ onClick RequestPort ] [ text "Connect to Serial Port" ]
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