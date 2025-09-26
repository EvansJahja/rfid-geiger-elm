module Device exposing (..)

import Json.Decode
import Json.Encode

-- Model
type alias Device = 
    { name : String
    , address : String
    }

-- Encoder
encodeDevice : Device -> Json.Encode.Value
encodeDevice device = 
    Json.Encode.object
        [ ("name", Json.Encode.string device.name)
        , ("address", Json.Encode.string device.address)
        ]

decodeDevice : Json.Decode.Decoder  Device
decodeDevice = 
    Json.Decode.map2 Device
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "address" Json.Decode.string)

decodeDeviceList : Json.Decode.Decoder (List Device)
decodeDeviceList =
    Json.Decode.list decodeDevice

deviceByAddr : List Device -> String -> Maybe Device
deviceByAddr devices addr =
    case List.filter (\d -> d.address == addr) devices of
        [] -> Nothing
        first :: _ -> Just first


