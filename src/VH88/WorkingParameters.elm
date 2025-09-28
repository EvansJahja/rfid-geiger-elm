module VH88.WorkingParameters exposing (..)
import Bytes.Decode
import Bytes.Encode as Encode
import Bytes
import VH88 exposing (PowerLevel)
import VH88 exposing (powerLevelToEncoder)



type alias WorkingParameters =
    {
        tagType : Int,
        outputCarriageReturn : OutputCarriageReturn,
        realTimeOutput : RealTimeOutput,
        minimumCarrierFrequency : Int,
        maximumCarrierFrequency : Int,
        transmitPower : PowerLevel
    }
toEncoder : WorkingParameters -> Encode.Encoder
toEncoder workingParameters =
    Encode.sequence
        [ Encode.unsignedInt8 workingParameters.tagType
        , encodeOutputCarriageReturn workingParameters.outputCarriageReturn
        , encodeRealTimeOutput workingParameters.realTimeOutput
        , Encode.unsignedInt8 0
        , Encode.unsignedInt8 0
        , Encode.unsignedInt8 workingParameters.minimumCarrierFrequency
        , Encode.unsignedInt8 workingParameters.maximumCarrierFrequency
        , powerLevelToEncoder workingParameters.transmitPower

        ]
    
type OutputCarriageReturn = WithoutCarriageReturn | WithCarriageReturn
encodeOutputCarriageReturn : OutputCarriageReturn -> Encode.Encoder
encodeOutputCarriageReturn outputCarriageReturn =
    case outputCarriageReturn of
        WithoutCarriageReturn ->
            Encode.unsignedInt8 0
        WithCarriageReturn ->
            Encode.unsignedInt8 1
        
type RealTimeOutput = OutputHost_StoreUSB
                    | NoOutputHost_StoreUSB
                    | OutputHost_NoStoreUSB
                    | NoOutputHost_NoStoreUSB

encodeRealTimeOutput : RealTimeOutput -> Encode.Encoder
encodeRealTimeOutput realTimeOutput =
    case realTimeOutput of
        OutputHost_StoreUSB ->
            Encode.unsignedInt8 3
        NoOutputHost_StoreUSB ->
            Encode.unsignedInt8 2
        OutputHost_NoStoreUSB ->
            Encode.unsignedInt8 1
        NoOutputHost_NoStoreUSB ->
            Encode.unsignedInt8 0