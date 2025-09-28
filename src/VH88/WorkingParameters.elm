module VH88.WorkingParameters exposing (..)

import Bytes.Decode
import Bytes.Encode as Encode
import Bytes
import VH88 exposing (PowerLevel)
import VH88 exposing (powerLevelToEncoder)



type alias Reserved = 
    { res3 : Int 
    , res4 : Int
    , res12 : Int
    , res13 : Int
    , res14 : Int
    , res15 : Int
    , res16 : Int
    , res17 : Int
    , res18 : Int
    , res20 : Int
    , res28 : Int
    , res29 : Int
    , res30 : Int
    , res31 : Int
    }
defaultReserved : Reserved
defaultReserved = 
    { res3 = 0
    , res4 = 0
    , res12 = 0
    , res13 = 0
    , res14 = 0
    , res15 = 0
    , res16 = 0
    , res17 = 0
    , res18 = 0
    , res20 = 1
    , res28 = 0
    , res29 = 0
    , res30 = 0
    , res31 = 0
    }

type alias WorkingParameters =
    { tagType : Int
    , outputCarriageReturn : OutputCarriageReturn
    , realTimeOutput : RealTimeOutput
    , minimumCarrierFrequency : Int
    , maximumCarrierFrequency : Int
    , transmitPower : PowerLevel
    , hwVersion : Int
    , moduleType : ModuleType
    , reserved : Reserved
    , workingMode : WorkingMode
    , dataOutputMode : DataOutputMode
    , readerVolume : ReaderVolume
    , readingMode : ReadingMode
    , filterDuplicateTags : FilterDuplicateTags
    , language : Language
    , deviceType : DeviceType
    }
toEncoder : WorkingParameters -> Encode.Encoder
toEncoder workingParameters =
    Encode.sequence
        [ Encode.unsignedInt8 workingParameters.tagType
        , encodeOutputCarriageReturn workingParameters.outputCarriageReturn
        , encodeRealTimeOutput workingParameters.realTimeOutput
        , Encode.unsignedInt8 workingParameters.reserved.res3
        , Encode.unsignedInt8 workingParameters.reserved.res4
        , Encode.unsignedInt8 workingParameters.minimumCarrierFrequency
        , Encode.unsignedInt8 workingParameters.maximumCarrierFrequency
        , powerLevelToEncoder workingParameters.transmitPower
        , Encode.unsignedInt32 Bytes.BE workingParameters.hwVersion
        , Encode.unsignedInt8 workingParameters.reserved.res12
        , Encode.unsignedInt8 workingParameters.reserved.res13
        , Encode.unsignedInt8 workingParameters.reserved.res14
        , Encode.unsignedInt8 workingParameters.reserved.res15
        , Encode.unsignedInt8 workingParameters.reserved.res16
        , Encode.unsignedInt8 workingParameters.reserved.res17
        , Encode.unsignedInt8 workingParameters.reserved.res18
        , encodeModuleType workingParameters.moduleType
        , Encode.unsignedInt8 workingParameters.reserved.res20
        , encodeWorkingMode workingParameters.workingMode
        , encodeDataOutputMode workingParameters.dataOutputMode
        , encodeReaderVolume workingParameters.readerVolume
        , encodeReadingMode workingParameters.readingMode
        , encodeFilterDuplicateTags workingParameters.filterDuplicateTags
        , encodeLanguage workingParameters.language
        , encodeDeviceType workingParameters.deviceType
        , Encode.unsignedInt8 workingParameters.reserved.res28
        , Encode.unsignedInt8 workingParameters.reserved.res29
        , Encode.unsignedInt8 workingParameters.reserved.res30
        , Encode.unsignedInt8 workingParameters.reserved.res31
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


type ModuleType = US_R2000 | EU_R2000 | US_5F | EU_5F | US_5S | EU_5S

encodeModuleType : ModuleType -> Encode.Encoder
encodeModuleType moduleType =
    case moduleType of
        US_R2000 ->
            Encode.unsignedInt8 0
        EU_R2000 ->
            Encode.unsignedInt8 1
        US_5F ->
            Encode.unsignedInt8 2
        EU_5F ->
            Encode.unsignedInt8 3
        US_5S ->
            Encode.unsignedInt8 4
        EU_5S ->
            Encode.unsignedInt8 5

type WorkingMode = RFID | Barcode
encodeWorkingMode : WorkingMode -> Encode.Encoder
encodeWorkingMode workingMode =
    case workingMode of
        RFID ->
            Encode.unsignedInt8 0
        Barcode ->
            Encode.unsignedInt8 1

type DataOutputMode = Data | EmulationKeyboard
encodeDataOutputMode : DataOutputMode -> Encode.Encoder
encodeDataOutputMode dataOutputMode =
    case dataOutputMode of
        Data ->
            Encode.unsignedInt8 0
        EmulationKeyboard ->
            Encode.unsignedInt8 1

type ReaderVolume = Mute | Low | Medium | High
encodeReaderVolume : ReaderVolume -> Encode.Encoder
encodeReaderVolume readerVolume =
    case readerVolume of
        Mute ->
            Encode.unsignedInt8 1
        Low ->
            Encode.unsignedInt8 2
        Medium ->
            Encode.unsignedInt8 3
        High ->
            Encode.unsignedInt8 4

type ReadingMode = Single | Continuous
encodeReadingMode : ReadingMode -> Encode.Encoder
encodeReadingMode readingMode =
    case readingMode of
        Single ->
            Encode.unsignedInt8 0
        Continuous ->
            Encode.unsignedInt8 1

type FilterDuplicateTags = Filter | NoFilter
encodeFilterDuplicateTags : FilterDuplicateTags -> Encode.Encoder
encodeFilterDuplicateTags filterDuplicateTags =
    case filterDuplicateTags of
        Filter ->
            Encode.unsignedInt8 1
        NoFilter ->
            Encode.unsignedInt8 0

type Language = English | Chinese
encodeLanguage : Language -> Encode.Encoder
encodeLanguage language =
    case language of
        English ->
            Encode.unsignedInt8 1
        Chinese ->
            Encode.unsignedInt8 0

type DeviceType = VH88 | VH76
encodeDeviceType : DeviceType -> Encode.Encoder
encodeDeviceType deviceType =
    case deviceType of
        VH88 ->
            Encode.unsignedInt8 0
        VH76 ->
            Encode.unsignedInt8 1

        