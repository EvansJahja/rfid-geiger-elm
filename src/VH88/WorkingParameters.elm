module VH88.WorkingParameters exposing (..)

import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode

import Bytes.Decode
import Bytes.Encode as Encode
import Bytes


type alias WorkingParameters =
    { tagType : Int
    , outputCarriageReturn : OutputCarriageReturn
    , realTimeOutput : RealTimeOutput
    , minimumCarrierFrequency : Int
    , maximumCarrierFrequency : Int
    , transmitPower : Int
    , hwVersion : Int
    , moduleType : ModuleType
    , workingMode : WorkingMode
    , dataOutputMode : DataOutputMode
    , readerVolume : ReaderVolume
    , readingMode : ReadingMode
    , filterDuplicateTags : FilterDuplicateTags
    , language : Language
    , deviceType : DeviceType

    -- reserved fields
    , res3 : Int 
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
toEncoder : WorkingParameters -> Encode.Encoder
toEncoder workingParameters =
    Encode.sequence
        [ Encode.unsignedInt8 workingParameters.tagType
        , encodeOutputCarriageReturn workingParameters.outputCarriageReturn
        , encodeRealTimeOutput workingParameters.realTimeOutput
        , Encode.unsignedInt8 workingParameters.res3
        , Encode.unsignedInt8 workingParameters.res4
        , Encode.unsignedInt8 workingParameters.minimumCarrierFrequency
        , Encode.unsignedInt8 workingParameters.maximumCarrierFrequency
        , Encode.unsignedInt8 workingParameters.transmitPower
        , Encode.unsignedInt32 Bytes.BE workingParameters.hwVersion
        , Encode.unsignedInt8 workingParameters.res12
        , Encode.unsignedInt8 workingParameters.res13
        , Encode.unsignedInt8 workingParameters.res14
        , Encode.unsignedInt8 workingParameters.res15
        , Encode.unsignedInt8 workingParameters.res16
        , Encode.unsignedInt8 workingParameters.res17
        , Encode.unsignedInt8 workingParameters.res18
        , encodeModuleType workingParameters.moduleType
        , Encode.unsignedInt8 workingParameters.res20
        , encodeWorkingMode workingParameters.workingMode
        , encodeDataOutputMode workingParameters.dataOutputMode
        , encodeReaderVolume workingParameters.readerVolume
        , encodeReadingMode workingParameters.readingMode
        , encodeFilterDuplicateTags workingParameters.filterDuplicateTags
        , encodeLanguage workingParameters.language
        , encodeDeviceType workingParameters.deviceType
        , Encode.unsignedInt8 workingParameters.res28
        , Encode.unsignedInt8 workingParameters.res29
        , Encode.unsignedInt8 workingParameters.res30
        , Encode.unsignedInt8 workingParameters.res31
        ]



toDecoder : Bytes.Decode.Decoder WorkingParameters
toDecoder =
    Decode.map4
        (\firstBlock secondBlock thirdBlock fourthBlock -> 
            { tagType = firstBlock.tagType
            , outputCarriageReturn = firstBlock.outputCarriageReturn
            , realTimeOutput = firstBlock.realTimeOutput
            , res3 = firstBlock.res3_4.res3
            , res4 = firstBlock.res3_4.res4
            , minimumCarrierFrequency = secondBlock.minCarrierFreq
            , maximumCarrierFrequency = secondBlock.maxCarrierFreq
            , transmitPower = secondBlock.transmitPower
            , hwVersion = secondBlock.hwVersion
            , res12 = secondBlock.res12_18.res12
            , res13 = secondBlock.res12_18.res13
            , res14 = secondBlock.res12_18.res14
            , res15 = secondBlock.res12_18.res15
            , res16 = secondBlock.res12_18.res16
            , res17 = secondBlock.res12_18.res17
            , res18 = secondBlock.res12_18.res18
            , moduleType = thirdBlock.moduleType
            , res20 = thirdBlock.res20
            , workingMode = thirdBlock.workingMode
            , dataOutputMode = thirdBlock.dataOutputMode
            , readerVolume = thirdBlock.readerVolume
            , readingMode = fourthBlock.readingMode
            , filterDuplicateTags = fourthBlock.filterDuplicateTags
            , language = fourthBlock.language
            , deviceType = fourthBlock.deviceType
            , res28 = fourthBlock.res28_31.res28
            , res29 = fourthBlock.res28_31.res29
            , res30 = fourthBlock.res28_31.res30
            , res31 = fourthBlock.res28_31.res31
            }
        )
        decodeFirstBlock
        decodeSecondBlock
        decodeThirdBlock
        decodeFourthBlock

type alias Res3_4 = { res3 : Int, res4 : Int }
type alias FirstBlock = 
    { tagType : Int
    , outputCarriageReturn : OutputCarriageReturn
    , realTimeOutput : RealTimeOutput
    , res3_4 : Res3_4
    }

decodeFirstBlock : Bytes.Decode.Decoder FirstBlock
decodeFirstBlock = 
    Decode.map4
        FirstBlock
        (Bytes.Decode.unsignedInt8)
        decodeOutputCarriageReturn
        decodeRealTimeOutput
        (
        Decode.map2
                Res3_4
                (Decode.unsignedInt8)
                (Decode.unsignedInt8)
        )

type alias SecondBlock = 
    { minCarrierFreq : Int
    , maxCarrierFreq : Int
    , transmitPower : Int
    , hwVersion : Int
    , res12_18 : Res12_18
    }

decodeSecondBlock : Bytes.Decode.Decoder SecondBlock
decodeSecondBlock = 
    Decode.map5
        SecondBlock
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)
        (Decode.unsignedInt32 Bytes.BE)
        decodeRes12_18

type alias Res12_18 = 
    { res12 : Int
    , res13 : Int
    , res14 : Int
    , res15 : Int
    , res16 : Int
    , res17 : Int
    , res18 : Int
    }

decodeRes12_18 : Bytes.Decode.Decoder Res12_18
decodeRes12_18 = 
    Decode.map4
        (\res12 res13 res14 res15 -> 
            { res12 = res12
            , res13 = res13
            , res14 = res14
            , res15 = res15
            }
        )
        
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)
        |> Decode.andThen
            (\partialRes ->
                Decode.map3
                    (\res16 res17 res18 ->
                        { res12 = partialRes.res12
                        , res13 = partialRes.res13
                        , res14 = partialRes.res14
                        , res15 = partialRes.res15
                        , res16 = res16
                        , res17 = res17
                        , res18 = res18
                        }
                    )
                    (Decode.unsignedInt8)
                    (Decode.unsignedInt8)
                    (Decode.unsignedInt8)
            )


type alias ThridBlock = 
    { moduleType : ModuleType
    , res20 : Int
    , workingMode : WorkingMode
    , dataOutputMode : DataOutputMode
    , readerVolume : ReaderVolume
    }
decodeThirdBlock : Bytes.Decode.Decoder ThridBlock
decodeThirdBlock = 
    Decode.map5 ThridBlock
        decodeModuleType
        (Decode.unsignedInt8)
        decodeWorkingMode
        decodeDataOutputMode
        decodeReaderVolume

type alias FourthBlock = 
    { readingMode : ReadingMode
    , filterDuplicateTags : FilterDuplicateTags
    , language : Language
    , deviceType : DeviceType
    , res28_31 : Res28_Res31
    }
decodeFourthBlock : Bytes.Decode.Decoder FourthBlock
decodeFourthBlock = 
    Decode.map5 FourthBlock
        decodeReadingMode
        decodeFilterDuplicateTags
        decodeLanguage
        decodeDeviceType
        decodeRes28_31


decodeRes28_31 : Bytes.Decode.Decoder Res28_Res31
decodeRes28_31 = 
    Decode.map4 Res28_Res31
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)
        (Decode.unsignedInt8)

type alias Res28_Res31 =
    { res28 : Int
    , res29 : Int
    , res30 : Int
    , res31 : Int
    }

--

type OutputCarriageReturn = WithoutCarriageReturn | WithCarriageReturn
encodeOutputCarriageReturn : OutputCarriageReturn -> Encode.Encoder
encodeOutputCarriageReturn outputCarriageReturn =
    case outputCarriageReturn of
        WithoutCarriageReturn ->
            Encode.unsignedInt8 0
        WithCarriageReturn ->
            Encode.unsignedInt8 1

decodeOutputCarriageReturn : Decode.Decoder OutputCarriageReturn
decodeOutputCarriageReturn =
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    0 ->
                        Decode.succeed WithoutCarriageReturn
                    1 ->
                        Decode.succeed WithCarriageReturn
                    _ ->
                        Decode.fail
            )
        
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

decodeRealTimeOutput : Decode.Decoder RealTimeOutput
decodeRealTimeOutput = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    3 ->
                        Decode.succeed OutputHost_StoreUSB
                    2 ->
                        Decode.succeed NoOutputHost_StoreUSB
                    1 ->
                        Decode.succeed OutputHost_NoStoreUSB
                    0 ->
                        Decode.succeed NoOutputHost_NoStoreUSB
                    _ ->
                        Decode.fail
            )

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

decodeModuleType : Decode.Decoder ModuleType
decodeModuleType =
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    0 ->
                        Decode.succeed US_R2000
                    1 ->
                        Decode.succeed EU_R2000
                    2 ->
                        Decode.succeed US_5F
                    3 ->
                        Decode.succeed EU_5F
                    4 ->
                        Decode.succeed US_5S
                    5 ->
                        Decode.succeed EU_5S
                    _ ->
                        Decode.fail
            )

type WorkingMode = RFID | Barcode
encodeWorkingMode : WorkingMode -> Encode.Encoder
encodeWorkingMode workingMode =
    case workingMode of
        RFID ->
            Encode.unsignedInt8 0
        Barcode ->
            Encode.unsignedInt8 1
decodeWorkingMode : Decode.Decoder WorkingMode
decodeWorkingMode = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    0 ->
                        Decode.succeed RFID
                    1 ->
                        Decode.succeed Barcode
                    _ ->
                        Decode.fail
            )

type DataOutputMode = Data | EmulationKeyboard
encodeDataOutputMode : DataOutputMode -> Encode.Encoder
encodeDataOutputMode dataOutputMode =
    case dataOutputMode of
        Data ->
            Encode.unsignedInt8 0
        EmulationKeyboard ->
            Encode.unsignedInt8 1
decodeDataOutputMode : Decode.Decoder DataOutputMode
decodeDataOutputMode = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    0 ->
                        Decode.succeed Data
                    1 ->
                        Decode.succeed EmulationKeyboard
                    _ ->
                        Decode.fail
            )

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
decodeReaderVolume : Decode.Decoder ReaderVolume
decodeReaderVolume = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    1 ->
                        Decode.succeed Mute
                    2 ->
                        Decode.succeed Low
                    3 ->
                        Decode.succeed Medium
                    4 ->
                        Decode.succeed High
                    _ ->
                        Decode.fail
            )

type ReadingMode = Single | Continuous
encodeReadingMode : ReadingMode -> Encode.Encoder
encodeReadingMode readingMode =
    case readingMode of
        Single ->
            Encode.unsignedInt8 0
        Continuous ->
            Encode.unsignedInt8 1
decodeReadingMode : Decode.Decoder ReadingMode
decodeReadingMode = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    0 ->
                        Decode.succeed Single
                    1 ->
                        Decode.succeed Continuous
                    _ ->
                        Decode.fail
            )

type FilterDuplicateTags = Filter | NoFilter
encodeFilterDuplicateTags : FilterDuplicateTags -> Encode.Encoder
encodeFilterDuplicateTags filterDuplicateTags =
    case filterDuplicateTags of
        Filter ->
            Encode.unsignedInt8 1
        NoFilter ->
            Encode.unsignedInt8 0
decodeFilterDuplicateTags : Decode.Decoder FilterDuplicateTags
decodeFilterDuplicateTags = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    1 ->
                        Decode.succeed Filter
                    0 ->
                        Decode.succeed NoFilter
                    _ ->
                        Decode.fail
            )

type Language = English | Chinese
encodeLanguage : Language -> Encode.Encoder
encodeLanguage language =
    case language of
        English ->
            Encode.unsignedInt8 1
        Chinese ->
            Encode.unsignedInt8 0
decodeLanguage : Decode.Decoder Language
decodeLanguage = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    1 ->
                        Decode.succeed English
                    0 ->
                        Decode.succeed Chinese
                    _ ->
                        Decode.fail
            )

type DeviceType = VH88 | VH76
encodeDeviceType : DeviceType -> Encode.Encoder
encodeDeviceType deviceType =
    case deviceType of
        VH88 ->
            Encode.unsignedInt8 0
        VH76 ->
            Encode.unsignedInt8 1

decodeDeviceType : Decode.Decoder DeviceType
decodeDeviceType = 
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                case value of
                    0 ->
                        Decode.succeed VH88
                    1 ->
                        Decode.succeed VH76
                    _ ->
                        Decode.fail
            )

        