module VH88WorkingParameters exposing (..)


import Test exposing (..)
import Expect
import VH88.WorkingParameters exposing (..)
import Fifo exposing (Fifo)
import Bytes.Encode
import Bytes.Decode
import VH88.WorkingParameters as WorkingParameters
import BytesHelper
import Debug
import VH88

-- Sample working parameters
-- [0x4,0x1,0x0,0x0,0x0,0x7,0x3b,0x4,0x0,0x6,0x1,0x9,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x0,0x0,0x0]
-- 32 bytes

suite : Test
suite =
    describe "Working Parameters"
    [
        test "Test encode working parameters" <|
            \_ ->
                let
                    
                    workingParameter : VH88.WorkingParameters.WorkingParameters
                    workingParameter =
                        { tagType = 4
                        , outputCarriageReturn = WithCarriageReturn
                        , realTimeOutput = NoOutputHost_NoStoreUSB
                        , minimumCarrierFrequency = 7
                        , maximumCarrierFrequency = 59
                        , transmitPower = VH88.powerLevel 4 |> mustResult
                        , hwVersion = 0x00060109
                        , moduleType = US_R2000
                        , deviceType = VH88
                        , filterDuplicateTags = NoFilter
                        , language = English
                        , workingMode = RFID
                        , dataOutputMode = Data
                        , readerVolume = Mute
                        , readingMode = Single

                        -- reserved fields
                        , res3 = 0
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
                    encoded = Bytes.Encode.encode (WorkingParameters.toEncoder workingParameter)
                    asList = BytesHelper.decodeIntList encoded

                in
                Expect.equal (Just [0x4,0x1,0x0,0x0,0x0,0x7,0x3b,0x4,0x0,0x6,0x1,0x9,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x0,0x0,0x0]) asList
        , test "Test decode working parameters" <|
            \_ ->
                let
                    bytes = BytesHelper.encodeIntList [0x4,0x1,0x0,0x0,0x0,0x7,0x3b,0x4,0x0,0x6,0x1,0x9,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x0,0x0,0x0]
                    expected : WorkingParameters
                    expected =
                        { tagType = 4
                        , outputCarriageReturn = WithCarriageReturn
                        , realTimeOutput = NoOutputHost_NoStoreUSB
                        , minimumCarrierFrequency = 7
                        , maximumCarrierFrequency = 59
                        , transmitPower = VH88.powerLevel 4 |> mustResult
                        , hwVersion = 0x00060109
                        , moduleType = US_R2000
                        , deviceType = VH88
                        , filterDuplicateTags = NoFilter
                        , language = English
                        , workingMode = RFID
                        , dataOutputMode = Data
                        , readerVolume = Mute
                        , readingMode = Single

                        -- reserved fields
                        , res3 = 0
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
                in
                    Expect.equal (Just expected) (Bytes.Decode.decode VH88.WorkingParameters.toDecoder bytes)
    ]

mustResult : Result err a -> a
mustResult result =
    case result of
        Ok value ->
            value

        Err e ->
            Debug.todo (Debug.toString e)