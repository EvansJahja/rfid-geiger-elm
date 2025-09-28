module VH88WorkingParameters exposing (..)


import Test exposing (..)
import Expect
import VH88.WorkingParameters exposing (..)
import Fifo exposing (Fifo)
import Bytes.Encode
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
        test "Placeholder test" <|
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
                        }
                    encoded = Bytes.Encode.encode (VH88.WorkingParameters.toEncoder workingParameter)
                    asList = BytesHelper.decodeIntList encoded

                in
                Expect.equal (Just [0x4,0x1,0x0,0x0,0x0,0x7,0x3b,0x4,0x0,0x6,0x1,0x9,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x1,0x0,0x0,0x0,0x0,0x0]) asList

    ]

mustResult : Result err a -> a
mustResult result =
    case result of
        Ok value ->
            value

        Err e ->
            Debug.todo (Debug.toString e)