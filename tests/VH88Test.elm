module VH88Test exposing (..)

import Test exposing (..)
import Expect
import VH88 
import Fifo exposing (Fifo)
import VH88.Packet as Packet
import VH88.Command as Command exposing (CommandWithArgs(..), Command(..))

suite : Test
suite =
    describe "VH88 Tests"
        [ describe "Test for remaining bytes in FIFO after extracting packet"
            [ test "Sanity check" <|
                \_ ->
                let
                    fifo = Fifo.fromList [0x40, 0x02, 0x03, 0xBB]
                    (_, nextFifo) = VH88.fifoBytesToPacket fifo
                in
                    Expect.equal 0 (List.length (Fifo.toList nextFifo))
                
            , test "Excess packet bytes remain in FIFO" <|
                \_ ->
                let
                    fifo = Fifo.fromList [0x40, 0x02, 0x03, 0xBB, 0x55, 0x66]
                    (_, nextFifo) = VH88.fifoBytesToPacket fifo
                in
                    Expect.equal [0x55, 0x66] (Fifo.toList nextFifo)
            , test "Incomplete packet remains in FIFO" <|
                \_ ->
                let
                    fifo = Fifo.fromList [0x40, 0x02, 0x03]
                    (_, nextFifo) = VH88.fifoBytesToPacket fifo
                in
                    Expect.equal [0x40, 0x02, 0x03] (Fifo.toList nextFifo)
            , test "Fifo must discard bytes before boot code" <|
                \_ ->
                -- valid boot code are 0x40, 0xF0, 0xF4, or 0xF1
                let
                    fifo = Fifo.fromList [0x00, 0x11, 0x22, 0x33, 0x40, 0x02, 0x03, 0xBB]
                    (_, nextFifo) = VH88.fifoBytesToPacket fifo
                in
                    Expect.equal 0 (List.length (Fifo.toList nextFifo))
            ]
        , describe "Test command packets"
            [
                test "Serialize SetRFIDPower 5" <|
                    \_ ->
                    case VH88.setRFIDPower 5 |> Result.map Packet.packetToBytes of
                        Ok commandBytes ->
                            Expect.equal [0x40, 0x03, 0x04, 0x05, 0xb4] commandBytes
                        
                        Err _ ->
                            Expect.fail "Command should succeed"
                            
                , test "SetRFIDPower validation - invalid power" <|
                    \_ ->
                    case VH88.setRFIDPower 50 of
                            Ok _ -> Expect.fail "Should reject power > 33"
                            Err msg -> Expect.equal "Power level must be between 0 and 33" msg

            ]
        , describe "Test receiving packets"
            [
                test "Deserialize real time reading" <|
                    \_ ->
                    let
                        bytes = [ 240 , 14 , 225 , 226 , 0 , 71 , 24 , 184 , 48 , 100 , 38 , 123 , 194 , 1 , 12 , 36 , 241 , 3 , 1 , 0 , 11 ]
                        fifo = Fifo.fromList bytes
                        (resultPacket, _) = VH88.fifoBytesToPacket fifo
                    in
                    case resultPacket of
                        Ok (Packet.Response (Command.CommandWithArgs (cmd, args))) ->
                            Expect.equal (Command.HostComputerCardReading) cmd

                        _ ->
                            Expect.fail "Fail to parse packet"
            ]
        ]

{-- [
        240,
        14,
        225,
        226,
        0,
        71,
        24,
        184,
        48,
        100,
        38,
        123,
        194,
        1,
        12,
        36,
        241,
        3,
        1,
        0,
        11
    ]

    [
    240,
    14,
    225,
    226,
    0,
    71,
    24,
    184,
    48,
    100,
    38,
    123,
    194,
    1,
    12,
    36
]
--}