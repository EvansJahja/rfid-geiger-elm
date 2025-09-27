module SerDeTest exposing (..)

import Test exposing (..)
import Expect
import SerDe 
import SerDe.Commands
import Fifo exposing (Fifo)

suite : Test
suite =
    describe "SerDe Tests"
        [ describe "Test for remaining bytes in FIFO after extracting packet"
            [ test "Sanity check" <|
                \_ ->
                let
                    fifo = Fifo.fromList [0x40, 0x02, 0x03, 0xBB]
                    (_, nextFifo) = SerDe.fifoBytesToPacket fifo
                in
                    Expect.equal 0 (List.length (Fifo.toList nextFifo))
                
            , test "Excess packet bytes remain in FIFO" <|
                \_ ->
                let
                    fifo = Fifo.fromList [0x40, 0x02, 0x03, 0xBB, 0x55, 0x66]
                    (_, nextFifo) = SerDe.fifoBytesToPacket fifo
                in
                    Expect.equal [0x55, 0x66] (Fifo.toList nextFifo)
            , test "Incomplete packet remains in FIFO" <|
                \_ ->
                let
                    fifo = Fifo.fromList [0x40, 0x02, 0x03]
                    (_, nextFifo) = SerDe.fifoBytesToPacket fifo
                in
                    Expect.equal [0x40, 0x02, 0x03] (Fifo.toList nextFifo)
            , test "Fifo must discard bytes before boot code" <|
                \_ ->
                -- valid boot code are 0x40, 0xF0, 0xF4, or 0xF1
                let
                    fifo = Fifo.fromList [0x00, 0x11, 0x22, 0x33, 0x40, 0x02, 0x03, 0xBB]
                    (_, nextFifo) = SerDe.fifoBytesToPacket fifo
                in
                    Expect.equal 0 (List.length (Fifo.toList nextFifo))

            ]
        , describe "Test command packets"
            [
                test "Serialize CmdSetRFIDPower 5" <|
                    \_ ->
                    let
                        commandBytes = SerDe.Commands.setRFIDPower 5
                    in
                        Expect.equal [0x40, 0x03, 0x04, 0x05, 0xb4] commandBytes

            ]
        ]