module VH88.Packet exposing 
    ( Packet(..) 
    , packetToBytes
    , packetContents
    , bytesToEnvelope
    , envelopeToPacket
    )

import VH88.Command as Command exposing (CommandWithArgs(..))
import Fifo exposing (Fifo)
import VH88.Error exposing (Error(..), ErrorCode(..))
import Bitwise

type Packet
    = Request Command.CommandWithArgs
    | Response Command.CommandWithArgs
    | Error Command.CommandWithArgs
    | Status Command.CommandWithArgs

packetToBootCode : Packet -> Int
packetToBootCode packet =
    case packet of
        Request _ ->
            0x40

        Response _ ->
            0xF0

        Error _ ->
            0xF4

        Status _ ->
            0xF1
packetContents : Packet -> (Command.Command, List Int)
packetContents packet =
    case packet of
        Request (CommandWithArgs (cmd, args)) ->
            (cmd, args)

        Response (CommandWithArgs (cmd, args)) ->
            (cmd, args)

        Error (CommandWithArgs (cmd, args)) ->
            (cmd, args)

        Status (CommandWithArgs (cmd, args)) ->
            (cmd, args)

packetToBytes : Packet -> List Int
packetToBytes packet =
    let
        cmdBootCode = packetToBootCode packet
        (cmd, cmdParams) = packetContents packet
        cmdAsInt = Command.commandToInt cmd
        envelopeContents =  cmdAsInt :: cmdParams
        envelope = createEnvelope cmdBootCode envelopeContents
    in
    envelopeToBytes envelope


type alias Envelope = { boot: Int, length: Int, contents: List Int, checksum: Int }

-- ENVELOPES
envelopeToBytes : Envelope -> List Int
envelopeToBytes envelope =
    envelope.boot :: envelope.length :: envelope.contents ++ [ envelope.checksum ]

-- -- Notes on fifo
-- -- We use a FIFO to store incoming bytes from the serial port
-- -- and we return the remaining bytes in the FIFO after extracting a packet
-- -- or original FIFO if no complete packet is found
bytesToEnvelope : (Fifo Int) -> (Result Error Envelope, Fifo Int)
bytesToEnvelope potentiallyNotBootCodeAlignedFifo = 
    let
        alignedFifo = alignBootCode potentiallyNotBootCodeAlignedFifo

        -- here, we know that bytes start with a valid boot code
        bytes = Fifo.toList alignedFifo
    in
        case bytes of
            boot :: length :: _ ->
                let
                    totalLength = length + 2 -- +2 for boot and length bytes
                    maybePacketBytes = List.take totalLength bytes
                in
                if Debug.log "List length" (List.length maybePacketBytes) == Debug.log "total Length" totalLength then
                    let
                        contents = List.drop 2 maybePacketBytes |> List.take (length - 1)
                        checksum = List.drop (totalLength - 1) maybePacketBytes |> List.head |> Maybe.withDefault 0
                        calculatedChecksum = calculateChecksum (boot :: length :: contents)
                        remainingBytes = List.drop totalLength bytes
                        remainingFifo = List.foldl Fifo.insert Fifo.empty remainingBytes
                    in
                    if checksum == calculatedChecksum then
                        ( Ok (Envelope boot length contents checksum), remainingFifo )
                    else
                        ( Err ChecksumMismatch, remainingFifo ) -- Return next fifo so we're not stuck
                else
                    ( Err IncompletePacket, alignedFifo ) -- Not enough bytes for a full packet, return original fifo
            _ ->
                ( Err InsufficientData, alignedFifo ) -- Not enough bytes for even boot and length, return original fifo

envelopeToPacket : Envelope -> Result Error Packet
envelopeToPacket envelope =
    case envelope.boot of 
        0x40 -> Err UnsupportedPacketType
        0xF0 -> Command.listIntToCommandWithArgs envelope.contents |> Maybe.map Response
                |> Maybe.map Ok
                |> Maybe.withDefault (Err UnparseableResponse)
        0xF4 -> Command.listIntToCommandWithArgs envelope.contents |> Maybe.map Error
                |> Maybe.map Ok
                |> Maybe.withDefault (Err UnparseableResponse)
        0xF1 -> Command.listIntToCommandWithArgs envelope.contents |> Maybe.map Status
                |> Maybe.map Ok
                |> Maybe.withDefault (Err UnparseableResponse)
        _ -> Err InvalidBootCode


createEnvelope : Int -> List Int -> Envelope
createEnvelope bootCode contents =
    let 
        length = List.length contents + 1 -- +1 for the checksum
        checksum = calculateChecksum ([bootCode, length] ++ contents)
    in
    Envelope bootCode length contents checksum

calculateChecksum : List Int -> Int
calculateChecksum bytes =
    (List.sum bytes) |> Bitwise.complement |> (+) 1 |> modBy 0x100 


alignBootCode : Fifo Int -> Fifo Int
alignBootCode fifo =
    let
        (maybeByte, newFifo) = Fifo.remove fifo
    in
    
    case maybeByte of
        Just byte ->
            if List.member byte [0x40, 0xF0, 0xF4, 0xF1] then
                fifo -- Found valid boot code, return aligned FIFO
            else
                alignBootCode newFifo -- Discard and continue searching
        Nothing ->
                        fifo -- FIFO is empty, return as is\n\n\n-- CLEAN COMMAND API
