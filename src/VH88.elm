module VH88 exposing (VH88Error(..), VH88Packet(..), fifoBytesToPacket, commandPacketToBytes)

import Fifo exposing (Fifo)
import Bitwise



-- TYPES

type VH88Error
    = ErrNotEnough
    | ErrDummyA
    | ErrDummyB
    | Unsupported
    | IllegalState

type VH88Packet
    = Command VH88CommandData
    | Response VH88CommandData
    | Error VH88CommandData
    | Status VH88CommandData


type alias VH88CommandData = {command: Int, params: List Int}

type alias CommandByte = Int

type alias CommandParamBytes = List Int


type alias VH88Envelope = { boot: Int, length: Int, contents: List Int, checksum: Int }


-- ENVELOPES
envelopeToBytes : VH88Envelope -> List Int
envelopeToBytes envelope =
    envelope.boot :: envelope.length :: envelope.contents ++ [ envelope.checksum ]

-- -- Notes on fifo
-- -- We use a FIFO to store incoming bytes from the serial port
-- -- and we return the remaining bytes in the FIFO after extracting a packet
-- -- or original FIFO if no complete packet is found
bytesToEnvelope : (Fifo Int) -> (Result VH88Error VH88Envelope, Fifo Int)
bytesToEnvelope potentiallyNotBootCodeAlignedFifo = 
    let
        alignedFifo = alignBootCode potentiallyNotBootCodeAlignedFifo

        -- here, we know that bytes start with a valid boot code
        bytes = Fifo.toList alignedFifo
    in
        case bytes of
            boot :: length :: rest ->
                let
                    totalLength = length + 2 -- +2 for boot and length bytes
                    maybePacketBytes = List.take totalLength bytes
                in
                if List.length maybePacketBytes == totalLength then
                    let
                        contents = List.drop 2 maybePacketBytes |> List.take (length - 1)
                        checksum = List.drop (totalLength - 1) maybePacketBytes |> List.head |> Maybe.withDefault 0
                        calculatedChecksum = calculateChecksum (boot :: length :: contents)
                        remainingBytes = List.drop totalLength bytes
                        remainingFifo = List.foldl Fifo.insert Fifo.empty remainingBytes
                    in
                    if checksum == calculatedChecksum then
                        ( Ok (VH88Envelope boot length contents checksum), remainingFifo )
                    else
                        ( Err ErrDummyA, alignedFifo ) -- Return original fifo on error
                else
                    ( Err ErrDummyB, alignedFifo ) -- Not enough bytes for a full packet, return original fifo
            _ ->
                ( Err ErrDummyB, alignedFifo ) -- Not enough bytes for even boot and length, return original fifo

envelopeToPacket : VH88Envelope -> Result VH88Error VH88Packet
envelopeToPacket envelope =
    case envelope.boot of 
        0x40 -> Err Unsupported
        0xF0 -> Ok (Response (makeCommandData envelope.contents))
        0xF4 -> Ok (Error (makeCommandData envelope.contents))
        0xF1 -> Ok (Status (makeCommandData envelope.contents))
        _ -> Err IllegalState

makeCommandData : List Int -> VH88CommandData
makeCommandData contents =
    { command = List.head contents |> Maybe.withDefault 0
    , params = List.drop 1 contents
    }

-- PACKETS


packetToSerial : VH88Packet -> List Int
packetToSerial packet =
    case packet of
        Command cmd -> commandToSerial cmd
        _ -> Debug.todo "Implement packetToSerial for Response and Error"


commandToSerial : VH88CommandData -> List Int
commandToSerial cmd =
    commandPacketToBytes cmd.command cmd.params




commandPacketToBytes: CommandByte -> CommandParamBytes -> List Int
commandPacketToBytes cmdByte cmdParams =
    let
        cmdBootCode = 0x40
        envelopeContents = cmdByte :: cmdParams
        envelope = createEnvelope cmdBootCode envelopeContents
    in
    envelopeToBytes envelope


createEnvelope : Int -> List Int -> VH88Envelope
createEnvelope bootCode contents =
    let 
        length = List.length contents + 1 -- +1 for the checksum
        checksum = calculateChecksum ([bootCode, length] ++ contents)
    in
    VH88Envelope bootCode length contents checksum

calculateChecksum : List Int -> Int
calculateChecksum bytes =
    (List.sum bytes) |> Bitwise.complement |> (+) 1 |> modBy 0x100 


fifoBytesToPacket : (Fifo Int) -> (Result VH88Error VH88Packet, Fifo Int)
fifoBytesToPacket fifo =
    let
        (maybeEnvelope, remainingFifo) = bytesToEnvelope fifo
    in
    case maybeEnvelope of
        Ok envelope ->
            ( envelopeToPacket envelope, remainingFifo )
        Err err ->
            ( Err err, remainingFifo )

{-| Convert a list of bytes from serial into a string
Returns an error if the bytes are invalid
-}


-- HELPER FUNCTIONS

-- Align the FIFO buffer to the next valid boot code
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
            fifo -- FIFO is empty, return as is


isValidByte : Int -> Bool
isValidByte byte =
    byte >= 0 && byte <= 255