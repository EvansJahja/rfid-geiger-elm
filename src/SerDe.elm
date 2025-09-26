module SerDe exposing (SerDeError(..), VH88Command(..), VH88Packet(..), bytesToEnvelope, packetToSerial, createEnvelope, calculateChecksum)

import Bitwise exposing (complement)


-- TYPES

type SerDeError
    = ErrDummyA
    | ErrDummyB

type VH88Packet
    = Command VH88Command
    | Response VH88Response
    | Error VH88Error

type VH88Command
    = CmdSetRFIDPower Int

type VH88Response
    = RespTODO

type VH88Error
    = ErrTODO

type alias CommandByte = Int

type alias CommandParamBytes = List Int


type alias VH88Envelope = { boot: Int, length: Int, contents: List Int, checksum: Int }


-- SERIALIZATION / DESERIALIZATION

envelopeToBytes : VH88Envelope -> List Int
envelopeToBytes envelope =
    envelope.boot :: envelope.length :: envelope.contents ++ [ envelope.checksum ]

bytesToEnvelope : List Int -> Result SerDeError VH88Envelope
bytesToEnvelope bytes = 
    case bytes of
        boot :: length :: rest ->
            let
                contents = List.take (length - 1) rest
                checksum = List.drop (length - 1) rest |> List.head |> Maybe.withDefault 0
                calculatedChecksum = calculateChecksum (boot :: length :: contents)
            in
            if checksum == calculatedChecksum then
                Ok (VH88Envelope boot length contents checksum)
            else
                Err ErrDummyA
        _ ->
            Err ErrDummyB


    
        
    -- in
    


packetToSerial : VH88Packet -> List Int
packetToSerial packet =
    case packet of
        Command cmd -> commandToSerial cmd
        _ -> Debug.todo "Implement packetToSerial for Response and Error"


commandToSerial : VH88Command -> List Int
commandToSerial cmd =
    case cmd of
        CmdSetRFIDPower powerLevel ->
            -- Example: Convert command to bytes (this is just illustrative)
            commandPacketToBytes 0x04 [powerLevel]



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

{-| Convert a list of bytes from serial into a string
Returns an error if the bytes are invalid
-}


-- HELPER FUNCTIONS

isValidByte : Int -> Bool
isValidByte byte =
    byte >= 0 && byte <= 255