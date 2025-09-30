module VH88 exposing (..)

import Fifo exposing (Fifo)
import VH88.Command as Command exposing (CommandWithArgs(..), Command(..))
import VH88.Packet as Packet exposing (Packet(..), packetToBytes, envelopeToPacket, bytesToEnvelope)
import VH88.Error exposing (Error)
import VH88.WorkingParameters as WorkingParameters exposing (WorkingParameters) 
import BytesHelper



{-| Error codes from VH-88 protocol section 1.1.4 -}
-- for receiving packets



fifoBytesToPacket : (Fifo Int) -> (Result Error Packet, Fifo Int)
fifoBytesToPacket fifo =
    let
        (maybeEnvelope, remainingFifo) = Packet.bytesToEnvelope fifo
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


setRFIDPower : Int -> Result String Packet
setRFIDPower powerLevel =
    if powerLevel < 0 || powerLevel > 33 then
        Err "Power level must be between 0 and 33"
    else
        Ok (Packet.Request (CommandWithArgs (SetRfidPower, [ powerLevel ])))

readWorkingParameters : Packet
readWorkingParameters =
    Packet.Request (CommandWithArgs (ReadWorkingParameters, []))

setWorkingParameters : WorkingParameters -> Packet 
setWorkingParameters params =
    let
        encodedParams = BytesHelper.encodeListInt (WorkingParameters.toEncoder params)
    in
        Packet.Request (CommandWithArgs (SetWorkingParameters, encodedParams))



startListingTags : Packet
startListingTags =
    Packet.Request (CommandWithArgs (StartListingTags, [1, 0])) -- EPC, len=0