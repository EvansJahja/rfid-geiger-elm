module SerDe.Commands exposing (..)
import SerDe.Constants exposing (..)
import SerDe exposing (commandPacketToBytes)


setRFIDPower : Int -> List Int
setRFIDPower powerLevel =
    commandPacketToBytes SerDe.Constants.setRfidPowerCmd [ powerLevel ]