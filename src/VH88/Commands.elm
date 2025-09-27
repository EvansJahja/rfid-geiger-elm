module VH88.Commands exposing (..)
import VH88.Constants exposing (..)
import VH88 exposing (commandPacketToBytes)


setRFIDPower : Int -> List Int
setRFIDPower powerLevel =
    commandPacketToBytes VH88.Constants.setRfidPowerCmd [ powerLevel ]