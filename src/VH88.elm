module VH88 exposing 
    ( -- Core Types
      Command(..)           -- expose constructors for pattern matching in commands  
    , PowerLevel            -- opaque type
    , Packet(..)            -- expose constructors for pattern matching responses
    , Error(..)             -- expose constructors for error handling  
    , ErrorCode(..)         -- expose constructors for error codes
    
    -- Smart Constructors
    , powerLevel            -- Int -> Result String PowerLevel
    , minPower, maxPower    -- PowerLevel constants
    
    -- Core Operations  
    , commandToBytes        -- Command -> Result String (List Int)
    , fifoBytesToPacket     -- Fifo Int -> (Result Error Packet, Fifo Int)
    
    -- Utilities
    , errorCodeFromInt      -- Int -> ErrorCode
    , errorCodeToString     -- ErrorCode -> String
    
    -- Command byte constants (needed by Main.elm for pending command tracking)
    , cmdSetRFIDPower       -- Int
    )

import Fifo exposing (Fifo)
import Bitwise


-- POWER LEVEL TYPE

{-| Opaque type for RFID power level with validation -}
type PowerLevel = PowerLevel Int

{-| Smart constructor for power level with validation -}
powerLevel : Int -> Result String PowerLevel
powerLevel level =
    if level >= 0 && level <= 33 then
        Ok (PowerLevel level)
    else
        Err ("Power level must be between 0 and 33, got " ++ String.fromInt level)

{-| Minimum power level (0) -}
minPower : PowerLevel
minPower = PowerLevel 0

{-| Maximum power level (33) -}
maxPower : PowerLevel
maxPower = PowerLevel 33

{-| Extract the integer value from PowerLevel (for internal use) -}
powerLevelToInt : PowerLevel -> Int
powerLevelToInt (PowerLevel level) = level


-- COMMAND TYPES

{-| VH-88 Commands with type safety and parameter validation -}
type Command
    = SetDuplicateFilter Bool
    | GetDuplicateFilter  
    | SetRfidPower PowerLevel
    | ReadWorkingParameters
    | SetTagFilter { address : Int, length : Int, mask : List Int }
    | GetTagFilter
    | SetWorkingParameters (List Int)  -- TODO: Make this more specific
    | SetBluetoothKeyboardDelay Int
    | SetFactoryParameters (List Int)
    | RestoreFactorySettings
    | SetTime { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int }
    | GetTime
    | SetRfidInventoryInterval Int
    | GetRfidInventoryInterval  
    | SetTidUserInventoryParameters { area : Int, key : Int, address : Int, count : Int }
    | GetTidUserInventoryParameters Int
    | GetRfidModuleVersion
    | SwitchToScanCodeModule
    | SwitchToRfidModule
    | RestoreBluetoothFactorySettings
    | SaveBluetoothParameters
    | SetReaderId (List Int)  -- 10 bytes
    | GetReaderId
    | SetBluetoothName String  -- max 8 bytes
    | GetBluetoothName
    | HostComputerCardReading Bool
    | GetUDiskInventoryFileDirectory
    | DownloadUDiskInventoryData String
    | DeleteUDiskInventoryFile String
    | SaveCurrentInventoryDataToUDisk

{-| Error codes from VH-88 protocol section 1.1.4 -}
type ErrorCode
    = Success
    | AntennaConnectionFailed
    | NoTagRecognized
    | LockingTagParameterError
    | WrongLengthListingTags
    | CommandLengthError
    | MinFrequencyGreaterThanMax
    | FrequencyBandParameterError
    | PowerSettingFailed
    | WorkModuleSettingError
    | OutputModeSettingError
    | AutoFilterSettingError
    | TagFilterLengthOutOfRange
    | CommandParameterError
    | CurrentModuleNotRFID
    | HardwareFailedToExecute
    | PathOpeningFailed
    | OtherError
    | FileReadFailed
    | FilePointerMovementFailed
    | FileDeletionFailed
    | FailedToMatchTag
    | FailedToReadTag
    | FailedToWriteTag
    | FrequencySettingFailed
    | FileNameTooLong
    | DeviceNotSupported
    | CommandTimeout

errorCodeToString : ErrorCode -> String
errorCodeToString code =
    case code of
        Success -> "Success"
        AntennaConnectionFailed -> "Antenna Connection Failed"
        NoTagRecognized -> "No Tag Recognized"
        LockingTagParameterError -> "Locking Tag Parameter Error"
        WrongLengthListingTags -> "Wrong Length Listing Tags"
        CommandLengthError -> "Command Length Error"
        MinFrequencyGreaterThanMax -> "Min Frequency Greater Than Max"
        FrequencyBandParameterError -> "Frequency Band Parameter Error"
        PowerSettingFailed -> "Power Setting Failed"
        WorkModuleSettingError -> "Work Module Setting Error"
        OutputModeSettingError -> "Output Mode Setting Error"
        AutoFilterSettingError -> "Auto Filter Setting Error"
        TagFilterLengthOutOfRange -> "Tag Filter Length Out Of Range"
        CommandParameterError -> "Command Parameter Error"
        CurrentModuleNotRFID -> "Current Module Not RFID"
        HardwareFailedToExecute -> "Hardware Failed To Execute"
        PathOpeningFailed -> "Path Opening Failed"
        OtherError -> "Other Error"
        FileReadFailed -> "File Read Failed"
        FilePointerMovementFailed -> "File Pointer Movement Failed"
        FileDeletionFailed -> "File Deletion Failed"
        FailedToMatchTag -> "Failed To Match Tag"
        FailedToReadTag -> "Failed To Read Tag"
        FailedToWriteTag -> "Failed To Write Tag"
        FrequencySettingFailed -> "Frequency Setting Failed"
        FileNameTooLong -> "File Name Too Long"
        DeviceNotSupported -> "Device Not Supported"
        CommandTimeout -> "Command Timeout"

-- TYPES

type Error
    = InsufficientData
    | ChecksumMismatch  
    | IncompletePacket
    | UnsupportedPacketType
    | InvalidBootCode
    | ProtocolError ErrorCode

-- for receiving packets
type Packet
    = Response CommandData
    | Error CommandData
    | Status CommandData


type alias CommandData = {command: Int, params: List Int}

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
        0xF0 -> Ok (Response (makeCommandData envelope.contents))
        0xF4 -> Ok (Error (makeCommandData envelope.contents))
        0xF1 -> Ok (Status (makeCommandData envelope.contents))
        _ -> Err InvalidBootCode

makeCommandData : List Int -> CommandData
makeCommandData contents =
    { command = List.head contents |> Maybe.withDefault 0
    , params = List.drop 1 contents
    }



commandPacketToBytes: Int -> List Int -> List Int
commandPacketToBytes cmdByte cmdParams =
    let
        cmdBootCode = 0x40
        envelopeContents = cmdByte :: cmdParams
        envelope = createEnvelope cmdBootCode envelopeContents
    in
    envelopeToBytes envelope


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


fifoBytesToPacket : (Fifo Int) -> (Result Error Packet, Fifo Int)
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
                        fifo -- FIFO is empty, return as is\n\n\n-- CLEAN COMMAND API

{-| Convert command to bytes for port communication -}
commandToBytes : Command -> CommandResult
commandToBytes command =
    case command of
        SetRfidPower powerLevelValue ->
            Ok (commandPacketToBytes cmdSetRFIDPower [powerLevelToInt powerLevelValue])
        
        SetDuplicateFilter enabled ->
            let filterValue = if enabled then 1 else 0
            in Ok (commandPacketToBytes 0x02 [filterValue])
        
        GetDuplicateFilter ->
            Ok (commandPacketToBytes 0x03 [])
            
        ReadWorkingParameters ->
            Ok (commandPacketToBytes 0x06 [])
        
        SetWorkingParameters params ->
            Ok (commandPacketToBytes 0x09 params)
        
        GetTime ->
            Ok (commandPacketToBytes 0x12 [])
            
        SetTime time ->
            Ok (commandPacketToBytes 0x11 [time.year, time.month, time.day, time.hour, time.minute, time.second])
        
        _ ->
            Err "Command not yet implemented"

{-| Helper function to convert error codes from protocol to meaningful types -}
type alias CommandResult = Result String (List Int)
errorCodeFromInt : Int -> ErrorCode
errorCodeFromInt code =
    case code of
        0x00 -> Success
        0x01 -> AntennaConnectionFailed
        0x02 -> NoTagRecognized
        0x06 -> LockingTagParameterError
        0x07 -> WrongLengthListingTags
        0x08 -> CommandLengthError
        0x09 -> MinFrequencyGreaterThanMax
        0x10 -> FrequencyBandParameterError
        0x11 -> PowerSettingFailed
        0x12 -> WorkModuleSettingError
        0x13 -> OutputModeSettingError
        0x14 -> AutoFilterSettingError
        0x15 -> TagFilterLengthOutOfRange
        0x16 -> CommandParameterError
        0x17 -> CurrentModuleNotRFID
        0x18 -> HardwareFailedToExecute
        0x19 -> PathOpeningFailed
        0x20 -> OtherError
        0x21 -> FileReadFailed
        0x22 -> FilePointerMovementFailed
        0x23 -> FileDeletionFailed
        0x24 -> FailedToMatchTag
        0x25 -> FailedToReadTag
        0x26 -> FailedToWriteTag
        0x27 -> FrequencySettingFailed
        0x28 -> FileNameTooLong
        0x29 -> DeviceNotSupported
        0xFF -> CommandTimeout
        _ -> OtherError

-- COMMAND BYTES
cmdSetRFIDPower : Int
cmdSetRFIDPower = 0x04