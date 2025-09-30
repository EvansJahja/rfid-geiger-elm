module VH88.Error exposing (..)

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


type Error
    = InsufficientData
    | ChecksumMismatch  
    | IncompletePacket
    | UnsupportedPacketType
    | InvalidBootCode
    | UnparseableResponse
    | ProtocolError ErrorCode

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