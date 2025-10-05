module VH88.Command exposing
    ( Command(..)
    , CommandWithArgs(..)
    , commandByteToCommand
    , commandToInt
    , listIntToCommandWithArgs
    , toString
    )


type Command
    = SetDuplicateFilter
    | GetDuplicateFilter
    | SetRfidPower
    | ReadWorkingParameters
    | SetTagFilter
    | GetTagFilter
    | SetWorkingParameters
    | SetBluetoothKeyboardDelay
    | SetFactoryParameters
    | RestoreFactorySettings
    | SetTime
    | GetTime
    | SetRfidInventoryInterval
    | GetRfidInventoryInterval
    | SetTidUserInventoryParameters
    | GetTidUserInventoryParameters
    | GetRfidModuleVersion
    | SwitchToScanCodeModule
    | SwitchToRfidModule
    | RestoreBluetoothFactorySettings
    | SaveBluetoothParameters
    | SetReaderId
    | GetReaderId
    | SetBluetoothName
    | GetBluetoothName
    | HostComputerCardReading
    | GetUDiskInventoryFileDirectory
    | DownloadUDiskInventoryData
    | DeleteUDiskInventoryFile
    | SaveCurrentInventoryDataToUDisk
    | StartListingTags

toString : Command -> String
toString command =
    case command of
        SetDuplicateFilter ->
            "SetDuplicateFilter"

        GetDuplicateFilter ->
            "GetDuplicateFilter"

        SetRfidPower ->
            "SetRfidPower"

        ReadWorkingParameters ->
            "ReadWorkingParameters"

        SetTagFilter ->
            "SetTagFilter"

        GetTagFilter ->
            "GetTagFilter"

        SetWorkingParameters ->
            "SetWorkingParameters"

        SetBluetoothKeyboardDelay ->
            "SetBluetoothKeyboardDelay"

        SetFactoryParameters ->
            "SetFactoryParameters"

        RestoreFactorySettings ->
            "RestoreFactorySettings"

        SetTime ->
            "SetTime"

        GetTime ->
            "GetTime"

        SetRfidInventoryInterval ->
            "SetRfidInventoryInterval"

        GetRfidInventoryInterval ->
            "GetRfidInventoryInterval"

        SetTidUserInventoryParameters ->
            "SetTidUserInventoryParameters"

        GetTidUserInventoryParameters ->
            "GetTidUserInventoryParameters"

        GetRfidModuleVersion ->
            "GetRfidModuleVersion"

        SwitchToScanCodeModule ->
            "SwitchToScanCodeModule"

        SwitchToRfidModule ->
            "SwitchToRfidModule"

        RestoreBluetoothFactorySettings ->
            "RestoreBluetoothFactorySettings"

        SaveBluetoothParameters ->
            "SaveBluetoothParameters"

        SetReaderId ->
            "SetReaderId"

        GetReaderId ->
            "GetReaderId"

        SetBluetoothName ->
            "SetBluetoothName"

        GetBluetoothName ->
            "GetBluetoothName"

        HostComputerCardReading ->
            "HostComputerCardReading"

        GetUDiskInventoryFileDirectory ->
            "GetUDiskInventoryFileDirectory"

        DownloadUDiskInventoryData ->
            "DownloadUDiskInventoryData"

        DeleteUDiskInventoryFile ->
            "DeleteUDiskInventoryFile"

        SaveCurrentInventoryDataToUDisk ->
            "SaveCurrentInventoryDataToUDisk"

        StartListingTags ->
            "StartListingTags"


-- we need to convert to bytes


type CommandByte
    = CommandByte Int


commandMappings : List ( Command, CommandByte )
commandMappings =
    [ ( SetDuplicateFilter, CommandByte 0x02 )
    , ( GetDuplicateFilter, CommandByte 0x03 )
    , ( SetRfidPower, CommandByte 0x04 )
    , ( ReadWorkingParameters, CommandByte 0x06 )
    , ( SetTagFilter, CommandByte 0x07 )
    , ( GetTagFilter, CommandByte 0x08 )
    , ( SetWorkingParameters, CommandByte 0x09 )
    , ( SetBluetoothKeyboardDelay, CommandByte 0x0A )
    , ( SetFactoryParameters, CommandByte 0x0B )
    , ( RestoreFactorySettings, CommandByte 0x0C )
    , ( SetTime, CommandByte 0x11 )
    , ( GetTime, CommandByte 0x12 )
    , ( SetRfidInventoryInterval, CommandByte 0x13 )
    , ( GetRfidInventoryInterval, CommandByte 0x14 )
    , ( SetTidUserInventoryParameters, CommandByte 0x15 )
    , ( GetTidUserInventoryParameters, CommandByte 0x16 )
    , ( GetRfidModuleVersion, CommandByte 0x17 )
    , ( SwitchToScanCodeModule, CommandByte 0x18 )
    , ( SwitchToRfidModule, CommandByte 0x19 )
    , ( RestoreBluetoothFactorySettings, CommandByte 0x1A )
    , ( SaveBluetoothParameters, CommandByte 0x1B )
    , ( SetReaderId, CommandByte 0x1C )
    , ( GetReaderId, CommandByte 0x1D )
    , ( SetBluetoothName, CommandByte 0x1E )
    , ( GetBluetoothName, CommandByte 0x1F )
    , ( HostComputerCardReading, CommandByte 0xE1 )
    , ( GetUDiskInventoryFileDirectory, CommandByte 0x21 )
    , ( DownloadUDiskInventoryData, CommandByte 0x22 )
    , ( DeleteUDiskInventoryFile, CommandByte 0x23 )
    , ( SaveCurrentInventoryDataToUDisk, CommandByte 0x24 )
    , ( StartListingTags, CommandByte 0xEE )
    ]


intToCommandByte : Int -> CommandByte
intToCommandByte byte =
    CommandByte byte


commandToCommandByte : Command -> CommandByte
commandToCommandByte command =
    commandMappings
        |> List.filter (\( cmd, _ ) -> cmd == command)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault (CommandByte 0x00)



-- default to 0x00 if not found


commandToInt : Command -> Int
commandToInt command =
    let
        (CommandByte byte) =
            commandToCommandByte command
    in
    byte


commandByteToCommand : CommandByte -> Maybe Command
commandByteToCommand (CommandByte byte) =
    commandMappings
        |> List.filter (\( _, CommandByte b ) -> b == byte)
        |> List.head
        |> Maybe.map Tuple.first


type CommandWithArgs
    = CommandWithArgs ( Command, List Int )


listIntToCommandWithArgs : List Int -> Maybe CommandWithArgs
listIntToCommandWithArgs contents =
    case contents of
        cmd :: params ->
            commandByteToCommand (intToCommandByte cmd)
                |> Maybe.map
                    (\command ->
                        CommandWithArgs ( command, params )
                    )

        _ ->
            Nothing
