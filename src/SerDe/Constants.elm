module SerDe.Constants exposing (..)

-- VH-88 Command Codes from Protocol v1.5 - Section 1.2 Serial Control Commands

-- Basic RFID Settings Commands
setDuplicateFilterCmd : Int
setDuplicateFilterCmd = 0x02  -- Set whether reader should filter duplicate tags

getDuplicateFilterCmd : Int
getDuplicateFilterCmd = 0x03  -- Get whether reader filters duplicate tags

setRfidPowerCmd : Int
setRfidPowerCmd = 0x04  -- Set RFID power of the reader (range 0-33)

-- Parameter Commands
readWorkingParametersCmd : Int
readWorkingParametersCmd = 0x06  -- Read working parameters of the reader

setTagFilterCmd : Int
setTagFilterCmd = 0x07  -- Set label filter

getTagFilterCmd : Int
getTagFilterCmd = 0x08  -- Get tag filter

setWorkingParametersCmd : Int  
setWorkingParametersCmd = 0x09  -- Set working parameters of the reader

-- Bluetooth Commands
setBluetoothKeyboardDelayCmd : Int
setBluetoothKeyboardDelayCmd = 0x0A  -- Set output delay of Bluetooth emulation keyboard

restoreBluetoothFactorySettingsCmd : Int
restoreBluetoothFactorySettingsCmd = 0x20  -- Restore Bluetooth factory settings

saveBluetoothParametersCmd : Int
saveBluetoothParametersCmd = 0x21  -- Save Bluetooth parameters as factory parameters

setBluetoothNameCmd : Int
setBluetoothNameCmd = 0x8D  -- Set Bluetooth name (max 8 bytes)

getBluetoothNameCmd : Int
getBluetoothNameCmd = 0x8E  -- Get Bluetooth name

-- Factory and System Commands
setFactoryParametersCmd : Int
setFactoryParametersCmd = 0x0C  -- Set factory parameters [internal use]

restoreFactorySettingsCmd : Int
restoreFactorySettingsCmd = 0x0D  -- Restore reader factory settings

-- Time Commands
setTimeCmd : Int
setTimeCmd = 0x11  -- Set time of the reader

getTimeCmd : Int
getTimeCmd = 0x12  -- Get time of reader

-- Inventory Commands
setRfidInventoryIntervalCmd : Int
setRfidInventoryIntervalCmd = 0x13  -- Set RFID inventory interval

getRfidInventoryIntervalCmd : Int
getRfidInventoryIntervalCmd = 0x14  -- Get RFID inventory interval

setTidUserInventoryParametersCmd : Int
setTidUserInventoryParametersCmd = 0x15  -- Set inventory parameters in TID/USER area

getTidUserInventoryParametersCmd : Int
getTidUserInventoryParametersCmd = 0x16  -- Get inventory parameters in TID/USER area

-- Module Commands
getRfidModuleVersionCmd : Int
getRfidModuleVersionCmd = 0x18  -- Get version number of RFID module

switchToScanCodeModuleCmd : Int
switchToScanCodeModuleCmd = 0x1D  -- Switch to scan code module

switchToRfidModuleCmd : Int
switchToRfidModuleCmd = 0x2D  -- Switch to RFID module

-- Reader ID Commands
setReaderIdCmd : Int
setReaderIdCmd = 0x8B  -- Set ID of the reader

getReaderIdCmd : Int
getReaderIdCmd = 0x8C  -- Get ID of the reader

-- Host Computer Commands
hostComputerCardReadingCmd : Int
hostComputerCardReadingCmd = 0xE1  -- Host computer card reading (also used for scan code)

-- U Disk Commands
getUDiskInventoryFileDirectoryCmd : Int
getUDiskInventoryFileDirectoryCmd = 0xE2  -- Get U Disk inventory file directory

downloadUDiskInventoryDataCmd : Int
downloadUDiskInventoryDataCmd = 0xE3  -- Download U Disk inventory data

deleteUDiskInventoryFileCmd : Int
deleteUDiskInventoryFileCmd = 0xE4  -- Delete U Disk inventory file

saveCurrentInventoryDataToUDiskCmd : Int
saveCurrentInventoryDataToUDiskCmd = 0xE5  -- Save current inventory data to U disk

-- Error Codes from Protocol v1.1.4

errSuccess : Int
errSuccess = 0x00  -- Command executed successfully

errAntennaConnection : Int
errAntennaConnection = 0x01  -- Antenna connection failed

errNoTagRecognized : Int
errNoTagRecognized = 0x02  -- No tag recognized

errLockingTagParameter : Int
errLockingTagParameter = 0x06  -- Parameter error when locking tag

errWrongLengthListingTags : Int
errWrongLengthListingTags = 0x07  -- Wrong length when listing tags

errCommandLength : Int
errCommandLength = 0x08  -- Command length error

errMinFrequencyGreaterThanMax : Int
errMinFrequencyGreaterThanMax = 0x09  -- The minimum frequency cannot be greater than the maximum frequency

errFrequencyBandParameter : Int
errFrequencyBandParameter = 0x10  -- Frequency band parameter error

errPowerSetting : Int
errPowerSetting = 0x11  -- Power setting failed

errWorkModuleSetting : Int
errWorkModuleSetting = 0x12  -- Work module setting error

errOutputModeSetting : Int
errOutputModeSetting = 0x13  -- Wrong output mode setting

errAutoFilterSetting : Int
errAutoFilterSetting = 0x14  -- Auto filter setting error

errTagFilterLengthOutOfRange : Int
errTagFilterLengthOutOfRange = 0x15  -- Tag filter length is out of range

errCommandParameter : Int
errCommandParameter = 0x16  -- Command parameter error

errCurrentModuleNotRFID : Int
errCurrentModuleNotRFID = 0x17  -- The current module is not an RFID module, this command cannot be used

errHardwareFailedToExecute : Int
errHardwareFailedToExecute = 0x18  -- The hardware failed to execute this command

errPathOpeningFailed : Int
errPathOpeningFailed = 0x19  -- Path opening failed

errOther : Int
errOther = 0x20  -- Other errors

errFileReadFailed : Int
errFileReadFailed = 0x21  -- File read failed

errFilePointerMovementFailed : Int
errFilePointerMovementFailed = 0x22  -- File pointer movement failed

errFileDeletionFailed : Int
errFileDeletionFailed = 0x23  -- File deletion failed

errFailedToMatchTag : Int
errFailedToMatchTag = 0x24  -- Failed to match tag

errFailedToReadTag : Int
errFailedToReadTag = 0x25  -- Failed to read tag

errFailedToWriteTag : Int
errFailedToWriteTag = 0x26  -- Failed to write tag

errFrequencySetting : Int
errFrequencySetting = 0x27  -- Frequency setting failed

errFileNameTooLong : Int
errFileNameTooLong = 0x28  -- File name is too long

errDeviceNotSupported : Int
errDeviceNotSupported = 0x29  -- The device does not support this command

errCommandTimeout : Int
errCommandTimeout = 0xFF  -- Command execution timeout


