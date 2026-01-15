INCLUDE "DEVICE.asm"

_ELECTRON_=TRUE         ; Electron version
_SWRAM_=TRUE            ; Sideways RAM Version
_ROMS_=FALSE            ; Exclude *ROMS command
_TUBEHOST_=FALSE        ; Exclude Tube Host
_TUBE_BASE=&FCE0        ; Base Address of Tube

MAGIC_ADDRESS=&B5FE     ; Match the ROM/RAM threshold of existing hardware

INCLUDE "DEFAULTS.asm"

; This matches the name used in the latest official release (1.54)
MACRO BASE_NAME
    EQUS "ElkSD128 "
    SYSTEM_NAME
    EQUS " SPI"
ENDMACRO

INCLUDE "mmfs100.asm"
