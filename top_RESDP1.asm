INCLUDE "DEVICE.asm"

_ELECTRON_=TRUE         ; Electron version
_SWRAM_=TRUE            ; Sideways RAM Version
_ROMS_=FALSE            ; Exclude *ROMS command
_TUBEHOST_=FALSE        ; Exclude Tube Host
_TUBE_BASE=&FCE0        ; Base Address of Tube

INCLUDE "DEFAULTS.asm"

; This matches the name used in the latest official release (1.44)
MACRO BASE_NAME
    EQUS "Electron "
    SYSTEM_NAME
    EQUS " SWRAM SPI 16K"
ENDMACRO

INCLUDE "mmfs100.asm"
