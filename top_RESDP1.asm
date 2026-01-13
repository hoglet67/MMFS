INCLUDE "DEVICE.asm"

_ELECTRON_=TRUE         ; Electron version
_SWRAM_=TRUE            ; Sideways RAM Version
_ROMS_=FALSE            ; Exclude *ROMS command
_TUBEHOST_=FALSE        ; Exclude Tube Host
_TUBE_BASE=&FCE0        ; Base Address of Tube

INCLUDE "DEFAULTS.asm"

MACRO BASE_NAME
    EQUS "ElkSD128 "
    SYSTEM_NAME
    EQUS " SPI"
ENDMACRO

INCLUDE "mmfs100.asm"
