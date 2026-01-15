INCLUDE "DEVICE.asm"

_ELECTRON_=TRUE         ; Electron version
_SWRAM_=TRUE            ; Sideways RAM Version
_ROMS_=FALSE            ; Exclude *ROMS command
_TUBEHOST_=FALSE        ; Exclude Tube Host
_TUBE_BASE=&FCE0        ; Base Address of Tube

INCLUDE "DEFAULTS.asm"

; This matches the name used in the 1.60 build Ramtop posted
MACRO BASE_NAME
    EQUS "Electron "
    SYSTEM_NAME
    EQUS " SWRAM 16K"
ENDMACRO

INCLUDE "mmfs100.asm"
