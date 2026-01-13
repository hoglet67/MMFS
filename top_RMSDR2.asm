INCLUDE "DEVICE.asm"

_SWRAM_=TRUE            ; Sideways RAM Version
_ROMS_=FALSE            ; Exclude *ROMS command

INCLUDE "DEFAULTS.asm"

; This matches the name used in the latest official release (1.54)
MACRO BASE_NAME
    EQUS "MasterSD R2 "
    SYSTEM_NAME
    EQUS " SPI"
ENDMACRO

INCLUDE "mmfs100.asm"
