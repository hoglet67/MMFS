INCLUDE "DEVICE.asm"

_ELECTRON_=FALSE
_TUBE_BASE=&FFE0

INCLUDE "bootstrap.asm"

.romst
IF _DEVICE_="R"
    incbin  "build/R/RMMMFS.rom"
ELSE
    incbin  "build/"+_DEVICE_+"/SWMMFS.rom"
ENDIF
.end

SAVE &8000, &C000
