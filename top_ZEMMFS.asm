INCLUDE "DEVICE.asm"

_ELECTRON_=TRUE
_TUBE_BASE=&FCE0

INCLUDE "bootstrap.asm"

.romst
IF _DEVICE_="R"
    incbin  "build/R/REMMFS.rom"
ELSE
    incbin  "build/"+_DEVICE_+"/ESWMMFS.rom"
ENDIF
.end

\ Code will always end at C400 because we just include the entire ROM and chop the end off.
\ As long as ROMs end before &C000-&400 = &BC00 they'll work fine here.
SAVE &8000, &C000
