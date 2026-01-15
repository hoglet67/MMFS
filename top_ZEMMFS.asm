INCLUDE "DEVICE.asm"

_ELECTRON_=TRUE
_TUBE_BASE=&FCE0

IF _DEVICE_="S"
MAGIC_ADDRESS=&B5FE     ; Match the ROM/RAM threshold of existing hardware
ENDIF

INCLUDE "bootstrap.asm"

.romst
IF _DEVICE_="S"
    incbin  "build/S/RESD128.rom"
ELSE
    incbin  "build/"+_DEVICE_+"/ESWMMFS.rom"
ENDIF
.end

\ Code will always end at C400 because we just include the entire ROM and chop the end off.
\ As long as ROMs end before &C000-&400 = &BC00 they'll work fine here.
SAVE &8000, &C000
