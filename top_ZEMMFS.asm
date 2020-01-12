\ Device: U=User Port, T=User Port Turbo, M=Memory Mapped, E=Elk Printer Port
INCLUDE "DEVICE.asm"

_ELECTRON_=TRUE
_TUBE_BASE=&FCE0

INCLUDE "bootstrap.asm"

.romst
IF _DEVICE_='U'
    incbin  "build/U/ESWMMFS.rom"
ELIF _DEVICE_='T'
    incbin  "build/T/ESWMMFS.rom"
ELIF _DEVICE_='E'
    incbin  "build/E/ESWMMFS.rom"
ELIF _DEVICE_='P'
    incbin  "build/P/ESWMMFS.rom"
ELIF _DEVICE_='M'
    incbin  "build/M/ESWMMFS.rom"
ELIF _DEVICE_='G'
    incbin  "build/G/ESWMMFS.rom"
ELSE
    ERROR "Illegal device"
ENDIF
.end

\ Code will always end at C400 because we just include the entire ROM and chop the end off.
\ As long as ROMs end before &C000-&400 = &BC00 they'll work fine here.
SAVE "", &8000, &C000
