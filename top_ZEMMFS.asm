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
ELSE
    ERROR "Illegal device"
ENDIF
.end        
        
SAVE "", &8000, &C000
