\ Device: U=User Port, T=User Port Turbo, M=Memory Mapped, E=Elk Printer Port
INCLUDE "DEVICE.asm"

_ELECTRON_=FALSE
_SRAM_WP_=TRUE
        
INCLUDE "bootstrap.asm"

.romst
IF _DEVICE_='U'
    incbin  "build/U/SWMMFS.rom"
ELIF _DEVICE_='T'
    incbin  "build/T/SWMMFS.rom"
ELIF _DEVICE_='E'
    incbin  "build/E/SWMMFS.rom"
ELIF _DEVICE_='M'
    incbin  "build/M/SWMMFS.rom"
ELSE
    ERROR "Illegal device"
ENDIF
.end        
        
SAVE "", &8000, &C000
