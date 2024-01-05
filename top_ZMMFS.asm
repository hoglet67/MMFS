\ Device: U=User Port, T=User Port Turbo, M=Memory Mapped, E=Elk Printer Port
INCLUDE "DEVICE.asm"

_ELECTRON_=FALSE
_TUBE_BASE=&FFE0

INCLUDE "bootstrap.asm"

.romst
    incbin  "build/"+_DEVICE_+"/SWMMFS.rom"
.end

SAVE &8000, &C000
