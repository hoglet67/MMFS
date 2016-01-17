_MASTER_=FALSE          ; Master version
_SWRAM_=TRUE            ; Sideways RAM Version
_ROMS_=TRUE             ; Include *ROMS command (i.e. No DFS or 8271 DFS)
_UTILS_=TRUE            ; Include utilites (*DUMP etc.) (i.e. No DFS)
_TUBEHOST_=FALSE        ; Include Tube Host (i.e. no DFS or DFS 0.90)
_VIA_BASE=&FCB0         ; Base Address of 6522 VIA
_TUBE_BASE=&FCE0        ; Base Address of Tube

INCLUDE "mmfs100.asm"

SAVE "build/ESWMMFS", &8000, &C000
