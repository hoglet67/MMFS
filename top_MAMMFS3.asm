_MASTER_=TRUE           ; Master version
_ELECTRON_=FALSE        ; Electron version
_SWRAM_=FALSE           ; Sideways RAM Version
_BP12K=FALSE            ; B+ private RAM version
_ROMS_=FALSE            ; Include *ROMS command (i.e. No DFS or 8271 DFS)
_UTILS_=FALSE           ; Include utilites (*DUMP etc.) (i.e. No DFS)
_TUBEHOST_=FALSE        ; Include Tube Host (i.e. no DFS or DFS 0.90)
_VIA_BASE=&FEA0         ; Base Address of 6522 VIA
_TUBE_BASE=&FEE0        ; Base Address of Tube
_TURBOMMC=FALSE         ; true = build for TurboMMC, and enable PB2-4 as outputs
_LARGEFILES=TRUE        ; true = enable long (>64K) file support
_DEBUG=FALSE            ; true = enable debugging of service calls, etc

MACRO BASE_NAME
	EQUS "Master MMFS"
ENDMACRO

INCLUDE "mmfs100.asm"
