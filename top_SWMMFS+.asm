_MASTER_=FALSE          ; Master version
_ELECTRON_=FALSE        ; Electron version
_SWRAM_=TRUE            ; Sideways RAM Version (NB: this and _BP12K_ both TRUE)
_BP12K_=TRUE            ; B+ private RAM version
_ROMS_=TRUE             ; Include *ROMS command (i.e. No DFS or 8271 DFS)
_COMMANDS_=TRUE         ; Include normal DFS commands (*COPY etc)
_UTILS_=TRUE            ; Include utilites (*DUMP etc.) (i.e. No DFS)
_TUBEHOST_=TRUE         ; Include Tube Host (i.e. no DFS or DFS 0.90)
_VIA_BASE=&FE60         ; Base Address of 6522 VIA
_TUBE_BASE=&FEE0        ; Base Address of Tube
_LARGEFILES=TRUE        ; true = enable long (>64K) file support
_DEBUG=FALSE            ; true = enable debugging of service calls, etc
_DEBUG_MMC=FALSE        ; true = enable debugging of MMC initialization
_DFS_EMUL=TRUE          ; true = use DFS filesystem number + handles

MACRO BASE_NAME
	EQUS "Model B+ MMFS 12K"
ENDMACRO

INCLUDE "mmfs100.asm"
