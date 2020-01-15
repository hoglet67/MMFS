_MASTER_=FALSE          ; Master version
_ELECTRON_=TRUE         ; Electron version
_SWRAM_=TRUE            ; Sideways RAM Version
_BP12K_=FALSE           ; B+ private RAM version
_ROMS_=FALSE            ; Include *ROMS command (i.e. No DFS or 8271 DFS)
_COMMANDS_=TRUE         ; Include normal DFS commands (*COPY etc)
_UTILS_=FALSE           ; Include utilites (*DUMP etc.) (i.e. No DFS)
_TUBEHOST_=FALSE        ; Include Tube Host (i.e. no DFS or DFS 0.90)
_VIA_BASE=&FCB0         ; Base Address of 6522 VIA
_TUBE_BASE=&FCE0        ; Base Address of Tube
_LARGEFILES=FALSE       ; true = enable long (>64K) file support
_DEBUG=FALSE            ; true = enable debugging of service calls, etc
_DEBUG_MMC=TRUE         ; true = enable debugging of MMC initialization
_DFS_EMUL=TRUE          ; true = use DFS filesystem number + handles
_USE_MGC_SHIFTREG=TRUE  ; false = Bit Bang, true = Shift Reg

MACRO BASE_NAME
	EQUS "MGC MMFS SWRAM (ShiftReg, Debug)"
ENDMACRO

INCLUDE "mmfs100.asm"
