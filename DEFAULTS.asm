; The defaults match standard model B version

_MASTER_=?FALSE          ; Master version
_ELECTRON_=?FALSE        ; Electron version
_SWRAM_=?FALSE           ; Sideways RAM Version
_BP12K_=?FALSE           ; B+ private RAM version
_ROMS_=?TRUE             ; Include *ROMS command (i.e. No DFS or 8271 DFS)
_COMMANDS_=?TRUE         ; Include normal DFS commands (*COPY etc)
_UTILS_=?TRUE            ; Include utilites (*DUMP etc.) (i.e. No DFS)
_TUBEHOST_=?TRUE         ; Include Tube Host (i.e. no DFS or DFS 0.90)
_TUBE_BASE=?&FEE0        ; Base Address of Tube
_LARGEFILES=?TRUE        ; Enable long (>64K) file support
_DEBUG=?FALSE            ; Enable debugging of service calls, etc
_DEBUG_MMC=?FALSE        ; Enable debugging of MMC initialization
_DFS_EMUL=?TRUE          ; Use DFS filesystem number + handles
_FASTGBPB_=?TRUE         ; Include FAST OSGBPB
_DEFAULT_LIB_DRIVE_=?0   ; Default drive for *LIB
_PI1MHZ_FILES_=?(_DEVICE_="1") ; Pi1MHz: disc images through the Pi's file service, not raw card sectors