MACRO DEVICE_NAME
If _DEVICE_="T" OR _DEVICE_="T2" OR _DEVICE_="T3"
    EQUS " Turbo"
ELIF _DEVICE_="E"
    EQUS " EPP"
ELIF _DEVICE_="P"
    EQUS " BPP"
ELIF _DEVICE_="C"
    EQUS " CEP"
ELIF _DEVICE_="M"
    EQUS " MM"
ELIF _DEVICE_="1"
    EQUS " Pi1MHz"
ENDIF
ENDMACRO

MACRO DFS_EMUL_NAME
IF NOT(_DFS_EMUL)
    EQUS " xFS"
ENDIF
ENDMACRO

\\ This macro only used in the MM32 builds
MACRO BOOT_NAME
    BASE_NAME
    DEVICE_NAME
    DFS_EMUL_NAME
    EQUB 13, 13
ENDMACRO

MACRO BUILD_NAME
    BASE_NAME
    DEVICE_NAME
    DFS_EMUL_NAME
    EQUB 0
ENDMACRO

MACRO BUILD_COPYRIGHT
    EQUS "(C)2011 Mather", 0
ENDMACRO

MACRO BUILD_VERSION
    EQUS "1.57", 0     \\ #VERSION#
ENDMACRO
