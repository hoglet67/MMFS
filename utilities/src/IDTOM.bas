tracks%=80:step%=8:max_retry%=10
dfs_rom%=-1:mmfs_rom%=-1
ON ERROR PROCerr
*MMFS
mmfs_rom%=?&DBC
PROCswitch(mmfs_rom%, -1):*DISC
dfs_rom%=?&DBC
MODE 7
@%=3
DIM block% &100, data% &A00*step%
PRINT "DFS (ROM ";~dfs_rom%") to MMFS (ROM ";~mmfs_rom%") Disk Imager"
INPUT "Enter source DFS drive (0-3): ", from_drive%
INPUT "Enter dest MMFS disk (0-511): ", to_din%
FOR base%=0 TO tracks%-1 STEP step%
PROCswitch(mmfs_rom%, dfs_rom%):*DISC
FOR offset%=0 TO step%-1
track%=base%+offset%
PRINT track%;" ";
retry%=0
REPEAT
block%?0=from_drive%
block%!1=data%+&A00*offset%
block%?5=3
block%?6=&53:REM read
block%?7=track%
block%?8=0:REM sector
block%!9=10 OR &1E20:REM sector count
A%=&7F:X%=block% MOD 256:Y%=block% DIV 256:CALL &FFF1
result%=block%?(7+block%?5)
retry%=retry%+1
UNTIL result%=0 OR retry%>=max_retry%
IF result%<>0 THEN PRINT '"Read track ";track%" FAILED! &";~result%:PROCexit:END
NEXT
PROCswitch(dfs_rom%, mmfs_rom%):*MMFS
OSCLI "DIN 0 "+STR$(to_din%)
FOR offset%=0 TO step%-1
track%=base%+offset%
block%?0=0
block%!1=data%+&A00*offset%
block%?5=3
block%?6=&4B:REM write
block%?7=track%
block%?8=0
block%!9=10 OR &1E20
A%=&7F:X%=block% MOD 256:Y%=block% DIV 256:CALL &FFF1
result%=block%?(7+block%?5)
IF result%<>0 THEN PRINT '"WRITE track ";track%" FAILED! &";~result%:PROCexit:END
NEXT
PRINT
NEXT
PROCexit
END
DEF PROCswitch(old%, new%)
*FX 143,10
IF old%>-1:old%?&2A1=0
IF new%>-1:new%?&2A1=&82
ENDPROC
DEF PROCexit
ON ERROR OFF
PROCswitch(-1, dfs_rom%)
PROCswitch(-1, mmfs_rom%)
*MMFS
ENDPROC
DEF PROCerr
REPORT
PRINT " at line ";ERL
PROCexit
END
