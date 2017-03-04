tracks%=80:step%=8:max_retry%=10
dfs_rom%=-1:mmfs_rom%=-1
ON ERROR PROCerr
*MMFS
mmfs_rom%=?&DBC
mmfs_rom%?&2A1=0
*DISC
dfs_rom%=?&DBC
MODE 7
@%=3
DIM block% &100, data% &A00*step%
PRINT "MMFS (ROM ";~mmfs_rom%") to DFS (ROM ";~dfs_rom%") Disk Imager"
INPUT "Enter source MMFS disk (0-511): ", from_din%
INPUT "Enter dest DFS drive (0-3): ", to_drive%
FOR base%=0 TO tracks%-1 STEP step%
dfs_rom%?&2A1=0:mmfs_rom%?&2A1=&82:*MMFS
OSCLI "DIN 0 "+STR$(from_din%)
FOR offset%=0 TO step%-1
track%=base%+offset%
block%?0=0
block%!1=data%+&A00*offset%
block%?5=3
block%?6=&53:REM read
block%?7=track%
block%?8=0:REM sector
block%!9=10 OR &1E20:REM sector count
A%=&7F:X%=block% MOD 256:Y%=block% DIV 256:CALL &FFF1
result%=block%?(7+block%?5)
IF result%<>0 THEN PRINT '"Read track ";track%" FAILED! &";~result%:PROCexit:END
NEXT
dfs_rom%?&2A1=&82:mmfs_rom%?&2A1=0:*DISC
FOR offset%=0 TO step%-1
track%=base%+offset%
PRINT track%;" ";
retry%=0
REPEAT
block%?0=to_drive%
block%!1=data%+&A00*offset%
block%?5=3
block%?6=&4B:REM write
block%?7=track%
block%?8=0
block%!9=10 OR &1E20
A%=&7F:X%=block% MOD 256:Y%=block% DIV 256:CALL &FFF1
result%=block%?(7+block%?5)
retry%=retry%+1
UNTIL result%=0 OR retry%>=max_retry%
IF result%<>0 THEN PRINT '"WRITE track ";track%" FAILED! &";~result%:PROCexit:END
NEXT
PRINT
NEXT
PROCexit
END
DEF PROCexit
ON ERROR OFF
IF dfs_rom%>-1:dfs_rom%?&2A1=&82
IF mmfs_rom%>-1:mmfs_rom%?&2A1=&82
*MMFS
ENDPROC
DEF PROCerr
REPORT
PRINT " at line ";ERL
PROCexit
END
