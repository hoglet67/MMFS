\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** August 2011
\** Includes code from Acorn's DFS 2.26, and DFS 2.24 (Master)

\** MAIN CODE **\

\\ Include *DONBOOT and code to load the default drives on startup
\\ (costs 45 bytes)
_DONBOOT_=NOT(_MM32_)

\\ Enable support for large (>512 disk) MMB files
\\ (costs 128 bytes)
_LARGEMMB_=NOT(_MM32_)

\\ Include *DBASE and code to load the default base on startup
\\ (costs 95 bytes)
_DBASE_=NOT(_MM32_)

;; At the moment, we either include or exclude all the optional commands

;; Normal Commands
_INCLUDE_CMD_ACCESS_=_COMMANDS_
_INCLUDE_CMD_BACKUP_=_COMMANDS_
_INCLUDE_CMD_COMPACT_=_COMMANDS_
_INCLUDE_CMD_COPY_=_COMMANDS_
_INCLUDE_CMD_DELETE_=_COMMANDS_
_INCLUDE_CMD_DESTROY_=_COMMANDS_
_INCLUDE_CMD_ENABLE_=_COMMANDS_
_INCLUDE_CMD_FORM_VERIFY_=_COMMANDS_
_INCLUDE_CMD_FREE_MAP_=_COMMANDS_
_INCLUDE_CMD_RENAME_=_COMMANDS_
_INCLUDE_CMD_TITLE_=_COMMANDS_
_INCLUDE_CMD_WIPE_=_COMMANDS_

;; DUTILS Commands
_INCLUDE_CMD_DBASE_=_COMMANDS_ AND _DBASE_
_INCLUDE_CMD_DCAT_=_COMMANDS_
_INCLUDE_CMD_DDRIVE_=_COMMANDS_
_INCLUDE_CMD_DFREE_=_COMMANDS_
_INCLUDE_CMD_DONBOOT_=_COMMANDS_ AND _DONBOOT_
_INCLUDE_CMD_DOP_=_COMMANDS_
_INCLUDE_CMD_DRECAT_=_COMMANDS_
_INCLUDE_CMD_DABOUT_=_COMMANDS_

;; Additional MMFS2 DUTILs commands
_MM32_DRENAME=NOT(_BP12K_)

\ MA/MP constants must be even numbers
IF _MASTER_
	CPU 1				; 65C12
	MA=&C000-&0E00			; Offset to Master hidden static workspace
ELIF _BP12K_
	; Memory at &Axxx in the 12K private RAM has the (to us) undesirable
	; property that code running there accesses the display RAM, whether
	; that's main or shadow RAM. We therefore can't have any code which
	; needs to access user memory there. To use as much of it as possible up
	; harmlessly, we situate our workspace in that range.
	MA=&A200-&0E00
	UTILSBUF=(&BF-&B6)+HI(MA+&0E00)
	MAEND=(UTILSBUF+1)<<8
ELIF _SWRAM_
	MA=&B600-&0E00
	UTILSBUF=&BF			; Utilities buffer page
ELSE
	MA=0
ENDIF
MP=HI(MA)

INCLUDE "VERSION.asm"
INCLUDE "SYSVARS.asm"			; OS constants

DirectoryParam=&CC
CurrentDrv=&CD

CurrentCat=MA+&1082

IF _MM32_
	TubeNoTransferIf0=MA+&10AE
	MMC_STATE=MA+&10AF
ELSE
	TubeNoTransferIf0=MA+&109E
	MMC_STATE=MA+&109F			; Bit 6 set if card initialised
ENDIF

FSMessagesOnIfZero=MA+&10C6
CMDEnabledIf1=MA+&10C7
DEFAULT_DIR=MA+&10C9
DEFAULT_DRIVE=MA+&10CA
LIB_DIR=MA+&10CB
LIB_DRIVE=MA+&10CC
PAGE=MA+&10CF
RAMBufferSize=MA+&10D0			; HIMEM-PAGE
ForceReset=MA+&10D3
TubePresentIf0=MA+&10D6
CardSort=MA+&10DE

IF _LARGEMMB_
	DiskTableIndex=MA+&10D4
	MACRO DO_ASLA_X4
	IF _SWRAM_
		JSR A_rolx4	; actually 4x ASL A
	ELSE
		ASL A
		ASL A
		ASL A
		ASL A
	ENDIF
	ENDMACRO
	MACRO MASK_DISKNO
		AND #&1F
	ENDMACRO
ELSE
	MACRO MASK_DISKNO
		AND #&01
	ENDMACRO
ENDIF

buf%=MA+&E00
cat%=MA+&E00
FilesX8=MA+&F05

\\ TODO: CardSort should be protected by VID...

IF _MM32_
	VID=MA+&10E0				; VID
	VID2=VID				; 14 bytes
	MMC_CIDCRC=VID2+&E			; 2 bytes
	CHECK_CRC7=VID2+&10			; 1 byte
ELSE
IF _LARGEMMB_
	VID=MA+&10DF				; VID
	CHUNK_BASE=VID+&E			; 1 byte
	NUM_CHUNKS=VID+&F    			; 1 byte
	CHECK_CRC7=VID+&10			; 1 byte
ELSE
	VID=MA+&10E0				; VID
	CHECK_CRC7=VID+&E			; 1 byte
ENDIF
	DRIVE_INDEX0=VID 			; 4 bytes
	DRIVE_INDEX4=VID+4			; 4 bytes
	MMC_SECTOR=VID+8			; 3 bytes
	MMC_SECTOR_VALID=VID+&B			; 1 bytes
	MMC_CIDCRC=VID+&C			; 2 bytes
ENDIF

IF _MM32_
	OWCtlBlock = MA+&10B0		; 16 bytes
ENDIF

IF _DFS_EMUL
	filesysno%=&04			; Filing System Number
	filehndl%=&10			; First File Handle - 1
ELSE
	filesysno%=&74			; Filing System Number
	filehndl%=&70			; First File Handle - 1
ENDIF

tubeid%=&0A			; See Tube Application Note No.004 Page 7
					; &0A is unallocated so shouldn't clash

MACRO BP12K_NEST
	IF _BP12K_
		JSR PageIn12K
		JSR nested
		JMP PageOut12K
	.nested
	ENDIF
ENDMACRO


IF _SWRAM_ AND NOT(_BP12K_)
   guard_value=&B5FE
;; Add a special marker that ZMMFS uses to identify an already installed SWMMFS
   org &B5FE
   EQUB MAGIC0
   EQUB MAGIC1
ELSE
   guard_value=&C000
ENDIF

   ORG &8000
	GUARD guard_value

	\\ ROM Header
.langentry
	BRK
	BRK
	BRK
.serventry
	JMP MMFS_SERVICECALLS

.romtype
	EQUB &82
.copywoffset
	EQUB LO(copyright-1)
.binversion
	EQUB &7B
.title
    BUILD_NAME
.version
    BUILD_VERSION
.copyright
    BUILD_COPYRIGHT
.header_end

.Go_FSCV
	JMP (FSCV)

	\ Illuminate Caps Lock & Shift Lock
.SetLEDS
IF _ELECTRON_
   LDA &282
   EOR #&80
   STA &282
   STA &FE07
ELSE
	LDX #&6
	STX &FE40
	INX
	STX &FE40
ENDIF
	RTS

	\ Reset LEDs
.ResetLEDS
	JSR RememberAXY
	LDA #&76
	JMP OSBYTE

.errDISK
	JSR ReportErrorCB		; Disk Error
	BRK
	EQUS "Disc "
	BCC ErrCONTINUE

.errBAD
	JSR ReportErrorCB		; Bad Error
	BRK
	EQUS "Bad "
	BCC ErrCONTINUE

	\\ **** Report Error ****
	\\ A string terminated with 0 causes JMP &100

	\\ Check if writing channel buffer
.ReportErrorCB
	LDA MA+&10DD			; Error while writing
	BNE brk100_notbuf		; channel buffer?
	JSR ClearEXECSPOOLFileHandle
.brk100_notbuf
	LDA #&FF
	STA CurrentCat
	STA MA+&10DD			; Not writing buffer

.ReportError
	LDX #&02
	LDA #&00			; "BRK"
	STA &0100
.ErrCONTINUE
	\STA &B3			; Save A???
	JSR ResetLEDS
.ReportError2
	PLA 				; Word &AE = Calling address + 1
	STA &AE
	PLA
	STA &AF
	\LDA &B3			; Restore A???
	LDY #&00
	JSR inc_word_AE_and_load
	STA &0101			; Error number
	DEX
.errstr_loop
	INX
	JSR inc_word_AE_and_load
	STA &0100,X
	BMI prtstr_return2		; Bit 7 set, return
	BNE errstr_loop
	JSR TUBE_RELEASE
	JMP &0100

\* print Nibble and fall into print string
.PrintNibble_PrintString
	JSR PrintNibble
	\ **** Print String ****
	\ String terminated if bit 7 set
	\ Exit: AXY preserved, C=0
.PrintString
	STA &B3				; Print String (bit 7 terminates)
	PLA 				; A,X,Y preserved
	STA &AE
	PLA
	STA &AF
	LDA &B3
	PHA 				; Save A & Y
	TYA
	PHA
	LDY #&00
.prtstr_loop
	JSR inc_word_AE_and_load
	BMI prtstr_return1		; If end
	JSR PrintChrA
	\\ PrintChrA uses RememberAXY, so the final instruction is PLA
	\\ which means it's safe to BPL
	BPL prtstr_loop	      		; always
.prtstr_return1
	PLA 				; Restore A & Y
	TAY
	PLA
.prtstr_return2
	CLC
	JMP (&00AE)			; Return to caller

	\ As above sub, but can be spooled
.PrintStringSPL
{
	STA &B3				; Save A
	PLA 				; Pull calling address
	STA &AE
	PLA
	STA &AF
	LDA &B3				; Save A & Y
	PHA
	TYA
	PHA
	LDY #&00
.pstr_loop
	JSR inc_word_AE_and_load
	BMI prtstr_return1
	JSR OSASCI
	JMP pstr_loop
}

IF _MM32_
	\ Print space, on exit A=&20
.PrintSpace
	LDA #' '
	BNE PrintChrA
ENDIF

.PrintNibFullStop
	JSR PrintNibble
.PrintFullStop
	LDA #&2E
.PrintChrA
	JSR RememberAXY			; Print character
	PHA
	LDA #&EC
	JSR osbyte_X0YFF
	TXA 				; X = chr destination
	PHA
	ORA #&10
	JSR osbyte03_Aoutstream		; Disable spooled output
	PLA
	TAX
	PLA
	JSR OSASCI			; Output chr
	JMP osbyte03_Xoutstream		; Restore previous setting

IF _ROMS_
	\ Currently only used in *ROMS so save 3bytes
	\ Print BCD/Hex : A=number
.PrintBCD
	JSR BinaryToBCD
ENDIF
.PrintHex
	PHA
	JSR A_rorx4
	JSR PrintNibble
	PLA
.PrintNibble
	JSR NibToASC
	BNE PrintChrA			; always

	\ As above but allows it to be spooled
.PrintBCDSPL
	JSR BinaryToBCD
.PrintHexSPL
	PHA
	JSR A_rorx4
	JSR PrintNibbleSPL
	PLA
.PrintNibbleSPL
	JSR NibToASC
	JMP OSASCI

	\\ Print spaces, exit C=0 A preserved
.Print2SpacesSPL
	JSR PrintSpaceSPL		; Print 2 spaces
.PrintSpaceSPL
	PHA				; Print space
	LDA #&20
	JSR OSASCI
	PLA
	CLC
	RTS


	\ Convert low nibble to ASCII
.NibToASC
{
	AND #&0F
	CMP #&0A
	BCC nibasc
	ADC #&06
.nibasc
	ADC #&30
	RTS
}

.CopyVarsB0BA
{
	JSR CopyWordB0BA
	DEX
	DEX 				;restore X to entry value
	JSR cpybyte1			;copy word (b0)+y to 1072+x
.cpybyte1
	LDA (&B0),Y
	STA MA+&1072,X
	INX
	INY
	RTS
}

.CopyWordB0BA
{
	JSR cpybyte2			;Note: to BC,X in 0.90
.cpybyte2
	LDA (&B0),Y
	STA &BA,X
	INX
	INY
	RTS
}

.read_fspTextPointer
	JSR Set_CurDirDrv_ToDefaults	; **Read filename to &1000
	JMP rdafsp_entry		; **1st pad &1000-&103F with spaces
.read_fspBA_reset
	JSR Set_CurDirDrv_ToDefaults	; Reset cur dir & drive
.read_fspBA
	LDA &BA				; **Also creates copy at &C5
	STA TextPointer
	LDA &BB
	STA TextPointer+1
	LDY #&00
	JSR GSINIT_A
.rdafsp_entry
	LDX #&20			; Get drive & dir (X="space")
	JSR GSREAD_A			; get C
	BCS errBadName			; IF end of string
	STA MA+&1000
	CMP #&2E			; C="."?
	BNE rdafsp_notdot		; ignore leading ...'s
.rdafsp_setdrv
	STX DirectoryParam		; Save directory (X)
	BEQ rdafsp_entry		; always
.rdafsp_notdot
	CMP #&3A			; C=":"? (Drive number follows)
	BNE rdafsp_notcolon
	JSR Param_DriveNo_BadDrive	; Get drive no.
	JSR GSREAD_A
	BCS errBadName			; IF end of string
	CMP #&2E			; C="."?
	BEQ rdafsp_entry		; err if not eg ":0."

.errBadName
	JSR errBAD
	EQUB &CC
	EQUS "name",0

.rdafsp_notcolon
{
	TAX 				; X=last Chr
	JSR GSREAD_A			; get C
	BCS Rdafsp_padall		; IF end of string
	CMP #&2E			; C="."?
	BEQ rdafsp_setdrv
	LDX #&01			; Read rest of filename
.rdafsp_rdfnloop
	STA MA+&1000,X
	INX
	JSR GSREAD_A
	BCS rdafsp_padX			; IF end of string
	CPX #&07
	BNE rdafsp_rdfnloop
	BEQ errBadName
}

.GSREAD_A
{
	JSR GSREAD			; GSREAD ctrl chars cause error
	PHP 				; C set if end of string reached
	AND #&7F
	CMP #&0D			; Return?
	BEQ dogsrd_exit
	CMP #&20			; Control character? (I.e. <&20)
	BCC errBadName
	CMP #&7F			; Backspace?
	BEQ errBadName
.dogsrd_exit
	PLP
	RTS
}

.SetTextPointerYX
	STX TextPointer
	STY TextPointer+1
	LDY #&00
	RTS

.GSINIT_A
	CLC
	JMP GSINIT

.Rdafsp_padall
	LDX #&01			; Pad all with spaces
.rdafsp_padX
{
	LDA #&20			; Pad with spaces
.rdafsp_padloop
	STA MA+&1000,X
	INX
	CPX #&40			; Why &40? : Wildcards buffer!
	BNE rdafsp_padloop
	LDX #&06			; Copy from &1000 to &C5
.rdafsp_cpyfnloop
	LDA MA+&1000,X			; 7 byte filename
	STA &C5,X
	DEX
	BPL rdafsp_cpyfnloop
	RTS
}

.prt_filename_Yoffset
{
	JSR RememberAXY
	LDA MA+&0E0F,Y
	PHP
	AND #&7F			; directory
	BNE prt_filename_prtchr
	JSR Print2SpacesSPL		; if no dir. print "  "
	BEQ prt_filename_nodir		; always?
.prt_filename_prtchr
	JSR PrintChrA			; print dir
	JSR PrintFullStop		; print "."
.prt_filename_nodir
	LDX #&06			; print filename
.prt_filename_loop
	LDA MA+&0E08,Y
	AND #&7F
	JSR PrintChrA
	INY
	DEX
	BPL prt_filename_loop
	JSR Print2SpacesSPL		; print "  "
	LDA #&20			; " "
	PLP
	BPL prt_filename_notlocked
	LDA #&4C			; "L"
.prt_filename_notlocked
	JSR PrintChrA			; print "L" or " "
	LDY #&01
}

.prt_Yspaces
	JSR PrintSpaceSPL
	DEY
	BNE prt_Yspaces
	RTS


.A_rorx6and3
	LSR A
	LSR A
.A_rorx4and3
	LSR A
	LSR A
.A_rorx2and3
	LSR A
	LSR A
	AND #&03
	RTS

.A_rorx5
	LSR A
.A_rorx4
	LSR A
.A_rorx3
	LSR A
	LSR A
	LSR A
	RTS

.A_rolx5
	ASL A
.A_rolx4
	ASL A
	ASL A
	ASL A
	ASL A
.getcat_exit
	RTS

.parameter_afsp_Param_SyntaxErrorIfNull_getcatentry_fspTxtP
	JSR parameter_afsp
.Param_SyntaxErrorIfNull_getcatentry_fspTxtP
	JSR Param_SyntaxErrorIfNull
.getcatentry_fspTxtP
	JSR read_fspTextPointer
	BMI getcatentry			;always
.getcatentry_fspBA
	JSR read_fspBA_reset
.getcatentry
	JSR get_cat_firstentry80
	BCS getcat_exit

.err_FILENOTFOUND
	JSR ReportError
	EQUB &D6
	EQUS "Not found",0		; Not Found error




\ *EX (<dir>)
.fscv9_starEX
	JSR SetTextPointerYX
.CMD_EX
{
	JSR Set_CurDirDrv_ToDefaults
	JSR GSINIT_A
	BEQ cmd_ex_nullstr		; If null string
	JSR ReadDirDrvParameters2	; Get dir
.cmd_ex_nullstr
	LDA #&2A			; "*"
	STA MA+&1000
	JSR Rdafsp_padall
	JSR parameter_afsp
	JSR getcatentry
	JMP cmd_info_loop
}

	\ *INFO <afsp>
.fscv10_starINFO
	JSR SetTextPointerYX
	LDA #info_cmd_index - cmdtable1-1  ; BF needs to point to the INFO command
	STA &BF                            ; Param_SyntaxErrorIfNull to work
.CMD_INFO
	JSR parameter_afsp_Param_SyntaxErrorIfNull_getcatentry_fspTxtP
.cmd_info_loop
	JSR prt_InfoLine_Yoffset
	JSR get_cat_nextentry
	BCS cmd_info_loop
	RTS


.get_cat_firstentry81
	JSR CheckCurDrvCat		; Get cat entry
	LDA #&00
	BEQ getcatentry2		; always

.get_cat_firstentry80fname
	LDX #&06			; copy filename from &C5 to &1058
.getcatloop1
	LDA &C5,X
	STA MA+&1058,X
	DEX
	BPL getcatloop1
	LDA #&20
	STA MA+&105F
	LDA #&58
	BNE getcatentry1		; always

.get_cat_nextentry
	LDX #&00			; Entry: wrd &B6 -> first entry
	BEQ getcatloop2			; always

.get_cat_firstentry80
	LDA #&00			; now first byte @ &1000+X
.getcatentry1
	PHA 				; Set up & return first
	JSR CheckCurDrvCat		; catalogue entry matching
	PLA 				; string at &1000+A
.getcatentry2
	TAX
	LDA #&00			; word &B6 = &E00 = PTR
	STA &B6
.getcatloop2
	LDY #&00
	LDA #MP+&0E			; string at &1000+A
	STA &B7
	LDA &B6
	CMP FilesX8
	BCS matfn_exitC0		; If >FilesX8 Exit with C=0
	ADC #&08
	STA &B6				; word &B6 += 8
	JSR MatchFilename
	BCC getcatloop2			; not a match, try next file
	LDA DirectoryParam
	LDY #&07
	JSR MatchChr
	BNE getcatloop2			; If directory doesn't match
	LDY &B6
	SEC 				; Return, Y=offset-8, C=1
.Y_sub8
	DEY
	DEY
	DEY
	DEY
	DEY
	DEY
	DEY
	DEY
	RTS

.MatchFilename
{
	JSR RememberAXY			; Match filename at &1000+X
.matfn_loop1
	LDA MA+&1000,X			; with that at (&B6)
	CMP MA+&10CE
	BNE matfn_nomatch		; e.g. If="*"
	INX
.matfn_loop2
	JSR MatchFilename
	BCS matfn_exit			; If match then exit with C=1
	INY
	CPY #&07
	BCC matfn_loop2			; If Y<7
.matfn_loop3
	LDA MA+&1000,X			; Check next char is a space!
	CMP #&20
	BNE matfn_exitC0		; If exit with c=0 (no match)
	RTS 				; exit with C=1
.matfn_nomatch
	CPY #&07
	BCS matfn_loop3			; If Y>=7
	JSR MatchChr
	BNE matfn_exitC0
	INX
	INY
	BNE matfn_loop1			; next chr
}
.matfn_exitC0
	CLC 				; exit with C=0
.matfn_exit
	RTS

.MatchChr
{
	CMP MA+&10CE
	BEQ matchr_exit			; eg. If "*"
	CMP MA+&10CD
	BEQ matchr_exit			; eg. If "#"
	JSR IsAlphaChar
	EOR (&B6),Y
	BCS matchr_notalpha		; IF not alpah char
	AND #&5F
.matchr_notalpha
	AND #&7F
.matchr_exit
	RTS 				; If n=1 then matched
}

.UcaseA2
{
	PHP
	JSR IsAlphaChar
	BCS ucasea
	AND #&5F			; A=Ucase(A)
.ucasea
	AND #&7F			; Ignore bit 7
	PLP
	RTS
}

.DeleteCatEntry_YFileOffset
{
	JSR CheckFileNotLockedOrOpenY	; Delete catalogue entry
.delcatloop
	LDA MA+&0E10,Y
	STA MA+&0E08,Y
	LDA MA+&0F10,Y
	STA MA+&0F08,Y
	INY
	CPY FilesX8
	BCC delcatloop
	TYA
	SBC #&08
	STA FilesX8
	CLC
}
.print_infoline_exit
	RTS

.IsAlphaChar
{
	PHA
	AND #&5F			; Uppercase
	CMP #&41
	BCC isalpha1			; If <"A"
	CMP #&5B
	BCC isalpha2			; If <="Z"
.isalpha1
	SEC
.isalpha2
	PLA
	RTS
}

.prt_InfoMsg_Yoffset
	BIT FSMessagesOnIfZero		; Print message
	BMI print_infoline_exit
.prt_InfoLine_Yoffset
	JSR RememberAXY			; Print info
	JSR prt_filename_Yoffset
	TYA 				; Save offset
	PHA
	LDA #&60			; word &B0=1060
	STA &B0
	LDA #MP+&10
	STA &B1
	JSR ReadFileAttribsToB0_Yoffset	; create no. str
	LDY #&02
	JSR PrintSpaceSPL		; print "  "
	JSR PrintHex3Byte		; Load address
	JSR PrintHex3Byte		; Exec address
	JSR PrintHex3Byte		; Length
	PLA
	TAY
	LDA MA+&0F0E,Y			; First sector high bits
	AND #&03
	JSR PrintNibble
	LDA MA+&0F0F,Y			; First sector low byte
	JSR PrintHex

	\ Print New Line
.PrintNewLine
	PHA
	LDA #&0D
	JSR PrintChrA
	PLA
	RTS

.PrintHex3Byte
{
	LDX #&03			; eg print "123456 "
.printhex3byte_loop
	LDA MA+&1062,Y
	JSR PrintHex
	DEY
	DEX
	BNE printhex3byte_loop
	JSR Y_add7
	JMP PrintSpaceSPL
}



.ReadFileAttribsToB0_Yoffset
{
	JSR RememberAXY			; Decode file attribs
	TYA
	PHA 				; bytes 2-11
	TAX 				; X=cat offset
	LDY #&12			; Y=(B0) offset
	LDA #&00			; Clear pwsp+2 to pwsp+&11
.readfileattribs_clearloop
	DEY
	STA (&B0),Y
	CPY #&02
	BNE readfileattribs_clearloop
.readfileattribs_copyloop
	JSR readfileattribs_copy2bytes	; copy low bytes of
	INY 				; load/exec/length
	INY
	CPY #&0E
	BNE readfileattribs_copyloop
	PLA
	TAX
	LDA MA+&0E0F,X
	BPL readfileattribs_notlocked	; If not locked
	LDA #&08
	STA (&B0),Y			; pwsp+&E=8
.readfileattribs_notlocked
	LDA MA+&0F0E,X			; mixed byte
	LDY #&04			; load address high bytes
	JSR readfileattribs_addrHiBytes
	LDY #&0C			; file length high bytes
	LSR A
	LSR A
	PHA
	AND #&03
	STA (&B0),Y
	PLA
	LDY #&08			; exec address high bytes
.readfileattribs_addrHiBytes
	LSR A
	LSR A				; /4
	PHA
	AND #&03
	CMP #&03			; done slightly diff. to 8271
	BNE readfileattribs_nothost
	LDA #&FF
	STA (&B0),Y
	INY
.readfileattribs_nothost
	STA (&B0),Y
.readfileattribs_exits
	PLA
	RTS
.readfileattribs_copy2bytes
	JSR readfileattribs_copy1byte
.readfileattribs_copy1byte
	LDA MA+&0F08,X
	STA (&B0),Y
	INX
	INY
	RTS
}

.inc_word_AE_and_load
{
	INC &AE
	BNE inc_word_AE_exit
	INC &AF
.inc_word_AE_exit
	LDA (&AE),Y
	RTS
}

	\\ Save AXY and restore after
	\\ calling subroutine exited
.RememberAXY
	PHA
	TXA
	PHA
	TYA
	PHA
	LDA #HI(rAXY_restore-1)		; Return to rAXY_restore
	PHA
	LDA #LO(rAXY_restore-1)
	PHA

.rAXY_loop_init
{
	LDY #&05
.rAXY_loop
	TSX
	LDA &0107,X
	PHA
	DEY
	BNE rAXY_loop
	LDY #&0A
.rAXY_loop2
	LDA &0109,X
	STA &010B,X
	DEX
	DEY
	BNE rAXY_loop2
	PLA
	PLA
}

.rAXY_restore
	PLA
	TAY
	PLA
	TAX
	PLA
	RTS

.RememberXYonly
	PHA
	TXA
	PHA
	TYA
	PHA
	JSR rAXY_loop_init
.axyret1
	TSX
	STA &0103,X
	JMP rAXY_restore


IF NOT(_MM32_)
IF _LARGEMMB_
	\ ** Convert 12 bit binary in word &B8/9 to
	\ ** 4 digit BCD in word &B5/6 (decno%)
decno%=&B5
	\Used by GetDisk
.DecNo_BIN2BCD
{
	SED		; Switch to decimal mode
	LDA #0		; Ensure the result is clear
	STA decno%+0
	STA decno%+1
;	STA decno%+2
	LDX #16		; The number of source bits
.loop
	ROL &B8		; Shift out one bit
	ROL &B9
	PHP
	LDA decno%+0	; And add into result
	ADC decno%+0
	STA decno%+0
	LDA decno%+1	; propagating any carry
	ADC decno%+1
	STA decno%+1
;	LDA decno%+2	; ... thru whole result
;	ADC decno%+2
;	STA decno%+2
	PLP
	DEX		; And repeat for next bit
	BNE loop
	ROL &B8		; Restore the original value
	ROL &B9
	CLD		; Back to binary
	RTS
}

ELSE
	\ ** Convert 9 bit binary in word &B8 to
	\ ** 3 digit BCD in word &B5 (decno%)
decno%=&B5
	\Used by GetDisk
.DecNo_BIN2BCD
{
	LDX &B8
	LDY #0
	STY decno%+1
.b2b_loop
	TXA
	CMP #10
	BCC b2b_exloop
	SBC #10
	TAX
	SED
	CLC
	TYA
	ADC #&10
	TAY
	CLD
	BCC b2b_loop
	INC decno%+1
	BCS b2b_loop
.b2b_exloop
	SED
	STY decno%
	ADC decno%
	TAY
	BCC b2b_noinc
	INC decno%+1
	CLC
.b2b_noinc
	LDX &B9 			; >&FF ?
	BEQ b2b_noadd
	ADC #&56
	TAY
	LDA decno%+1
	ADC #&02
	STA decno%+1
.b2b_noadd
	CLD
	STY decno%
	RTS
}
ENDIF
ENDIF

	\ Convert binary in A to BCD
.BinaryToBCD
{
	JSR RememberXYonly
	TAY
	BEQ bbcd_exit			; If nothing to do!
	CLC
	SED
	LDA #&00
.bbcd_loop
	ADC #&01
	DEY
	BNE bbcd_loop
	CLD
.bbcd_exit
	RTS 				; A=BCD
}

IF _MM32_
	\ MM32 reverts to (modified) original DFS code.
	\ Convert decimal to binary
	\ On exit: C=1 if error, else X=number
.Param_ReadNum
{
	JSR GSINIT_A
	SEC
	BEQ L4			;If null str

	PHP
	LDA #0
	STA &B9
	BEQ L2			;always

.L1	SEC
	SBC #&30
	BCC L3			;IF K<'0'

	CMP #&0A
	BCS L3			;If K>'9'

	STA &B8
	LDA &B9
	ASL A
	STA &B9
	ASL A
	ASL A
	ADC &B9
	ADC &B8
	STA &B9			;?B9=?B9 X 10 + ?B8

.L2	JSR GSREAD		;K=chr
	BCC L1			;If not end of str

	LDX &B9
	PLP
	CLC
	RTS

.L3	PLP				;C=1, Z=0

.L4	RTS
}
ELSE
	\\ **** Read decimal number at TxtPtr+Y ****
	\\ on exit;
	\\ if valid (0 - 510);
	\\ C=0, AX=number, &
	\\ Y points to next chr
	\\ if not valid;
	\\ C=1, AX=0, &
	\\ Y=entry value
	\\ Exit: Uses memory &B0 + &B1

rn%=&B0

.Param_ReadNum
{
	TYA
	PHA				;\ 1
	LDA #0
	STA rn%
	STA rn%+1

	JSR GSINIT_A
	BEQ rnnotval			; If null string
	CMP #&22
	BEQ rnnotval

	JSR GSREAD
	BCS rnnotval			; Should never happen!
.rnloop
	\ C=0
	SBC #'0' - 1			; -1 as C=0 to save SED
	BMI rnnotval
	CMP #10
	BCS rnnotval

	\\ N=N*2+N*8+A
	PHA				;\ 2
	LDA rn%
	ASL A
	PHA				;\ 3
	ROL rn%+1
	LDX rn%+1
	ASL A
	ROL rn%+1
	ASL A
	ROL rn%+1
	STA rn%
	PLA				;\ 3
	ADC rn%
	STA rn%
	TXA
	ADC rn%+1
	TAX
	PLA				;\ 2
	ADC rn%
	STA rn%
	TXA
	ADC #0
	STA rn%+1
IF _LARGEMMB_
	\\ Limit to 8192 so final check doesn't overflow
	CMP #&20
ELSE
	CMP #2
ENDIF
	BCS rnnotval

	JSR GSREAD
	BCC rnloop

IF _LARGEMMB_
	LDX #rn%
	JSR calculate_div_mod_511_zp_x
	BCS rnnotval
	\ C=0
	LDX rn%
	PLA 				;\ 1 - ignore Y
	LDA rn%+1
	RTS
ELSE
	\\ <>511?
.rnexit
	LDX rn%
	LDA rn%+1
	BEQ rnok
	INX
	BEQ rnnotval
	DEX
.rnok
	PLA 				;\ 1 - ignore Y
	LDA rn%+1
	CLC
	RTS
ENDIF

	\ Not a valid number, restore Y and set C
.rnnotval
	PLA				;\ 1
	TAY 				; restore Y
	LDA #0
	TAX
	SEC
	RTS
}
ENDIF


.fscv5_starCAT
{
	JSR SetTextPointerYX
	JSR Param_OptionalDriveNo
	JSR LoadCurDrvCat

	LDY #&FF			; ** PRINT CAT
	STY &A8				; Y=FF
	INY
	STY &AA				; Y=0
.cat_titleloop
	LDA MA+&0E00,Y			; print disk title
	CPY #&08
	BCC cat_titlelo
	LDA MA+&0EF8,Y
.cat_titlelo
	JSR PrintChrA
	INY
	CPY #&0C
	BNE cat_titleloop
	JSR PrintString			; Print " (n) "; n=cycle no.
	EQUS " ("			; Print "Drive "

IF _MM32_
	LDA MA+&0F04
	JSR PrintHex
ELSE
	\ Print disk no. instead of cycle no.
	LDX CurrentDrv
	JSR PrtDiskNo
ENDIF

	JSR PrintString
	EQUS ")",13,"Drive "
	LDA CurrentDrv
	JSR PrintNibble			; print drv.no.
	LDY #&0D
	JSR prt_Yspaces			; print 13 spaces
	JSR PrintString
	EQUS "Option "
	LDA MA+&0F06
	JSR A_rorx4
	PHA
		; print option.no
	JSR PrintNibble_PrintString			; print " ("
	EQUS " ("
	LDY #&03			; print option.name
	PLA
	ASL A
	ASL A
	TAX
.cat_printoptionnameloop
	LDA diskoptions_table,X
	JSR PrintChrA
	INX
	DEY
	BPL cat_printoptionnameloop
	JSR PrintString			; print ") Dir. :"
	EQUS ")",13,"Dir. :"
	LDA DEFAULT_DRIVE
	JSR PrintNibFullStop		; print driveno+"."
	LDA DEFAULT_DIR
	JSR PrintChrA			; print dir
	LDY #&0B
	JSR prt_Yspaces			; print 11 spaces
	JSR PrintString
	EQUS "Lib. :"			; print "Lib. :"
	LDA LIB_DRIVE
	JSR PrintNibFullStop		; print library.drv+"."
	LDA LIB_DIR
	JSR PrintChrA			; print library.dir
	JSR PrintNewLine		; print
	LDY #&00			; Mark files in cur dir
.cat_curdirloop
	CPY FilesX8			; no.of.files?
	BCS cat_sortloop1		; If @ end of catalogue
	LDA MA+&0E0F,Y
	EOR DEFAULT_DIR
	AND #&5F
	BNE cat_curdirnext		; If not current dir
	LDA MA+&0E0F,Y			; Set dir to null, sort=>first
	AND #&80			; Keep locked flag (bit 7)
	STA MA+&0E0F,Y
.cat_curdirnext
	JSR Y_add8
	BCC cat_curdirloop		; always
.cat_sortloop1
	LDY #&00			; Any unmarked files?
	JSR cat_getnextunmarkedfileY
	BCC cat_printfilename		; If yes
	LDA #&FF
	STA CurrentCat
	JMP PrintNewLine		; ** EXIT OF PRINT CAT
.cat_getnextunmarkedfile_loop
	JSR Y_add8
.cat_getnextunmarkedfileY
	CPY FilesX8
	BCS cat_exit			; If @ end of cat exit, c=1
	LDA MA+&0E08,Y
	BMI cat_getnextunmarkedfile_loop	; If marked file
.cat_exit
	RTS

.cat_printfilename
	STY &AB				; save Y=cat offset
	LDX #&00
.cat_copyfnloop
	LDA MA+&0E08,Y			; Copy filename to 1060
	JSR UcaseA2
	STA MA+&1060,X
	INY
	INX
	CPX #&08
	BNE cat_copyfnloop		; Chk fn < all other unmarked files
.cat_comparefnloop1
	JSR cat_getnextunmarkedfileY	; Next unmarked file
	BCS cat_printfn			; If last file, so print anyway
	SEC
	LDX #&06
.cat_comparefnloop2
	LDA MA+&0E0E,Y			; compare filenames
	JSR UcaseA2			; (catfn-memfn)
	SBC MA+&1060,X
	DEY
	DEX
	BPL cat_comparefnloop2
	JSR Y_add7
	LDA MA+&0E0F,Y			; compare dir
	JSR UcaseA2			; (clrs bit 7)
	SBC MA+&1067
	BCC cat_printfilename		; If catfn<memfn
	JSR Y_add8
	BCS cat_comparefnloop1		; else memfn>catfn
.cat_printfn
	LDY &AB				; Y=cat offset
	LDA MA+&0E08,Y			; mark file as printed
	ORA #&80
	STA MA+&0E08,Y
	LDA MA+&1067			; dir
	CMP &AA				; dir being printed
	BEQ cat_samedir			; If in same dir
	LDX &AA
	STA &AA				; Set dir being printed
	BNE cat_samedir			; If =0 =default dir
	JSR PrintNewLine		; Two newlines after def dir
.cat_newline
	JSR PrintNewLine
	LDY #&FF
	BNE cat_skipspaces		; always => ?&A8=0
.cat_samedir
	LDY &A8				; [if ?&A0<>0 = first column]
	BNE cat_newline
	LDY #&05			; print column gap
	JSR prt_Yspaces			; print 5 spaces => ?&A8=1
.cat_skipspaces
	INY
	STY &A8
	LDY &AB				; Y=cat offset
	JSR Print2SpacesSPL		; print 2 spaces
	JSR prt_filename_Yoffset	; Print filename
	JMP cat_sortloop1
}


.diskoptions_table
	EQUS "off",0,"LOAD"
	EQUS "RUN",0,"EXEC"


.Getnextblock_Yoffset
	LDA MA+&0F0E,Y
	JSR A_rorx4and3
	STA &C2				;len byte 3
	CLC
	LDA #&FF			; -1
	ADC MA+&0F0C,Y			; + len byte 1
	LDA MA+&0F0F,Y			; + start sec byte 1
	ADC MA+&0F0D,Y			; + len byte 2
	STA &C3
	LDA MA+&0F0E,Y			; start sec byte 2
	AND #&03
	ADC &C2				; calc. next "free" sector
	STA &C2				; wC2=start sec + len - 1
.Getfirstblock_Yoffset
	SEC
	LDA MA+&0F07,Y			; secs on disk
	SBC &C3				; or start sec of prev.
	PHA 				; file
	LDA MA+&0F06,Y			; - end of prev. file (wC2)
	AND #&03
	SBC &C2
	TAX
	LDA #&00
	CMP &C0
	PLA 				; ax=secs on disk-next blk
	SBC &C1
	TXA 				; req'd=c0/c1/c4
	SBC &C4				; big enough?
.NotCmdTable2
	RTS


	\ COMMAND TABLE 1		; MMFS commands
.cmdtable1
	EQUB &FF			; Last command number (-1)
IF _INCLUDE_CMD_ACCESS_
	EQUS "ACCESS"			; Command string
	EQUB &80+&32			; Parameters (Bit 7 is string terminator)
ENDIF
IF _INCLUDE_CMD_BACKUP_
	EQUS "BACKUP"
	EQUB &80+&0C
ENDIF
	EQUS "CLOSE"
	EQUB &80
IF _INCLUDE_CMD_COMPACT_
	EQUS "COMPACT"
	EQUB &80+&05
ENDIF
IF _INCLUDE_CMD_COPY_
	EQUS "COPY"
	EQUB &80+&2C
ENDIF
IF _INCLUDE_CMD_DELETE_
	EQUS "DELETE"
	EQUB &80+&08
ENDIF
IF _INCLUDE_CMD_DESTROY_
	EQUS "DESTROY"
	EQUB &80+&02
ENDIF
	EQUS "DIR"
	EQUB &80+&06
	EQUS "DRIVE"
	EQUB &80+&01
IF _INCLUDE_CMD_ENABLE_
	EQUS "ENABLE"
	EQUB &80
ENDIF
	EQUS "EX"
	EQUB &80+&06
IF _INCLUDE_CMD_FORM_VERIFY_
	EQUS "FORM"
	EQUB &80+&5F
ENDIF
IF _INCLUDE_CMD_FREE_MAP_
	EQUS "FREE"
	EQUB &80+&04
ENDIF
.info_cmd_index
	EQUS "INFO"
	EQUB &80+&02
	EQUS "LIB"
	EQUB &80+&06
IF _INCLUDE_CMD_FREE_MAP_
	EQUS "MAP"
	EQUB &80+&04
ENDIF
IF _INCLUDE_CMD_RENAME_
	EQUS "RENAME"
	EQUB &80+&0D
ENDIF
IF _INCLUDE_CMD_TITLE_
	EQUS "TITLE"
	EQUB &80+&0A
ENDIF
IF _INCLUDE_CMD_FORM_VERIFY_
	EQUS "VERIFY"
	EQUB &80+&05
ENDIF
IF _INCLUDE_CMD_WIPE_
	EQUS "WIPE"
	EQUB &80+&02
ENDIF
	BRK				; End of table

	\ COMMAND TABLE 2
.cmdtable2				; UTILS commands
IF _UTILS_ OR _ROMS_
	EQUB (cmdaddr2-cmdaddr1)/2-1
IF _UTILS_
	EQUS "BUILD"
	EQUB &80+&08
	EQUS "DUMP"
	EQUB &80+&08
	EQUS "LIST"
	EQUB &80+&08
ENDIF
IF _ROMS_
	EQUS "ROMS"
	EQUB &80+&0B
ENDIF
IF _UTILS_
	EQUS "TYPE"
	EQUB &80+&08
ENDIF
	BRK
ENDIF

.cmdtable22
	EQUB (cmdaddr22-cmdaddr1)/2-1
IF _DFS_EMUL
	EQUS "DISC"
	EQUB &80
	EQUS "DISK"
	EQUB &80
ENDIF
	EQUS "MMFS"
	EQUB &80
	EQUS "CARD"
	EQUB &80
	BRK

	\ COMMAND TABLE 3		; HELP
.cmdtable3
	EQUB (cmdaddr3-cmdaddr1)/2-1
	EQUS "DUTILS"
	EQUB &80
	EQUS "MMFS"
	EQUB &80
IF _UTILS_ OR _ROMS_
	EQUS "UTILS"
	EQUB &80
ENDIF
	BRK

	\ COMMAND TABLE 4		; DUTILS commands
.cmdtable4
	EQUB (cmdaddr4-cmdaddr1)/2-1
IF _MM32_
	EQUS "ACCESS"
	EQUB &80+&3E
	EQUS "BOOT"
	EQUB &80+&09
IF _MM32_DEBUG
	EQUS "BUG"
	EQUB &80
ENDIF
	EQUS "CAT"
	EQUB &80+&0E
	EQUS "DIR"
	EQUB &80+&09
	EQUS "DRIVE"
	EQUB &80
IF _MM32_DDUMP
	EQUS "DUMP"
	EQUB &80+&04
ENDIF
	EQUS "IN"
	EQUB &80+&74
	EQUS "OUT"
	EQUB &80+&04
IF _MM32_DRENAME
	EQUS "RENAME"
	EQUB &80+&77
ENDIF
ELSE
IF _INCLUDE_CMD_DBASE_
	EQUS "BASE"
	EQUB &80+&0B
ENDIF
	EQUS "BOOT"
	EQUB &80+&07
IF _INCLUDE_CMD_DCAT_
	EQUS "CAT"
	EQUB &80+&0E
ENDIF
IF _INCLUDE_CMD_DDRIVE_
	EQUS "DRIVE"
	EQUB &80+&04
ENDIF
IF _INCLUDE_CMD_DFREE_
	EQUS "FREE"
	EQUB &80
ENDIF
	EQUS "IN"
	EQUB &80+&74
IF _INCLUDE_CMD_DONBOOT_
	EQUS "ONBOOT"
	EQUB &80+&74
ENDIF
IF _INCLUDE_CMD_DOP_
	EQUS "OP"
	EQUB &80+&49
ENDIF
	EQUS "OUT"
	EQUB &80+&04
IF _INCLUDE_CMD_DRECAT_
	EQUS "RECAT"
	EQUB &80
ENDIF
ENDIF
IF _INCLUDE_CMD_DABOUT_
	EQUS "ABOUT"
	EQUB &80
ENDIF
	BRK

	\\ Address of sub-routines
	\\ If bit 15 clear, call MMC_BEGIN2
.cmdaddr1
IF _INCLUDE_CMD_ACCESS_
	EQUW CMD_ACCESS-1
ENDIF
IF _INCLUDE_CMD_BACKUP_
	EQUW CMD_BACKUP-1
ENDIF
	EQUW CMD_CLOSE-1
IF _INCLUDE_CMD_COMPACT_
	EQUW CMD_COMPACT-1
ENDIF
IF _INCLUDE_CMD_COPY_
	EQUW CMD_COPY-1
ENDIF
IF _INCLUDE_CMD_DELETE_
	EQUW CMD_DELETE-1
ENDIF
IF _INCLUDE_CMD_DESTROY_
	EQUW CMD_DESTROY-1
ENDIF
	EQUW CMD_DIR-1
	EQUW CMD_DRIVE-1
IF _INCLUDE_CMD_ENABLE_
	EQUW CMD_ENABLE-1
ENDIF
	EQUW CMD_EX-1
IF _INCLUDE_CMD_FORM_VERIFY_
	EQUW CMD_FORM-&8001
ENDIF
IF _INCLUDE_CMD_FREE_MAP_
	EQUW CMD_FREE-1
ENDIF
	EQUW CMD_INFO-1
	EQUW CMD_LIB-1
IF _INCLUDE_CMD_FREE_MAP_
	EQUW CMD_MAP-1
ENDIF
IF _INCLUDE_CMD_RENAME_
	EQUW CMD_RENAME-1
ENDIF
IF _INCLUDE_CMD_TITLE_
	EQUW CMD_TITLE-1
ENDIF
IF _INCLUDE_CMD_FORM_VERIFY_
	EQUW CMD_VERIFY-&8001
ENDIF
IF _INCLUDE_CMD_WIPE_
	EQUW CMD_WIPE-1
ENDIF
	EQUW NotCmdTable1-1

.cmdaddr2
IF _UTILS_ OR _ROMS_
IF _UTILS_
	EQUW CMD_BUILD-1

	EQUW CMD_DUMP-1
	EQUW CMD_LIST-1
ENDIF
IF _ROMS_
	EQUW CMD_ROMS-&8001
ENDIF
IF _UTILS_
	EQUW CMD_TYPE-1
ENDIF
	EQUW NotCmdTable2-1
ENDIF

.cmdaddr22
IF _DFS_EMUL
	EQUW CMD_DISC-1
	EQUW CMD_DISC-1
ENDIF
	EQUW CMD_CARD-1
	EQUW CMD_CARD-1
	EQUW NotCmdTable22-1

.cmdaddr3
	EQUW CMD_DUTILS-1
	EQUW CMD_MMFS-1
IF _UTILS_ OR _ROMS_
	EQUW CMD_UTILS-1
ENDIF
	EQUW CMD_NOTHELPTBL-1

.cmdaddr4
IF _MM32_
	EQUW mm32_cmd_daccess-&8001
	EQUW mm32_cmd_dboot-&8001
IF _MM32_DEBUG
	EQUW mm32_cmd_dbug-&8001
ENDIF
	EQUW mm32_cmd_dcat-&8001
	EQUW mm32_cmd_ddir-&8001
	EQUW mm32_cmd_ddrive-&8001
IF _MM32_DDUMP
	EQUW mm32_cmd_ddump-&8001
ENDIF
	EQUW mm32_cmd_din-&8001
	EQUW mm32_cmd_dout-&8001
IF _MM32_DRENAME
	EQUW mm32_cmd_drename-&8001
ENDIF
ELSE
IF _INCLUDE_CMD_DBASE_
	EQUW CMD_DBASE-&8001
ENDIF
	EQUW CMD_DBOOT-&8001
IF _INCLUDE_CMD_DCAT_
	EQUW CMD_DCAT-&8001
ENDIF
IF _INCLUDE_CMD_DDRIVE_
	EQUW CMD_DDRIVE-&8001
ENDIF
IF _INCLUDE_CMD_DFREE_
	EQUW CMD_DFREE-&8001
ENDIF
	EQUW CMD_DIN-&8001
IF _INCLUDE_CMD_DONBOOT_
	EQUW CMD_DONBOOT-&8001
ENDIF
IF _INCLUDE_CMD_DOP_
	EQUW CMD_DOP-&8001
ENDIF
	EQUW CMD_DOUT-&8001
IF _INCLUDE_CMD_DRECAT_
	EQUW CMD_DRECAT-&8001
ENDIF
ENDIF
IF _INCLUDE_CMD_DABOUT_
	EQUW CMD_DABOUT-1
ENDIF
	EQUW NotCmdTable4-1
.cmdaddrX

cmdtab1size= (cmdaddr2-cmdaddr1)/2-1
cmdtab2size= (cmdaddr22-cmdaddr2)/2-1
cmdtab22size= (cmdaddr3-cmdaddr22)/2-1
cmdtab3size= (cmdaddr4-cmdaddr3)/2-1
cmdtab4size= (cmdaddrX-cmdaddr4)/2-1

cmdtab2= cmdtable2-cmdtable1
cmdtab22= cmdtable22-cmdtable1
cmdtab3= cmdtable3-cmdtable1
cmdtab4= cmdtable4-cmdtable1

	\ End of address tables


.NotCmdTable1
	LDX #cmdtab4
	JSR GSINIT_A
	LDA (TextPointer),Y
	INY
	ORA #&20
	CMP #&64			; "d"
	BEQ UnrecCommandTextPointer
	DEY
	JMP NotCmdTable4

.fscv3_unreccommand
	JSR SetTextPointerYX
	LDX #&00

.UnrecCommandTextPointer
{
	LDA cmdtable1,X			; Get number of last command
	STA &BE
	TYA 				; X=FD+3=0 ie all commands
	PHA 				; X=start command,

.unrecloop1
	INC &BE

	PLA 				; contain addr/code of prev.
	PHA
	TAY 				; restore Y
	JSR GSINIT_A			; TextPointer+Y = cmd line

	INX
	LDA cmdtable1,X
	BEQ gocmdcode			; If end of table

	DEX
	DEY
	STX &BF				; USED IF SYNTAX ERROR

.unrecloop2
	INX
	INY 				; X=start of next string-1
	LDA cmdtable1,X
	BMI endofcmd_oncmdline

.unrecloop2in
	EOR (TextPointer),Y		; end of table entry - matched!
	AND #&5F
	BEQ unrecloop2			; ignore case
	DEX 				; while chrs eq go loop2

.unrecloop3
	INX 				; init next loop
	LDA cmdtable1,X
	BPL unrecloop3

	LDA (TextPointer),Y		; find end of table entry
	CMP #&2E			; does cmd line end with
	BNE unrecloop1			; full stop?
	INY 				; If no, doesn't match
	BCS gocmdcode

.endofcmd_oncmdline
	LDA (TextPointer),Y		; If >="." (always)
	JSR IsAlphaChar			; matched table entry
	BCC unrecloop1

.gocmdcode
	PLA 				; if more chars.

	\jsr PrintString
	\equs "COMMAND: "

	\lda &BE
	\jsr PrintHex
	\jsr PrintNewLine

	LDA &BE
	ASL A
	TAX
	LDA cmdaddr1+1,X		; Forget Y
	BPL dommcinit
.gocmdcode2
	PHA				; Push sub address and
	LDA cmdaddr1,X			; return to it!
	PHA
	RTS

.dommcinit
	JSR MMC_BEGIN2
	ORA #&80
	BNE gocmdcode2
}

IF _INCLUDE_CMD_WIPE_
.CMD_WIPE
{
	JSR parameter_afsp_Param_SyntaxErrorIfNull_getcatentry_fspTxtP
.wipeloop
	LDA MA+&0E0F,Y
	BMI wipenext			; Ignore locked files
	JSR prt_filename_Yoffset
	JSR ConfirmYNcolon		; Confirm Y/N
	BNE wipenext
	LDX &B6
	JSR CheckForDiskChange
	STX &B6
	JSR DeleteCatEntry_AdjustPtr
	STY &AB
	JSR SaveCatToDisk
	LDA &AB
	STA &B6
.wipenext
	JSR get_cat_nextentry
	BCS wipeloop
	RTS
}
ENDIF

IF _INCLUDE_CMD_DELETE_
.CMD_DELETE
{
	JSR parameter_fsp
	JSR Param_SyntaxErrorIfNull_getcatentry_fspTxtP
	JSR prt_InfoMsg_Yoffset
	JSR DeleteCatEntry_YFileOffset
	JMP SaveCatToDisk
}
ENDIF

IF _INCLUDE_CMD_DESTROY_
.CMD_DESTROY
{
	JSR IsEnabledOrGo		; If NO it returns to calling sub
	JSR parameter_afsp_Param_SyntaxErrorIfNull_getcatentry_fspTxtP
.destroyloop1
	LDA MA+&0E0F,Y			; Print list of matching files
	BMI destroylocked1		; IF file locked
	JSR prt_filename_Yoffset
	JSR PrintNewLine
.destroylocked1
	JSR get_cat_nextentry
	BCS destroyloop1
	JSR GoYN			; Confirm Y/N
	BNE Y_rts
	JSR CheckForDiskChange
	JSR get_cat_firstentry80
.destroyloop2
	LDA MA+&0E0F,Y
	BMI destroylocked2		; IF file locked
	JSR DeleteCatEntry_AdjustPtr
.destroylocked2
	JSR get_cat_nextentry
	BCS destroyloop2
	JSR SaveCatToDisk
.msgDELETED
	JSR PrintString
	EQUS "Deleted",13
}
ENDIF

.Y_add8
	INY
.Y_add7
	INY
	INY
	INY
	INY
	INY
	INY
	INY
.Y_rts
	RTS

IF _INCLUDE_CMD_DESTROY_ OR _INCLUDE_CMD_WIPE_
.DeleteCatEntry_AdjustPtr
	JSR DeleteCatEntry_YFileOffset	; Delete cat entry
	LDY &B6
	JSR Y_sub8			; Take account of deletion
	STY &B6				; so ptr is at next file
	RTS
ENDIF

	\\ *DRIVE <drive>
.CMD_DRIVE
	JSR Param_DriveNo_Syntax
	STA DEFAULT_DRIVE
	RTS

.osfileFF_loadfiletoaddr
	JSR getcatentry_fspBA		; Get Load Addr etc.
	JSR SetParamBlockPointerB0	; from catalogue
	JSR ReadFileAttribsToB0_Yoffset	; (Just for info?)

.LoadFile_Ycatoffset
{
	STY &BA
	LDX #&00
	LDA &BE				; If ?BE=0 don't do Load Addr
	BNE loadAtLoadAddr

	\ use load address in control block
	INY
	INY
	LDX #&02
	BNE load_copyfileinfo_loop	; always

	\ use file's load address
.loadAtLoadAddr
	LDA MA+&0F0E,Y 			; mixed byte
	STA &C2
	JSR LoadAddrHi2

.load_copyfileinfo_loop
	LDA MA+&0F08,Y
	STA &BC,X
	INY
	INX
	CPX #&08
	BNE load_copyfileinfo_loop

	JSR ExecAddrHi2

	LDY &BA
	JSR prt_InfoMsg_Yoffset		; pt. print file info
	; Fall into  LoadMemBlockEX
}

	\\ **** Load block of memory ****
.LoadMemBlockEX
IF _SWRAM_
IF _MM32_
	JSR CheckForException
ELSE
	JSR MMC_BEGIN1
	JSR CalcRWVars
	JSR CheckForException
	JMP readblock
ENDIF
ENDIF

.LoadMemBlock
IF _MM32_
	LDA #&85
	BNE exec_block_rw
ELSE
	JSR MMC_BEGIN1
	JSR CalcRWVars
.readblock
	JSR MMC_ReadBlock

.rwblkexit
{
	LDA TubeNoTransferIf0
	BEQ rwblknottube
	JSR TUBE_RELEASE_NoCheck
.rwblknottube
	JSR MMC_END
	LDA #1
	RTS
}
ENDIF

	\\ **** Save block of memory ****
.SaveMemBlock
IF _MM32_
	LDA #&A5
	;BNE exec_block_rw
ELSE
	JSR MMC_BEGIN1
	JSR CalcRWVars
	JSR CheckWriteProtect
;.writeblock
	JSR MMC_WriteBlock
	JMP rwblkexit
ENDIF


IF _MM32_
\\ Block read/write
\\ On entry A=FDC command
.exec_block_rw
{
	\ Populate OSWORD control block
	STA OWCtlBlock+6	; FDC Command
	LDA #5
	STA OWCtlBlock+5	; Param count
	LDA CurrentDrv
	STA OWCtlBlock		; Drive

	\ Buffer address
	LDX #2
.loop0
	LDA &BC-1,X
	STA OWCtlBlock,X
	LDA MA+&1074-1,X
	STA OWCtlBlock+2,X
	DEX
	BNE loop0

	\ Convert sector address to track & sector
	LDA &C2
	AND #3
	TAX
	LDA &C3		; X:A = sector address
	LDY #&FF	; Y = track

.loop1
	SEC

.loop2
	INY
	SBC #10
	BCS loop2

	DEX
	BPL loop1

	;C=0
	ADC #10		; A = sector
	STY OWCtlBlock+7
	STA OWCtlBlock+8

	\ Block size (bytes)
	LDA &C1
	STA OWCtlBlock+9
	LDA &C2		; mixed byte
	LSR A
	LSR A
	LSR A
	LSR A
	AND #3
	STA OWCtlBlock+10
	LDA &C0
	STA OWCtlBlock+11

	JSR OW7F_Execute
	JSR ReportIfDiskFault

	LDA #1
	RTS
}
ENDIF

.osfile0_savememblock
	JSR CreateFile_FSP
	JSR SetParamBlockPointerB0
	JSR ReadFileAttribsToB0_Yoffset
	JMP SaveMemBlock

.fscv2_4_11_starRUN
	JSR SetTextPointerYX		; ** RUN
.NotCmdTable4
{
	JSR SetWordBA_txtptr		; (Y preserved)
	STY MA+&10DA			; Y=0
	JSR read_fspBA_reset		; Look in default drive/dir
	STY MA+&10D9			; Y=text ptr offset
	JSR get_cat_firstentry81
	BCS runfile_found		; If file found
	LDY MA+&10DA
	LDA LIB_DIR			; Look in library
	STA DirectoryParam
	LDA LIB_DRIVE
	JSR SetCurrentDrive_Adrive
	JSR read_fspBA
	JSR get_cat_firstentry81
	BCS runfile_found		; If file found

.errBADCOMMAND
	JSR errBAD
	EQUB &FE
	EQUS "command",0

.runfile_found
	LDA MA+&0F0E,Y			; New to DFS
	JSR A_rorx6and3			; If ExecAddr=&FFFFFFFF *EXEC it
	CMP #&03
	BNE runfile_run			; If ExecAddr<>&FFFFFFFF
	LDA MA+&0F0A,Y
	AND MA+&0F0B,Y
	CMP #&FF
	BNE runfile_run			; If ExecAddr<>&FFFFFFFF
	LDX #&06			; Else *EXEC file  (New to DFS)
.runfile_exec_loop
	LDA MA+&1000,X			; Move filename
	STA MA+&1007,X
	DEX
	BPL runfile_exec_loop
	LDA #&0D
	STA MA+&100E
	LDA #&45
	STA MA+&1000			; "E"
	LDA #&2E			; "."
	STA MA+&1001
	LDA #&3A			; ":"
	STA MA+&1002
	LDA CurrentDrv
	ORA #&30
	STA MA+&1003			; Drive number X
	LDA #&2E			; "."
	STA MA+&1004
	STA MA+&1006
	LDA DirectoryParam		; Directory D
	STA MA+&1005
	LDX #&00			; "E.:X.D.FILENAM"
	LDY #MP+&10
	JMP OSCLI

.runfile_run
	JSR LoadFile_Ycatoffset	; Load file (host|sp)
	CLC
	LDA MA+&10D9			; Word &10D9 += text ptr
	TAY 				; i.e. -> parameters
	ADC TextPointer
	STA MA+&10D9
	LDA TextPointer+1
	ADC #&00
	STA MA+&10DA
	LDA MA+&1076			; Execution address hi bytes
	AND MA+&1077
	ORA TubePresentIf0
	CMP #&FF
	BEQ runfile_inhost		; If in Host
	LDA &BE				; Copy exec add low bytes
	STA MA+&1074
	LDA &BF
	STA MA+&1075
	JSR TUBE_CLAIM
	LDX #&74			; Tell second processor
	\ assume tube code doesn't change sw rom
	LDY #MP+&10
	LDA #&04			; (Exec addr @ 1074)
	JMP TubeCode			; YX=addr,A=0:initrd,A=1:initwr,A=4:strexe
.runfile_inhost
	LDA #&01			; Execute program
	JMP (&00BE)
}

.SetWordBA_txtptr
	LDA #&FF
	STA &BE
	LDA TextPointer
	STA &BA
	LDA TextPointer+1
	STA &BB
	RTS

.CMD_DIR
	LDX #&00			; ** Set DEFAULT DIR/DRV
	BEQ setdirlib

.CMD_LIB
	LDX #&02			; ** Set LIBRARY DIR/DRV
.setdirlib
	JSR ReadDirDrvParameters
	STA DEFAULT_DRIVE,X
	LDA DirectoryParam
	STA DEFAULT_DIR,X
	RTS

	\\ Copy valuable data from static workspace (sws) to
	\\ private workspace (pws)
	\\ (sws data 10C0-10EF, and 1100-11BF)
IF NOT(_SWRAM_)
.SaveStaticToPrivateWorkspace
{
	JSR RememberAXY
IF _DEBUG
	JSR PrintString
	EQUB "Saving workspace", 13
ENDIF

	LDA &B0
	PHA
	LDA &B1
	PHA

	JSR SetPrivateWorkspacePointerB0
	LDY #&00
.stat_loop1
	CPY #&C0
	BCC stat_Y_lessC0
	LDA MA+&1000,Y
	BCS stat_Y_gtreqC0
.stat_Y_lessC0
	LDA MA+&1100,Y
.stat_Y_gtreqC0
	STA (&B0),Y
	INY
IF _MM32_
	CPY #LO(CHECK_CRC7+1)
ELSE
	CPY #&F0
ENDIF
	BNE stat_loop1

	PLA 				; Restore previous values
	STA &B1
	PLA
	STA &B0
	RTS
}
ENDIF


.ReadDirDrvParameters
	LDA DEFAULT_DIR			; Read drive/directory from
	STA DirectoryParam		; command line
	JSR GSINIT_A
	BNE ReadDirDrvParameters2	; If not null string
	LDA #&00
	JMP SetCurrentDrive_Adrive_noand; Drive 0!

.ReadDirDrvParameters2
{
	JSR Set_CurDrv_ToDefault
.rdd_loop
	JSR GSREAD_A
	BCS errBADDIRECTORY		; If end of string
	CMP #&3A			; ":"?
	BNE rdd_exit2
	JSR Param_DriveNo_BadDrive	; Get drive
	JSR GSREAD_A
	BCS rdd_exit1			; If end of string
	CMP #&2E			; "."?
	BEQ rdd_loop
.errBADDIRECTORY
	JSR errBAD
	EQUB &CE
	EQUS "dir",0

.rdd_exit2
	STA DirectoryParam
	JSR GSREAD_A			; Check end of string
	BCC errBADDIRECTORY		; If not end of string

.rdd_exit1
	LDA CurrentDrv
	RTS
}

titlestr%=MA+&1000

IF _INCLUDE_CMD_TITLE_
.CMD_TITLE
{
	JSR Param_SyntaxErrorIfNull
	JSR Set_CurDirDrv_ToDefaults_and_load	; load cat

	LDX #&0B			; blank title
	LDA #&00
.cmdtit_loop1
	JSR SetDiskTitleChr_Xpos
	DEX
	BPL cmdtit_loop1

.cmdtit_loop2
	INX 				; read title for parameter
	JSR GSREAD_A
	BCS cmdtit_savecat
	JSR SetDiskTitleChr_Xpos
	CPX #&0B
	BCC cmdtit_loop2

.cmdtit_savecat
IF _MM32_
	JMP SaveCatToDisk
ELSE
	JSR SaveCatToDisk		; save cat
	JMP UpdateDiskTableTitle	; update disk table
ENDIF

.SetDiskTitleChr_Xpos
IF NOT(_MM32_)
	STA titlestr%,X
ENDIF
	CPX #&08
	BCC setdisttit_page
	STA MA+&0EF8,X
	RTS
.setdisttit_page
	STA MA+&0E00,X
	RTS
}
ENDIF



IF _INCLUDE_CMD_ACCESS_
.CMD_ACCESS
{
	JSR parameter_afsp
	JSR Param_SyntaxErrorIfNull
	JSR read_fspTextPointer
	LDX #&00			; X=locked mask
	JSR GSINIT_A
	BNE cmdac_getparam		; If not null string
.cmdac_flag
	STX &AA
	JSR get_cat_firstentry80
	BCS cmdac_filefound
	JMP err_FILENOTFOUND
.cmdac_filefound
	JSR CheckFileNotOpenY		; Error if it is!
	LDA MA+&0E0F,Y			; Set/Reset locked flag
	AND #&7F
	ORA &AA
	STA MA+&0E0F,Y
	JSR prt_InfoMsg_Yoffset
	JSR get_cat_nextentry
	BCS cmdac_filefound
	\BCC jmp_savecattodisk		; Save catalogue
.jmp_savecattodisk
	JMP SaveCatToDisk

.cmdac_paramloop
	LDX #&80			; Locked bit
.cmdac_getparam
	JSR GSREAD_A
	BCS cmdac_flag			; If end of string
	AND #&5F
	CMP #&4C			; "L"?
	BEQ cmdac_paramloop
.errBADATTRIBUTE
	JSR errBAD			; Bad attribute
	EQUB &CF
	EQUS "attribute",0
}
ENDIF


.fscv0_starOPT
	JSR RememberAXY
	TXA
	CMP #&04
	BEQ SetBootOption_Yoption
	CMP #&05
	BEQ DiskTrapOption
	CMP #&02
	BCC opts0_1			; If A<2
.errBADOPTION
	JSR errBAD
	EQUB &CB
	EQUS "option",0

.opts0_1
	LDX #&FF			; *OPT 0,Y or *OPT 1,Y
	TYA
	BEQ opts0_1_Y0
	LDX #&00
.opts0_1_Y0
	STX FSMessagesOnIfZero		; =NOT(Y=0), I.e. FF=messages off
	RTS

.SetBootOption_Yoption
	TYA 				; *OPT 4,Y
	PHA
	JSR Set_CurDirDrv_ToDefaults
	JSR LoadCurDrvCat		; load cat
	PLA
	JSR A_rolx4
	EOR MA+&0F06
	AND #&30
	EOR MA+&0F06
	STA MA+&0F06
	JMP SaveCatToDisk		; save cat

.DiskTrapOption
{
IF NOT(_MASTER_)			; Master DFS always has higher priority
	\ Bit 6 of the PagedROM_PrivWorkspaces = disable *DISC, *DISK commands etc.
	TYA				; *OPT 5,Y
	PHP
IF _BP12K_
	LDA PagedRomSelector_RAMCopy
	AND #&7F
	TAX
ELSE
	LDX PagedRomSelector_RAMCopy
ENDIF
	LDA PagedROM_PrivWorkspaces,X
	AND #&BF			; Clear bit 6
	PLP
	BEQ skip
	ORA #&40			; Set bit 6 if Y<>0
.skip	STA PagedROM_PrivWorkspaces,X
ENDIF
	RTS
}

.errDISKFULL
	JSR errDISK
	EQUB &C6
	EQUS "full",0

.CreateFile_FSP
{
	JSR read_fspBA_reset
	JSR get_cat_firstentry80	; loads cat/does file exist?
	BCC createfile_nodel		; If NO

	JSR DeleteCatEntry_YFileOffset	; delete previous file

.createfile_nodel
	LDA &C0				; save wC0
	PHA
	LDA &C1
	PHA
	SEC
	LDA &C2				; A=1078/C1/C0=start address
	SBC &C0				; B=107A/C3/C2=end address
	STA &C0				; C=C4/C1/C0=file length
	LDA &C3
	SBC &C1
	STA &C1
	LDA MA+&107A
	SBC MA+&1078
	STA &C4				; C=B-A
	JSR CreateFile_2
	LDA MA+&1079			; Load Address=Start Address
	STA MA+&1075			; (4 bytes)
	LDA MA+&1078
	STA MA+&1074
	PLA
	STA &BD
	PLA
	STA &BC
	RTS
}

.CreateFile_2
{
	LDA #&00			; NB Cat stored in
	STA &C2				; desc start sec order
	LDA #&02			; (file at 002 last)
	STA &C3				; wC2=&200=sector
	LDY FilesX8			; find free block
	CPY #&F8			; big enough
	BCS errCATALOGUEFULL		; for new file
	JSR Getfirstblock_Yoffset
	JMP cfile_cont2

.cfile_loop
	BEQ errDISKFULL
	JSR Y_sub8
	JSR Getnextblock_Yoffset
.cfile_cont2
	TYA
	BCC cfile_loop			; If not big enough
	STY &B0				; Else block found
	LDY FilesX8			; Insert space into catalogue
.cfile_insertfileloop
	CPY &B0
	BEQ cfile_atcatentry		; If at new entry
	LDA MA+&0E07,Y
	STA MA+&0E0F,Y
	LDA MA+&0F07,Y
	STA MA+&0F0F,Y
	DEY
	BCS cfile_insertfileloop
.cfile_atcatentry
	LDX #&00
	JSR CreateMixedByte
.cfile_copyfnloop
	LDA &C5,X			; Copy filename from &C5
	STA MA+&0E08,Y
	INY
	INX
	CPX #&08
	BNE cfile_copyfnloop
.cfile_copyattribsloop
	LDA &BB,X			; Copy attributes
	DEY
	STA MA+&0F08,Y
	DEX
	BNE cfile_copyattribsloop
	JSR prt_InfoMsg_Yoffset
	TYA
	PHA
	LDY FilesX8
	JSR Y_add8
	STY FilesX8			; FilesX+=8
	JSR SaveCatToDisk		; save cat
	PLA
	TAY
	RTS
}

.errCATALOGUEFULL
	JSR ReportErrorCB
	EQUB &BE
	EQUS "Cat full",0

.CreateMixedByte
	LDA MA+&1076			; Exec address b17,b16
	AND #&03
	ASL A
	ASL A
	EOR &C4				; Length
	AND #&FC
	EOR &C4
	ASL A
	ASL A
	EOR MA+&1074			; Load address
	AND #&FC
	EOR MA+&1074
	ASL A
	ASL A
	EOR &C2				; Sector
	AND #&FC
	EOR &C2
	STA &C2				; C2=mixed byte
	RTS

IF _INCLUDE_CMD_ENABLE_
.CMD_ENABLE
	LDA #&01
	STA CMDEnabledIf1
	RTS
ENDIF


.LoadAddrHi2
	LDA #&00
	STA MA+&1075
	LDA &C2
	AND #&08
	STA MA+&1074
	BEQ ldadd_nothost
.SetLoadAddrToHost
	LDA #&FF
	STA MA+&1075
	STA MA+&1074
.ldadd_nothost
	RTS


.ExecAddrHi2
{
	LDA #&00
	STA MA+&1077
	LDA &C2
	JSR A_rorx6and3
	CMP #&03
	BNE exadd_nothost
	LDA #&FF
	STA MA+&1077
.exadd_nothost
	STA MA+&1076
	RTS
}

.Set_CurDirDrv_ToDefaults
	LDA DEFAULT_DIR			; set working dir
	STA DirectoryParam

.Set_CurDrv_ToDefault
	LDA DEFAULT_DRIVE		; set working drive
.SetCurrentDrive_Adrive
	AND #&03
.SetCurrentDrive_Adrive_noand
	STA CurrentDrv
	RTS
	\ (<drive>)
.Param_OptionalDriveNo
	JSR GSINIT_A
	BEQ Set_CurDrv_ToDefault	; null string

	\ <drive>
	\ Exit: A=DrvNo, C=0, XY preserved
.Param_DriveNo_Syntax
	JSR Param_SyntaxErrorIfNull
.Param_DriveNo_BadDrive
	JSR GSREAD_A
	BCS errBADDRIVE
	CMP #':' 			; ASC(":")
	BEQ Param_DriveNo_BadDrive
	SEC
	SBC #&30
	BCC errBADDRIVE
	CMP #&04
	\ exit with C=0
	BCC SetCurrentDrive_Adrive_noand

.errBADDRIVE
	JSR errBAD
	EQUB &CD
	EQUS "drive",0


IF NOT(_MM32_)
.jmpSYNTAX
	JMP errSYNTAX


.errDISKNOTFOUND
	JSR errDISK
	EQUB 214
	EQUS "not found",0


	\\ Read parameters : drive optional
	\\ (<drive>) <dno>/<dsp>
	\\ Exit: CurrentDrv=drive, Word &B8=disk no.

.Param_DriveAndDisk
{
	JSR Param_SyntaxErrorIfNull
	CMP #&22
	BNE param_nq1
	DEY

.param_nq1
	STY &B4				 ; Save Y
	JSR GSREAD_A
	CMP #':'			; ASC(":")
	BNE param_dad
	JSR Param_DriveNo_BadDrive
	BCC Param_Disk			; always

.param_dad
	LDY &B4				; Restore Y
	JSR Set_CurDrv_ToDefault

	\ Read 1st number
	JSR Param_ReadNum		; rn% @ B0
	BCS gddfind			; if not valid number
	JSR GSINIT_A
	BEQ gdnodrv			; if rest of line blank
	CMP #&22
	BNE param_nq2
	DEY

.param_nq2
	LDA rn%+1
	BNE errBADDRIVE			; AX>255
	LDA rn%
	CMP #4
	BCS errBADDRIVE
	STA CurrentDrv
}


	\ Read (2nd) number?
	\ If it's not a valid number:
	\ assume it's a disk name
	\ <dno>/<dsp>
	\ Exit: Word &B8 = disk no.
.Param_Disk
	JSR Param_ReadNum		; rn% @ B0
	BCS gddfind			; if not valid number
	JSR GSINIT_A
	BNE jmpSYNTAX			; if rest of line not blank

.gdnodrv
	LDA rn%+1
	STA &B9
	LDA rn%
	STA &B8
.gddfound
	RTS

	\ The parameter is not valid number;
	\ so it must be a disk name?!
gdptr%=&B0
gdsec%=&B2
gddiskno%=&B8
gdopt%=&B7

.gddfind
{
	JSR DMatchInit
	LDX #0			; don't return unformatted disks
	JSR GetDiskFirstAllX
	LDA dmLen%
	BEQ jmpSYNTAX
	LDA dmAmbig%
	BNE jmpSYNTAX

.gddlp
	LDA gddiskno%+1
	BMI errDISKNOTFOUND
	JSR DMatch
	BCC gddfound
	JSR GetDiskNext
	JMP gddlp
}
ENDIF

IF _INCLUDE_CMD_RENAME_
.CMD_RENAME
{
	JSR parameter_fsp
	JSR Param_SyntaxErrorIfNull
	JSR read_fspTextPointer
	TYA
	PHA
	JSR getcatentry
	JSR CheckFileNotLockedOrOpenY
	STY &C4
	PLA
	TAY
	JSR Param_SyntaxErrorIfNull
	LDA CurrentDrv
	PHA
	JSR read_fspTextPointer
	PLA
	CMP CurrentDrv
	BNE jmpBADDRIVE
	JSR get_cat_firstentry80
	BCC rname_ok
	CPY &C4
	BEQ rname_ok
.errFILEEXISTS
	JSR ReportErrorCB
	EQUB &C4
	EQUS "Exists",0
.jmpBADDRIVE
	JMP errBADDRIVE
.rname_ok
	LDY &C4				; Copy filename
	JSR Y_add8			; from C5 to catalog
	LDX #&07
.rname_loop
	LDA &C5,X
	STA MA+&0E07,Y
	DEY
	DEX
	BPL rname_loop			; else Save catalogue
	JMP SaveCatToDisk
}
ENDIF

.TUBE_CheckIfPresent
	LDA #&EA			; Tube present?
	LDX #&00			; X=FF if Tube present
	LDY #&FF
	JSR OSBYTE
	TXA
	EOR #&FF
	STA TubePresentIf0
	RTS

.TUBE_CLAIM
{
	PHA
.tclaim_loop
	LDA #&C0+tubeid%
	JSR TubeCode
	BCC tclaim_loop
	PLA
	RTS
}

.TUBE_RELEASE
	JSR TUBE_CheckIfPresent
	BMI trelease_exit
.TUBE_RELEASE_NoCheck
	PHA
	LDA #&80+tubeid%
	JSR TubeCode
	PLA
.trelease_exit
	RTS

.CheckESCAPE
	BIT &FF				; Check if ESCAPE presed
	BPL noesc			; Used by *FORM/VERIFY
.ReportESCAPE
	JSR osbyte7E_ackESCAPE2
	JSR ReportError
	EQUB &11
	EQUS "Escape",0

.PrintHex100
;{
	PHA 				; Print hex to &100+Y
	JSR A_rorx4
	JSR PrintNib100
	PLA

.PrintNib100
	JSR NibToASC
IF _MM32_
	STA &0100,X
	INX
ELSE
	STA &0100,Y
	INY
ENDIF
;}
.noesc
	RTS


.BootOptions
	EQUS "L.!BOOT",13
	EQUS "E.!BOOT",13

.AUTOBOOT
IF _MM32_
	LDA &B3				; ?&B3=value of Y on call 3
	JSR PrintString
	BOOT_NAME
	BCC initMMFS
ELSE
	\\ Code space optimization
	\\ e.g. for long boot names like:
	\\ Model B MMFS SWRAM (FE80) Turbo L
 	LDX #0
 .bootprloop
 	LDA title, X
 	BEQ bootprexit
 	JSR OSWRCH
 	INX
 	BNE bootprloop
 .bootprexit
 	JSR OSNEWL
 	JSR OSNEWL
 	LDA &B3				; ?&B3=value of Y on call 3
 	JMP initMMFS
ENDIF

.CMD_DISC
IF NOT(_MASTER_)
IF _BP12K_
	LDA PagedRomSelector_RAMCopy
	AND #&7F
	TAX
ELSE
	LDX PagedRomSelector_RAMCopy	; Are *DISC,*DISK disabled?
ENDIF
	LDA PagedROM_PrivWorkspaces,X
	AND #&40
	BEQ CMD_CARD
	RTS
ENDIF

.CMD_CARD
	LDA #&FF
.initMMFS
{
	JSR ReturnWithA0		; On entry: if A=0 then boot file
	PHA
IF _DEBUG
	JSR PrintString
	EQUB "Starting MMFS", 13
ENDIF
	LDA #&06
	JSR Go_FSCV			; new filing system

	\ Copy vectors/extended vectors
	LDX #&0D			; copy vectors
.vectloop
	LDA vectors_table,X
	STA &0212,X
	DEX
	BPL vectloop
	LDA #&A8			; copy extended vectors
	JSR osbyte_X0YFF
	STY &B1
	STX &B0
	LDX #&07
	LDY #&1B
.extendedvec_loop
	LDA extendedvectors_table-&1B,Y
	STA (&B0),Y
	INY
	LDA extendedvectors_table-&1B,Y
	STA (&B0),Y
	INY
	LDA PagedRomSelector_RAMCopy
IF _BP12K_
	ORA #&80
ENDIF
	STA (&B0),Y
	INY
	DEX
	BNE extendedvec_loop

	STY CurrentCat			; curdrvcat<>0
	STY MA+&1083			; ?
	STX CurrentDrv			; curdrv=0
	STX MMC_STATE			; Uninitialised

	LDX #&0F			; vectors claimed!
	JSR osbyte8F_servreq

	\\ If soft break and pws "full" and not booting a disk
	\\ then copy pws to sws
	\\ else reset fs to defaults.

IF _SWRAM_
	LDA ForceReset
	BMI initdfs_noreset
	LDA #&FF					; Now clear the force reset flag
	STA ForceReset				; so the reset only happens once
ELSE
	JSR SetPrivateWorkspacePointerB0
	LDY #<ForceReset
	LDA (&B0),Y			; A=PWSP+&D3 (-ve=soft break)
	BPL initdfs_reset		; Branch if power up or hard break

	\PLA
	\PHA
	\BEQ initdfs_reset		; Branch if boot file

	LDY #&D4
	LDA (&B0),Y			; A=PWSP+&D4
	BMI initdfs_noreset		; Branch if PWSP "empty"

	JSR ClaimStaticWorkspace

IF _DEBUG
	JSR PrintString
	EQUB "Restoring workspace", 13
ENDIF
	LDY #&00			; ** Restore copy of data
.copyfromPWStoSWS_loop
	LDA (&B0),Y			; from private wsp
	CPY #&C0			; to static wsp
	BCC copyfromPWS1
	STA MA+&1000,Y
	BCS copyfromPWS2
.copyfromPWS1
	STA MA+&1100,Y
.copyfromPWS2
	DEY
	BNE copyfromPWStoSWS_loop

	\\ Check VID CRC and if wrong reset filing system
	JSR CalculateCRC7
	CMP CHECK_CRC7
	BNE setdefaults

	LDA #&A0			; Refresh channel block info
.setchansloop
	TAY
	PHA
	LDA #&3F
	JSR ChannelFlags_ClearBits	; Clear bits 7 & 6, C=0
	PLA
	STA MA+&111D,Y			; Buffer sector hi?
	SBC #&1F			; A=A-&1F-(1-C)=A-&20
	BNE setchansloop
	BEQ initdfs_noreset		; always

	\ Initialise SWS (Static Workspace)

.initdfs_reset
	JSR ClaimStaticWorkspace
ENDIF

	\ Set to defaults

.setdefaults
IF _DEBUG
	JSR PrintString
	EQUB "Setting MMFS defaults", 13
	NOP
ENDIF
	JSR FSDefaults

	\ INITIALISE VID VARIABLES
	\ Don't reset if booting
	\PLA
	\PHA
	\BEQ initdfs_noreset

	JSR VIDRESET

.initdfs_noreset
	JSR TUBE_CheckIfPresent		; Tube present?

IF _MM32_
	LDA #&FD				; Read hard/soft break
	JSR osbyte_X0YFF		; X=0=soft,1=power up,2=hard
	CPX #0
	BEQ skipautoload
	JSR MMC_BEGIN2
	JSR mm32_cmd_autoload
.skipautoload
ENDIF

	PLA
	BNE initdfs_exit		; branch if not boot file

	JSR LoadCurDrvCat
	LDA MA+&0F06			; Get boot option
	JSR A_rorx4
	BNE notOPT0				; branch if not opt.0

.initdfs_exit
	RTS

	\ Assumes cmd strings all in same page!
.notOPT0
	LDY #HI(BootOptions)		; boot file?
	LDX #LO(BootOptions)		; ->L.!BOOT
	CMP #&02
	BCC jmpOSCLI			; branch if opt 1
	BEQ oscliOPT2			; branch if opt 2
	IF HI(BootOptions+8)<>HI(BootOptions)
		LDY #HI(BootOptions+8)
	ENDIF
	LDX #LO(BootOptions+8)		; ->E.!BOOT
	BNE jmpOSCLI			; always
.oscliOPT2
	IF HI(BootOptions+10)<>HI(BootOptions)
		LDY #HI(BootOptions+10)
	ENDIF
	LDX #LO(BootOptions+10)		; ->!BOOT
.jmpOSCLI
	JMP OSCLI
}

.FSDefaults
{
IF _MM32_
	LDA #' '			; Reset the *DDRIVE table (MMFS2)
	STA MA+&11C0
	STA MA+&11D0
ENDIF
	LDA #'$'
	STA DEFAULT_DIR
	STA LIB_DIR
	LDA #0
	STA LIB_DRIVE
	LDY #&00
	STY DEFAULT_DRIVE
	STY MA+&10C0

	DEY				; Y=&FF
	STY CMDEnabledIf1
	STY FSMessagesOnIfZero
	STY MA+&10DD
	RTS
}

.VIDRESET				; Reset VID
{
IF _MM32_
	LDY #(CHECK_CRC7-VID-1)
ELSE
	\\ TODO: Don't need to clear the last byte
	LDY #(CHECK_CRC7-VID)
ENDIF
	LDA #0
.loop
IF _MM32_
	STA VID,Y
ELSE
	\\ TODO: this case is really the same as the MM32 one
	STA DRIVE_INDEX0,Y
ENDIF
	DEY
	BPL loop
	LDA #1
	STA CHECK_CRC7
	RTS
}

IF _DEBUG
.PrintAXY
	PHA
	JSR PrintString
	EQUB "A="
	NOP
	JSR PrintHex
	JSR PrintString
	EQUB ";X="
	NOP
	TXA
	JSR PrintHex
	JSR PrintString
	EQUB ";Y="
	NOP
	TYA
	JSR PrintHex
	JSR PrintString
	EQUB 13
	NOP
	PLA
	RTS
ENDIF
.MMFS_SERVICECALLS
{
IF _DEBUG
	PHA
	JSR PrintString
	EQUB "Service "
	NOP
	JSR PrintAXY
	PLA
ENDIF
IF _TUBEHOST_
	JSR SERVICE09_TUBEHelp		; Tube service calls
ENDIF

IF _MASTER_
	BIT PagedROM_PrivWorkspaces,X	; ROM disabled if 01xxxxxx or 10xxxxxx
	BPL lbl2			; if 0x
	BVS lbl3			; if 11
.lbl1	RTS
.lbl2	BVS lbl1			; if 01
.lbl3	\ Note: 00 = PWS in normal ram, 11 = PWS in hidden ram
ELSE
	PHA
IF _BP12K_
	LDA PagedRomSelector_RAMCopy
	AND #&7F
	TAX
ENDIF
	LDA PagedROM_PrivWorkspaces,X
	BMI romdisabled			; if bit 7 set
.lbl1	PLA
ENDIF

	CMP #&12
	BEQ SERVICE12_init_filesystem
	CMP #&0B
IF _MASTER_
	BCC label4
	CMP #&28
	BCS SERVICE_NULL
	CMP #&21
	BCC SERVICE_NULL
	SBC #&16
ELSE
	BCS SERVICE_NULL
ENDIF
.label4
	ASL A
	TAX
	LDA data+1,X
	PHA
	LDA data,X
	PHA

	\ Restore A & X values
IF _BP12K_
	TXA
	PHA
	LDA PagedRomSelector_RAMCopy
	AND #&7F
	TAX
	PLA
ELSE
	TXA
	LDX PagedRomSelector_RAMCopy
ENDIF
	LSR A
	CMP #&0B
	BCC label3
	ADC #&15
.label3

.SERVICE_NULL
	RTS

.romdisabled
	PLA
	RTS

.data	EQUW SERVICE_NULL-1		; 0
IF _MASTER_ OR _SWRAM_
	EQUW SERVICE_NULL-1		; 1 Use 21 instead for MASTER
ELSE
	EQUW SERVICE01_claim_absworkspace-1	; 1
ENDIF
	EQUW SERVICE02_claim_privworkspace-1	; 2
	EQUW SERVICE03_autoboot-1	; 3
	EQUW SERVICE04_unrec_command-1	; 4
	EQUW SERVICE_NULL-1		; 5
	EQUW SERVICE_NULL-1		; 6
	EQUW SERVICE_NULL-1		; 7
	EQUW SERVICE08_unrec_OSWORD-1	; 8
	EQUW SERVICE09_help-1		; 9
IF _SWRAM_
	EQUW SERVICE_NULL-1		; A
ELSE
	EQUW SERVICE0A_claim_statworkspace-1	; A
ENDIF
IF _MASTER_
	EQUW SERVICE21_ClaimHiddenSWS-1	; 21
	EQUW SERVICE22_ClaimHiddenPWS-1	; 22
	EQUW SERVICE_NULL-1		; 23
	EQUW SERVICE24_RequiredPWS-1	; 24
	EQUW SERVICE25_fs_info-1	; 25
	EQUW SERVICE_NULL-1		; 26
	EQUW SERVICE27_Reset-1		; 27

ENDIF


.SERVICE12_init_filesystem		; A=&12 Initialise filing system
	BP12K_NEST
	CPY #filesysno%			; Y=ID no. (4=dfs etc.)
	BNE label3
	JSR RememberAXY
	JMP CMD_CARD
}

IF NOT(_MASTER_) AND NOT(_SWRAM_)
.SERVICE01_claim_absworkspace		; A=1 Claim absolute workspace
{
	CPY #&17			; Y=current upper limit
	BCS exit			; already >=&17
	LDY #&17			; Up upper limit to &17
.exit
	RTS
}
ENDIF

.SERVICE02_claim_privworkspace		; A=2 Claim private workspace, Y=First available page
{
IF _MASTER_
	LDA PagedROM_PrivWorkspaces,X	; If A>=&DC Hidden ram is full so claim PWS in normal ram
	CMP #&DC
	BCC cont
	TYA
	STA PagedROM_PrivWorkspaces,X
.cont
	PHY				; A=PWS page
ELSE
	TYA
	PHA				; Save Y=PWS Page
ENDIF

IF _BP12K_
	JSR Init12K
ENDIF

IF NOT(_SWRAM_)
	STA &B1				; Set (B0) as pointer to PWSP
	LDY PagedROM_PrivWorkspaces,X
	IF NOT(_MASTER_)		; Preserve bit 6
		TYA
		AND #&40
		ORA &B1
	ENDIF
	STA PagedROM_PrivWorkspaces,X
	LDA #&00
	STA &B0
	CPY &B1				; Private workspace may have moved!
	BEQ samepage			; If same as before

	LDY #<ForceReset
	STA (&B0),Y			; PWSP?&D3=0
.samepage
ENDIF

	LDA #&FD			; Read hard/soft BREAK
	JSR osbyte_X0YFF		; X=0=soft,1=power up,2=hard
	DEX

IF _SWRAM_
IF _BP12K_
        \\ Don't allow soft resets to update ForceReset; this can cause resets
        \\ pending from previous power up/hard resets to be lost.
        BMI skipOnSoftReset
	JSR PageIn12K
ENDIF
	STX ForceReset
IF _BP12K_
	JSR PageOut12K
.skipOnSoftReset
ENDIF
ELSE
	TXA				; A= FF=soft,0=power up,1=hard
	LDY #<ForceReset
	AND (&B0),Y
	STA (&B0),Y			; So, PWSP?&D3 is +ve if:
	PHP				; power up, hard reset or PSWP page has changed
	INY
	PLP
	BPL notsoft			; If not soft break

	LDA (&B0),Y			; A=PWSP?&D4
	BPL notsoft			; If PWSP "full"

	\\ If soft break and pws is empty then I must have owned sws,
	\\ so copy it to my pws.
	JSR SaveStaticToPrivateWorkspace	; Copy valuable data to PWSP

.notsoft
	LDA #&00
	STA (&B0),Y			; PWSP?&D4=0 = PWSP "full"
ENDIF

IF _BP12K_
	LDA PagedRomSelector_RAMCopy
	AND #&7F
	TAX
ELSE
	LDX PagedRomSelector_RAMCopy 	; restore X & A, Y=Y+2
ENDIF
	PLA
	TAY
	LDA #&02

IF _MASTER_
	BIT PagedROM_PrivWorkspaces,X
	BMI srv3_exit			; PWS in hidden ram
ENDIF

IF NOT(_SWRAM_)
	INY 				; taken 1 or 2 pages for pwsp
	IF _UTILS_
		INY			; Utilities need a page too
	ENDIF
ENDIF
}
.srv3_exit
	RTS

.SERVICE03_autoboot			; A=3 Autoboot
{
	BP12K_NEST
	JSR RememberAXY
	STY &B3				; if Y=0 then !BOOT
	LDA #&7A			; Keyboard scan
	JSR OSBYTE			; X=int.key.no
	TXA
	BMI jmpAUTOBOOT
	CMP #&65			; "M" KEY
	BNE srv3_exit
	LDA #&78			; write current keys pressed info
	JSR OSBYTE
.jmpAUTOBOOT
	JMP AUTOBOOT
}

.SERVICE04_unrec_command		; A=4 Unrec Command
	BP12K_NEST
	JSR RememberAXY
	LDX #cmdtab22			; UTILS commands
.jmpunreccmd
	JMP UnrecCommandTextPointer
.NotCmdTable22
IF NOT(_MASTER_)
	IF _UTILS_ OR _ROMS_
		LDX #cmdtab2
		BNE jmpunreccmd
	ELSE
		RTS
	ENDIF
ELSE
	RTS
ENDIF

.SERVICE08_unrec_OSWORD
{
	BP12K_NEST
	JSR RememberAXY

	LDY &EF				; Y = Osword call
	BMI exit			; Y > &7F
	CPY #&7D
	BCC exit			; Y < &7D

	JSR FSisMMFS			; MMFS current fs?
	BNE exit

	JSR ReturnWithA0

	LDX &F0				; Osword X reg
	STX &B0
	LDX &F1				; Osword Y reg
	STX &B1

	LDY &EF
	INY
	BPL notOSWORD7F

	PHP
	CLI
	JSR Osword7F_8271_Emulation	; OSWORD &7F 8271 emulation
	PLP
	RTS

.notOSWORD7F
	JSR Set_CurDirDrv_ToDefaults_and_load		; Load catalogue
	INY
	BMI OSWORD7E

	LDY #&00			; OSWORD &7D return cycle no.
	LDA MA+&0F04
	STA (&B0),Y
.exit	RTS

.OSWORD7E
	LDA #&00			; OSWORD &7E
	TAY
	STA (&B0),Y
	INY
	LDA MA+&0F07			; sector count LB
	STA (&B0),Y
	INY
	LDA MA+&0F06			; sector count HB
	AND #&03
	STA (&B0),Y
	INY
	LDA #&00			; result
	STA (&B0),Y
	RTS
}


.SERVICE09_help				; A=9 *HELP
{
	JSR RememberAXY
	LDA (TextPointer),Y
	LDX #cmdtab3
	CMP #&0D
	BNE jmpunreccmd
	TYA
	LDY #cmdtab3size
	JMP Prthelp_Xtable
}

IF NOT(_SWRAM_)
.SERVICE0A_claim_statworkspace		; A=&A Claim Static Workspace
{
	\\ Another ROM wants the absolute workspace

	JSR RememberAXY

	\ Do I own sws?
	JSR SetPrivateWorkspacePointerB0
	LDY #&D4
	LDA (&B0),Y
	BPL exit			; If pws "full" then sws is not mine

	LDY #&00
	JSR ChannelBufferToDisk_Yhandle
	JSR SaveStaticToPrivateWorkspace	; copy valuable data to private wsp

	JSR SetPrivateWorkspacePointerB0	; Called again?
	LDY #&D4
	LDA #&00			; PWSP?&D4=0 = PWSP "full"
	STA (&B0),Y

	TSX 				; RememberAXY called earlier
	STA &0105,X			; changes value of A in stack to 0
.exit
	RTS
}
ENDIF

IF _MASTER_

.SERVICE21_ClaimHiddenSWS
{
	CPY #&CA
	BCS ok
	LDY #&CA
.ok	RTS
}

.SERVICE22_ClaimHiddenPWS
	TYA
	STA PagedROM_PrivWorkspaces,X
	LDA #&22
	INY
	RTS

.SERVICE24_RequiredPWS
	DEY
	RTS

.SERVICE25_fs_info
{
	LDX #&A
.srv25_loop
	LDA fsinfo,X
	STA (TextPointer),Y
	INY
	DEX
	BPL srv25_loop

	LDA #&25
	LDX PagedRomSelector_RAMCopy
	RTS

.fsinfo
	EQUB filesysno%
	EQUB filehndl%+5
	EQUB filehndl%+1
	EQUS "    SFMM"
}

.SERVICE27_Reset
{
	PHA
	TXA
	PHA
	TYA
	PHA
	LDA #&FD
	LDX #&00
	LDY #&FF
	JSR OSBYTE
	CPX #$00
	BEQ srv27_softbreak
	\ If this is not done, you get a Bad Sum error with autoboot on power on
	JSR VIDRESET
.srv27_softbreak
	PLA
	TAY
	PLA
	TAX
	PLA
	RTS
}
ENDIF	; End of MASTER ONLY service calls

	\ Test if MMFS by checking OSFILE vector.
.FSisMMFS
{
	LDA &213			; Check of the low OSFILE vector is pointing
	CMP #&FF            ; to the corresponding extended vector.
	BNE notMMFS
	LDA &212
	CMP #&1B
	BNE notMMFS
	LDA &0DBC			; Rom number in extended vector.
	CMP &F4				; Is it our ROM?
.notMMFS
	RTS
}


.FILEV_ENTRY
{
	JSR RememberXYonly
	PHA
	JSR parameter_fsp

	STX &B0				; XY -> parameter block
	STX MA+&10DB
	STY &B1
	STY MA+&10DC

	LDX #&00			; BA->filename
	LDY #&00			; BC & 1074=load addr (32 bit)
	JSR CopyWordB0BA		; BE & 1076=exec addr
.filev_copyparams_loop
	JSR CopyVarsB0BA		; C0 & 1078=start addr
	CPY #&12			; C2 & 107A=end addr
	BNE filev_copyparams_loop	; (lo word in zp, hi in page 10)

	PLA
	TAX
	INX
	CPX #&08			; NB A=FF -> X=0
	BCS filev_unknownop		; IF x>=8 (a>=7)
	LDA finv_tablehi,X		; get addr from table
	PHA 				; and "return" to it
	LDA finv_tablelo,X
	PHA
}
.filev_unknownop
	LDA #&00
	RTS

.FSCV_ENTRY
	CMP #&0C
	BCS filev_unknownop
	STX &B5				; Save X
IF _DEBUG
	JSR PrintString
	EQUB "FSCV "
	NOP
	JSR PrintAXY
ENDIF
	TAX
	LDA fscv_table2,X
	PHA
	LDA fscv_table1,X
	PHA
	TXA
	LDX &B5				; Restore X
.gbpbv_unrecop
	RTS

.GBPBV_ENTRY
{
	CMP #&09
	BCS gbpbv_unrecop
	JSR RememberAXY
	JSR ReturnWithA0
	STX MA+&107D
	STY MA+&107E
	TAY
	JSR gbpb_gosub
	PHP
	BIT MA+&1081
	BPL gbpb_nottube
	JSR TUBE_RELEASE_NoCheck
.gbpb_nottube
	PLP
	RTS
}

.gbpb_gosub
{
	LDA gbpbv_table1,Y
	STA MA+&10D7
	LDA gbpbv_table2,Y
	STA MA+&10D8
	LDA gbpbv_table3,Y		; 3 bit flags: bit 2=tube op
	LSR A
	PHP 				; Save bit 0 (0=write new seq ptr)
	LSR A
	PHP 				; Save bit 1 (1=read/write seq ptr)
	STA MA+&107F			; Save Tube operation
	JSR gbpb_wordB4_word107D	; (B4) -> param blk
	LDY #&0C
.gbpb_ctlblk_loop
	LDA (&B4),Y			; Copy param blk to 1060
	STA MA+&1060,Y
	DEY
	BPL gbpb_ctlblk_loop
	LDA MA+&1063			; Data ptr bytes 3 & 4
	AND MA+&1064
	ORA TubePresentIf0
	CLC
	ADC #&01
	BEQ gbpb_nottube1		; If not tube
	JSR TUBE_CLAIM
	CLC
	LDA #&FF
.gbpb_nottube1
	STA MA+&1081			; GBPB to TUBE IF >=&80
	LDA MA+&107F			; Tube op: 0 or 1
	BCS gbpb_nottube2		; If not tube
	LDX #&61
	LDY #MP+&10
	JSR TubeCode 			; (YX=addr,A=0:initrd,A=1:initwr,A=4:strexe) ; Init TUBE addr @ 1061
.gbpb_nottube2
	PLP 				; Bit 1
	BCS gbpb_rw_seqptr
	PLP 				; Bit 0, here always 0
}
.gbpb_jmpsub
	JMP (MA+&10D7)

.gbpb_rw_seqptr
{
	LDX #&03			; GBPB 1,2,3 or 4
.gbpb_seqptr_loop1
	LDA MA+&1069,X			; !B6=ctl blk seq ptr
	STA &B6,X
	DEX
	BPL gbpb_seqptr_loop1		; on exit A=file handle=?&1060
	LDX #&B6
	LDY MA+&1060
	LDA #&00
	PLP				; bit 0
	BCS gpbp_dontwriteseqptr
	JSR argsv_WriteSeqPointer	; If GBPB 1 & 3
.gpbp_dontwriteseqptr
	JSR argsv_rdseqptr_or_filelen	; read seq ptr to &B6
	LDX #&03
.gbpb_seqptr_loop2
	LDA &B6,X			; ctl blk seq prt = !B6
	STA MA+&1069,X
	DEX
	BPL gbpb_seqptr_loop2
}

.gbpb_rwdata
{
	JSR gbpb_bytesxferinvert	; Returns with N=1
	BMI gbpb_data_loopin		; always
.gbpb_data_loop
	LDY MA+&1060			; Y=file handle
	JSR gbpb_jmpsub			; *** Get/Put BYTE
	BCS gbpb_data_loopout		; If a problem occurred
	LDX #&09
	JSR gbpb_incdblword1060X	; inc. seq ptr
.gbpb_data_loopin
	LDX #&05
	JSR gbpb_incdblword1060X	; inc. bytes to txf
	BNE gbpb_data_loop
	CLC
.gbpb_data_loopout
	PHP
	JSR gbpb_bytesxferinvert	; bytes to txf XOR &FFFFFFFF
	LDX #&05
	JSR gbpb_incdblword1060X	; inc. bytes to txf
	LDY #&0C	 		; Copy parameter back
	JSR gbpb_wordB4_word107D	; (B4) -> param blk
.gbpb_restorectlblk_loop
	LDA MA+&1060,Y
	STA (&B4),Y
	DEY
	BPL gbpb_restorectlblk_loop
	PLP 				; C=1=txf not completed
	RTS 				; **** END GBPB 1-4
}

	\\ READ FILENAMES IN CURRENT CAT
.gbpb8_rdfilescurdir
	JSR Set_CurDirDrv_ToDefaults	; GBPB 8
	JSR CheckCurDrvCat
	LDA #LO(gbpb8_getbyte)
	STA MA+&10D7
	LDA #HI(gbpb8_getbyte)
	STA MA+&10D8
	BNE gbpb_rwdata			; always

.gbpb8_getbyte
{
	LDY MA+&1069			; GBPB 8 - Get Byte
.gbpb8_loop
	CPY FilesX8
	BCS gbpb8_endofcat		; If end of catalogue, C=1
	LDA MA+&0E0F,Y			; Directory
	JSR IsAlphaChar
	EOR DirectoryParam
	BCS gbpb8_notalpha
	AND #&DF
.gbpb8_notalpha
	AND #&7F
	BEQ gbpb8_filefound		; If in current dir
	JSR Y_add8
	BNE gbpb8_loop			; next file
.gbpb8_filefound
	LDA #&07			; Length of filename
	JSR gbpb_gb_SAVEBYTE
	STA &B0				; loop counter
.gbpb8_copyfn_loop
	LDA MA+&0E08,Y			; Copy fn
	JSR gbpb_gb_SAVEBYTE
	INY
	DEC &B0
	BNE gbpb8_copyfn_loop
	CLC 				; C=0=more to follow
.gbpb8_endofcat
	STY MA+&1069			; Save offset (seq ptr)
	LDA MA+&0F04
	STA MA+&1060			; Cycle number (file handle)
	RTS 				; **** END GBPB 8
}


	\\ GET MEDIA TITLE
.gbpb5_getmediatitle
{
	JSR Set_CurDirDrv_ToDefaults	; GBPB 5
	JSR CheckCurDrvCat
	LDA #&0C			; Length of title
	JSR gbpb_gb_SAVEBYTE
	LDY #&00
.gbpb5_titleloop
	CPY #&08			; Title
	BCS gbpb5_titlehi
	LDA MA+&0E00,Y
	BCC gbpb5_titlelo
.gbpb5_titlehi
	LDA MA+&0EF8,Y
.gbpb5_titlelo
	JSR gbpb_gb_SAVEBYTE
	INY
	CPY #&0C
	BNE gbpb5_titleloop
	LDA MA+&0F06			; Boot up option
	JSR A_rorx4
	JSR gbpb_gb_SAVEBYTE
	LDA CurrentDrv			; Current drive
	JMP gbpb_gb_SAVEBYTE
}

	\\ READ CUR DRIVE/DIR
.gbpb6_rdcurdirdevice
	JSR gbpb_SAVE_01		; GBPB 6
	LDA DEFAULT_DRIVE		; Length of dev.name=1
	ORA #&30			; Drive no. to ascii
	JSR gbpb_gb_SAVEBYTE
	JSR gbpb_SAVE_01		; Lendgh of dir.name=1
	LDA DEFAULT_DIR			; Directory
	BNE gbpb_gb_SAVEBYTE

	\\ READ LIB DRIVE/DIR
.gbpb7_rdcurlibdevice
	JSR gbpb_SAVE_01		; GBPB 7
	LDA LIB_DRIVE			; Length of dev.name=1
	ORA #&30			; Drive no. to ascii
	JSR gbpb_gb_SAVEBYTE
	JSR gbpb_SAVE_01		; Lendgh of dir.name=1
	LDA LIB_DIR			; Directory
	BNE gbpb_gb_SAVEBYTE

.gpbp_B8memptr
	PHA	 			; Set word &B8 to
	LDA MA+&1061			; ctl blk mem ptr (host)
	STA &B8
	LDA MA+&1062
	STA &B9
	LDX #&00
	PLA
	RTS

.gbpb_bytesxferinvert
{
	LDX #&03			; Bytes to tranfer XOR &FFFF
.gbpb_bytesxferinvert_loop
	LDA #&FF
	EOR MA+&1065,X
	STA MA+&1065,X
	DEX
	BPL gbpb_bytesxferinvert_loop
	RTS
}

.gbpb_wordB4_word107D
	LDA MA+&107D
	STA &B4
	LDA MA+&107E
	STA &B5
.gpbp_exit
	RTS

.gbpb_SAVE_01
	LDA #&01
	BNE gbpb_gb_SAVEBYTE		; always
.gbpb_getbyteSAVEBYTE
	JSR BGETV_ENTRY
	BCS gpbp_exit			; If EOF
.gbpb_gb_SAVEBYTE
	BIT MA+&1081
	BPL gBpb_gb_fromhost
	STA TUBE_R3_DATA		; fast Tube Bget
	BMI gbpb_incDataPtr
.gBpb_gb_fromhost
	JSR gpbp_B8memptr
	STA (&B8,X)
	JMP gbpb_incDataPtr
.gbpb_putbytes
	JSR gpbp_pb_LOADBYTE
	JSR BPUTV_ENTRY
	CLC
	RTS 				; always ok!
.gpbp_pb_LOADBYTE
	BIT MA+&1081
	BPL gbpb_pb_fromhost
	LDA TUBE_R3_DATA		; fast Tube Bput
	JMP gbpb_incDataPtr
.gbpb_pb_fromhost
	JSR gpbp_B8memptr
	LDA (&B8,X)

.gbpb_incDataPtr
	JSR RememberAXY			; Increment data ptr
	LDX #&01
.gbpb_incdblword1060X
{
	LDY #&04			; Increment double word
.gbpb_incdblword_loop
	INC MA+&1060,X
	BNE gbpb_incdblworkd_exit
	INX
	DEY
	BNE gbpb_incdblword_loop
.gbpb_incdblworkd_exit
	RTS
}
.fscv_osabouttoproccmd
	BIT CMDEnabledIf1
	BMI parameter_fsp
	DEC CMDEnabledIf1
.parameter_fsp
	LDA #&FF
	STA MA+&10CE
.param_out
	STA MA+&10CD
	RTS
.parameter_afsp
	LDA #&2A	; "*"
	STA MA+&10CE
	LDA #&23	; "#"
	BNE param_out

.osfile5_rdcatinfo
	JSR CheckFileExists		; READ CAT INFO
	JSR ReadFileAttribsToB0_Yoffset
	LDA #&01			; File type: 1=file found
	RTS
.osfile6_delfile
	JSR CheckFileNotLocked		; DELETE FILE
	JSR ReadFileAttribsToB0_Yoffset
	JSR DeleteCatEntry_YFileOffset
	BCC osfile_savecat_retA_1
.osfile1_updatecat
	JSR CheckFileExists		; UPDATE CAT ENTRY
	JSR osfile_update_loadaddr_Xoffset
	JSR osfile_update_execaddr_Xoffset
	BVC osfile_updatelocksavecat
.osfile3_wrexecaddr
	JSR CheckFileExists		; WRITE EXEC ADDRESS
	JSR osfile_update_execaddr_Xoffset
	BVC osfile_savecat_retA_1
.osfile2_wrloadaddr
	JSR CheckFileExists		; WRITE LOAD ADDRESS
	JSR osfile_update_loadaddr_Xoffset
	BVC osfile_savecat_retA_1
.osfile4_wrattribs
	JSR CheckFileExists		; WRITE ATTRIBUTES
	JSR CheckFileNotOpenY
.osfile_updatelocksavecat
	JSR osfile_updatelock
.osfile_savecat_retA_1
	JSR SaveCatToDisk
	LDA #&01
	RTS
.osfile_update_loadaddr_Xoffset
	JSR RememberAXY			; Update load address
	LDY #&02
	LDA (&B0),Y
	STA MA+&0F08,X
	INY
	LDA (&B0),Y
	STA MA+&0F09,X
	INY
	LDA (&B0),Y
	ASL A
	ASL A
	EOR MA+&0F0E,X
	AND #&0C
	BPL osfile_savemixedbyte	; always
.osfile_update_execaddr_Xoffset
	JSR RememberAXY			; Update exec address
	LDY #&06
	LDA (&B0),Y
	STA MA+&0F0A,X
	INY
	LDA (&B0),Y
	STA MA+&0F0B,X
	INY
	LDA (&B0),Y
	ROR A
	ROR A
	ROR A
	EOR MA+&0F0E,X
	AND #&C0
.osfile_savemixedbyte
	EOR MA+&0F0E,X			; save mixed byte
	STA MA+&0F0E,X
	CLV
	RTS
.osfile_updatelock
	JSR RememberAXY			; Update file locked flag
	LDY #&0E
	LDA (&B0),Y
	AND #&0A			; file attributes AUG pg.336
	BEQ osfile_notlocked
	LDA #&80			; Lock!
.osfile_notlocked
	EOR MA+&0E0F,X
	AND #&80
	EOR MA+&0E0F,X
	STA MA+&0E0F,X
	RTS

.CheckFileNotLocked
	JSR read_fspBA_findcatentry	; exit:X=Y=offset
	BCC ExitCallingSubroutine
.CheckFileNotLockedY
	LDA MA+&0E0F,Y
	BPL chklock_exit
.errFILELOCKED
	JSR ReportErrorCB
	EQUB &C3
	EQUS "Locked",0

.CheckFileNotLockedOrOpenY
	JSR CheckFileNotLockedY
.CheckFileNotOpenY
	JSR RememberAXY
	JSR IsFileOpen_Yoffset
	BCC checkexit
	JMP errFILEOPEN
.CheckFileExists
	JSR read_fspBA_findcatentry	; exit:X=Y=offset
	BCS checkexit			; If file found
.ExitCallingSubroutine
	PLA 				; Ret. To caller's caller
	PLA
	LDA #&00
.chklock_exit
	RTS

.read_fspBA_findcatentry
	JSR read_fspBA_reset
	JSR get_cat_firstentry80
	BCC checkexit
	TYA
	TAX 				; X=Y=offset
.SetParamBlockPointerB0
	LDA MA+&10DB			; Ptr to OSFILE param block
	STA &B0
	LDA MA+&10DC
	STA &B1
.checkexit
	RTS

	\ *** Calc amount of ram available ***
.CalcRAM
	LDA #&83
	JSR OSBYTE			; YX=OSHWM (PAGE)
	STY PAGE
	LDA #&84
	JSR OSBYTE			; YX=HIMEM
	TYA
	SEC
	SBC PAGE
	STA RAMBufferSize		; HIMEM page-OSHWM page
	RTS

IF NOT(_SWRAM_)
.ClaimStaticWorkspace
	LDX #&0A
	JSR osbyte8F_servreq		; Issue service request &A
	JSR SetPrivateWorkspacePointerB0
	LDY #<ForceReset
	LDA #&FF
	STA (&B0),Y			; Data valid in SWS
	STA ForceReset
	INY
	STA (&B0),Y			; Set pws is "empty"
	RTS

.SetPrivateWorkspacePointerB0
	PHA 				; Set word &B0 to
	LDA #&00
	STA &B0
	LDX PagedRomSelector_RAMCopy	; point to Private Workspace
	LDA PagedROM_PrivWorkspaces,X
IF NOT(_MASTER_)
	AND #&3F			; bits 7 & 6 are used as flags
ENDIF
	STA &B1
	PLA
	RTS
ENDIF


.osbyte0F_flushinbuf2
	JSR RememberAXY
.osbyte0F_flushinbuf
	LDA #&0F
	LDX #&01
	LDY #&00
	BEQ goOSBYTE			; always
.osbyte03_Aoutstream
	TAX
.osbyte03_Xoutstream
	LDA #&03
	BNE goOSBYTE			; always
.osbyte7E_ackESCAPE2
	JSR RememberAXY
.osbyte7E_ackESCAPE
	LDA #&7E
	BNE goOSBYTE
.osbyte8F_servreq
	LDA #&8F
	BNE goOSBYTE
;;.osbyteFF_startupopts
;;	LDA #&FF
.osbyte_X0YFF
	LDX #&00
.osbyte_YFF
	LDY #&FF
.goOSBYTE
	JMP OSBYTE

	\ Vector table copied to &0212
.vectors_table
	EQUW &FF1B	; FILEV
	EQUW &FF1E	; ARGSV
	EQUW &FF21	; BGETV
	EQUW &FF24	; BPUTV
	EQUW &FF27	; GBPBV
	EQUW &FF2A	; FINDV
	EQUW &FF2D	; FSCV

	\ Extended vector table
.extendedvectors_table
	EQUW FILEV_ENTRY
	BRK
	EQUW ARGSV_ENTRY
	BRK
	EQUW BGETV_ENTRY
	BRK
	EQUW BPUTV_ENTRY
	BRK
	EQUW GBPBV_ENTRY
	BRK
	EQUW FINDV_ENTRY
	BRK
	EQUW FSCV_ENTRY
	BRK

	\ OSFSC table 1 low bytes
.fscv_table1
	EQUB LO(fscv0_starOPT-1)
	EQUB LO(fscv1_EOF_Yhndl-1)
	EQUB LO(fscv2_4_11_starRUN-1)
	EQUB LO(fscv3_unreccommand-1)
	EQUB LO(fscv2_4_11_starRUN-1)
	EQUB LO(fscv5_starCAT-1)
	EQUB LO(fscv6_shutdownfilesys-1)
	EQUB LO(fscv7_hndlrange-1)
	EQUB LO(fscv_osabouttoproccmd-1)
	EQUB LO(fscv9_starEX-1)
	EQUB LO(fscv10_starINFO-1)
	EQUB LO(fscv2_4_11_starRUN-1)


	\ OSFSC table 2 high bytes
.fscv_table2
	EQUB HI(fscv0_starOPT-1)
	EQUB HI(fscv1_EOF_Yhndl-1)
	EQUB HI(fscv2_4_11_starRUN-1)
	EQUB HI(fscv3_unreccommand-1)
	EQUB HI(fscv2_4_11_starRUN-1)
	EQUB HI(fscv5_starCAT-1)
	EQUB HI(fscv6_shutdownfilesys-1)
	EQUB HI(fscv7_hndlrange-1)
	EQUB HI(fscv_osabouttoproccmd-1)
	EQUB HI(fscv9_starEX-1)
	EQUB HI(fscv10_starINFO-1)
	EQUB HI(fscv2_4_11_starRUN-1)

	\ OSFIND tables
.finv_tablelo
	EQUB LO(osfileFF_loadfiletoaddr-1)
	EQUB LO(osfile0_savememblock-1)
	EQUB LO(osfile1_updatecat-1)
	EQUB LO(osfile2_wrloadaddr-1)
	EQUB LO(osfile3_wrexecaddr-1)
	EQUB LO(osfile4_wrattribs-1)
	EQUB LO(osfile5_rdcatinfo-1)
	EQUB LO(osfile6_delfile-1)

.finv_tablehi
	EQUB HI(osfileFF_loadfiletoaddr-1)
	EQUB HI(osfile0_savememblock-1)
	EQUB HI(osfile1_updatecat-1)
	EQUB HI(osfile2_wrloadaddr-1)
	EQUB HI(osfile3_wrexecaddr-1)
	EQUB HI(osfile4_wrattribs-1)
	EQUB HI(osfile5_rdcatinfo-1)
	EQUB HI(osfile6_delfile-1)

	\ GBPB tables
.gbpbv_table1
	EQUB LO(NotCmdTable2)
	EQUB LO(gbpb_putbytes)
	EQUB LO(gbpb_putbytes)
	EQUB LO(gbpb_getbyteSAVEBYTE)
	EQUB LO(gbpb_getbyteSAVEBYTE)
	EQUB LO(gbpb5_getmediatitle)
	EQUB LO(gbpb6_rdcurdirdevice)
	EQUB LO(gbpb7_rdcurlibdevice)
	EQUB LO(gbpb8_rdfilescurdir)

.gbpbv_table2
	EQUB HI(NotCmdTable2)
	EQUB HI(gbpb_putbytes)
	EQUB HI(gbpb_putbytes)
	EQUB HI(gbpb_getbyteSAVEBYTE)
	EQUB HI(gbpb_getbyteSAVEBYTE)
	EQUB HI(gbpb5_getmediatitle)
	EQUB HI(gbpb6_rdcurdirdevice)
	EQUB HI(gbpb7_rdcurlibdevice)
	EQUB HI(gbpb8_rdfilescurdir)

.gbpbv_table3
	EQUB &04
	EQUB &02
	EQUB &03
	EQUB &06
	EQUB &07
	EQUB &04
	EQUB &04
	EQUB &04
	EQUB &04


.fscv7_hndlrange
	LDX #filehndl%+1 ;11		; lowest hndl issued
	LDY #filehndl%+5 ;15		; highest hndl poss.
.closeallfiles_exit
	RTS

.fscv6_shutdownfilesys
	JSR RememberAXY
IF _DEBUG
	JSR PrintString
	EQUB "Shutting down MMFS", 13
ENDIF
IF _MASTER_
	NOP
   JSR CloseSPOOLEXECfiles
	JMP SERVICE0A_claim_statworkspace ; save static to private workspace
ELSE
	;; fall through into CloseSPOOLEXECfiles
ENDIF

.CloseSPOOLEXECfiles
	LDA #&77			   ; Close any SPOOL or EXEC files
	JMP OSBYTE			; (Causes ROM serv.call &10)

	\ *CLOSE
.CMD_CLOSE
	LDA #&20
	STA MA+&1086
.CloseAllFiles_Osbyte77
	JSR CloseSPOOLEXECfiles
.CloseAllFiles
{
	LDA #&00			; intch=intch+&20
.closeallfiles_loop
	CLC
	ADC #&20
	BEQ closeallfiles_exit
	TAY
	JSR CloseFile_Yintch
	BNE closeallfiles_loop		; always
}

.CloseFiles_Yhandle
	LDA #&20			; Update catalogue if write only
	STA MA+&1086
	TYA
	BEQ CloseAllFiles_Osbyte77	; If y=0 Close all files
.Check_Yhandle_exists_and_close
	JSR CheckChannel_Yhndl_exYintch

.CloseFile_Yintch
{
	PHA 				; Save A
	JSR IsHndlinUse_Yintch		; (Saves X to &10C5)
	BCS closefile_exit		; If file not open
	LDA MA+&111B,Y			; bit mask
	EOR #&FF
	AND MA+&10C0
	STA MA+&10C0			; Clear 'open' bit
	LDA MA+&1117,Y			; A=flag byte
	AND #&60
	BEQ closefile_exit		; If bits 5&6=0
	JSR Channel_SetDirDrv_GetCatEntry_Yintch
	LDA MA+&1117,Y			; If file extended and not
	AND MA+&1086			; forcing buffer to disk
	BEQ closefile_buftodisk		; update the file length
	LDX MA+&10C3			; X=cat offset
	LDA MA+&1114,Y			; File lenth = EXTENT
	STA MA+&0F0C,X			; Len lo
	LDA MA+&1115,Y
	STA MA+&0F0D,X			; Len mi
	LDA MA+&1116,Y
	JSR A_rolx4			; Len hi
	EOR MA+&0F0E,X			; "mixed byte"
	AND #&30
	EOR MA+&0F0E,X
	STA MA+&0F0E,X
	JSR SaveCatToDisk		; Update catalog
	LDY MA+&10C2
.closefile_buftodisk
	JSR ChannelBufferToDisk_Yintch	; Restores Y
.closefile_exit
	LDX MA+&10C5			; Restore X (IsHndlInUse)
	PLA 				; Restore A
	RTS
}

.Channel_SetDirDrv_GetCatEntry_Yintch
	JSR Channel_SetDirDrive_Yintch
.Channel_GetCatEntry_Yintch
{
	LDX #&06			; Copy filename from
.chnl_getcatloop
	LDA MA+&110C,Y			; channel info to &C5
	STA &C5,X
	DEY
	DEY
	DEX
	BPL chnl_getcatloop
	JSR get_cat_firstentry80fname
	BCC errDISKCHANGED		; If file not found
	STY MA+&10C3			; ?&10C3=cat file offset
	LDY MA+&10C2			; Y=intch
}
.chkdskchangexit
	RTS

.Channel_SetDirDrive_Yintch
	LDA MA+&110E,Y			; Directory
	AND #&7F
	STA DirectoryParam
	LDA MA+&1117,Y			; Drive
	JMP SetCurrentDrive_Adrive

.CheckForDiskChange
	JSR RememberAXY
	LDA MA+&0F04
	JSR LoadCurDrvCat2
	CMP MA+&0F04
	BEQ chkdskchangexit		; If cycle no not changed!

.errDISKCHANGED
	JSR errDISK
	EQUB &C8
	EQUS "changed",0

	\ OSFIND: A=&40 ro, &80 wo, &C0 rw
.FINDV_ENTRY
{
	AND #&C0			; Bit 7=open for output
	BNE findvnot0_openfile		; Bit 6=open for input
	JSR RememberAXY
	JMP CloseFiles_Yhandle		; Close file #Y

.findvnot0_openfile
	JSR RememberXYonly		; Open file
	STX &BA				; YX=Location of filename
	STY &BB
	STA &B4				; A=Operation
	BIT &B4
	PHP
	JSR read_fspBA_reset
	JSR parameter_fsp
	JSR get_cat_firstentry80
	BCS findv_filefound		; If file found
	PLP
	BVC findv_createfile		; If not read only = write only
	LDA #&00			; A=0=file not found
	RTS 				; EXIT

.findv_createfile
	PHP 				; Clear data
	LDA #&00			; BC-C3=0
	LDX #&07			; 1074-107B=0
.findv_loop1
	STA &BC,X
	STA MA+&1074,X
	DEX
	BPL findv_loop1
	DEC &BE
	DEC &BF
	DEC MA+&1076
	DEC MA+&1077
	LDA #&40
	STA &C3				; End address = &4000
	JSR CreateFile_FSP		; Creates 40 sec buffer
.findv_filefound
	PLP 				; in case another file created
	PHP
	BVS findv_readorupdate		; If opened for read or update
	JSR CheckFileNotLockedY		; If locked report error
.findv_readorupdate
	JSR IsFileOpen_Yoffset		; Exits with Y=intch, A=flag
	BCC findv_openchannel		; If file not open
.findv_loop2
	LDA MA+&110C,Y
	BPL errFILEOPEN			; If already opened for writing
	PLP
	PHP
	BMI errFILEOPEN			; If opening again to write
	JSR IsFileOpenContinue		; ** File can only be opened  **
	BCS findv_loop2			; ** once if being written to **
.findv_openchannel
	LDY MA+&10C2			; Y=intch
	BNE SetupChannelInfoBlock_Yintch

.errTOOMANYFILESOPEN
	JSR ReportErrorCB
	EQUB &C0
	EQUS "Too many open",0
}

.errFILEOPEN
	JSR ReportErrorCB
	EQUB &C2
	EQUS "Open",0


.SetupChannelInfoBlock_Yintch
{
	LDA #&08
	STA MA+&10C4
.chnlblock_loop1
	LDA MA+&0E08,X			; Copy file name & attributes
	STA MA+&1100,Y			; to channel info block
	INY
	LDA MA+&0F08,X
	STA MA+&1100,Y
	INY
	INX
	DEC MA+&10C4
	BNE chnlblock_loop1

	LDX #&10
	LDA #&00			; Clear rest of block
.chnlblock_loop2
	STA MA+&1100,Y
	INY
	DEX
	BNE chnlblock_loop2

	LDA MA+&10C2			; A=intch
	TAY
	JSR A_rorx5
	ADC #MP+&11
	STA MA+&1113,Y			; Buffer page
	LDA MA+&10C1
	STA MA+&111B,Y			; Mask bit
	ORA MA+&10C0
	STA MA+&10C0			; Set bit in open flag byte
	LDA MA+&1109,Y			; Length0
	ADC #&FF			; If Length0>0 C=1
	LDA MA+&110B,Y			; Length1
	ADC #&00
	STA MA+&1119,Y			; Sector count
	LDA MA+&110D,Y			; Mixed byte
	ORA #&0F
	ADC #&00			; Add carry flag
	JSR A_rorx4and3			; Length2
	STA MA+&111A,Y
	PLP
	BVC chnlblock_setBit5		; If not read = write
	BMI chnlblock_setEXT		; If updating
	LDA #&80			; Set Bit7 = Read Only
	ORA MA+&110C,Y
	STA MA+&110C,Y
.chnlblock_setEXT
	LDA MA+&1109,Y			; EXTENT=file length
	STA MA+&1114,Y
	LDA MA+&110B,Y
	STA MA+&1115,Y
	LDA MA+&110D,Y
	JSR A_rorx4and3
	STA MA+&1116,Y
.chnlblock_cont
	LDA CurrentDrv			; Set drive
	ORA MA+&1117,Y
	STA MA+&1117,Y
	TYA 				; convert intch to handle
	JSR A_rorx5
	ORA #filehndl% 			; &10
	RTS 				; RETURN A=handle

.chnlblock_setBit5
	LDA #&20			; Set Bit5 = Update cat file len
	STA MA+&1117,Y			; when channel closed
	BNE chnlblock_cont		; always
}


.IsFileOpenContinue
	TXA 				; Continue looking for more
	PHA 				; instances of file being open
	JMP fop_nothisfile

.IsFileOpen_Yoffset
	LDA #&00
	STA MA+&10C2
	LDA #&08
	STA &B5				; Channel flag bit
	TYA
	TAX 				; X=cat offset
	LDY #&A0			; Y=intch
.fop_main_loop
	STY &B3
	TXA 				; save X
	PHA
	LDA #&08
	STA &B2				; cmpfn_loop counter
	LDA &B5
	BIT MA+&10C0
	BEQ fop_channelnotopen		; If channel not open
	LDA MA+&1117,Y
	EOR CurrentDrv
	AND #&03
	BNE fop_nothisfile		; If not current drv?
.fop_cmpfn_loop
	LDA MA+&0E08,X			; Compare filename
	EOR MA+&1100,Y
	AND #&7F
	BNE fop_nothisfile
	INX
	INY
	INY
	DEC &B2
	BNE fop_cmpfn_loop
	SEC
	BCS fop_matchifCset		; always
.fop_channelnotopen
	STY MA+&10C2			; Y=intch = allocated to new channel
	STA MA+&10C1			; A=Channel Flag Bit
.fop_nothisfile
	SEC
	LDA &B3
	SBC #&20
	STA &B3				; intch=intch-&20
	ASL &B5				; flag bit << 1
	CLC
.fop_matchifCset
	PLA 				; restore X
	TAX
	LDY &B3				; Y=intch
	LDA &B5				; A=flag bit
	BCS fop_exit
	BNE fop_main_loop		; If flag bit <> 0
.fop_exit
	RTS 				; Exit: A=flag Y=intch

;OSARGS A=&FF
.ChannelBufferToDisk_Yhandle_A0
	JSR ReturnWithA0
.ChannelBufferToDisk_Yhandle
{
	LDA MA+&10C0			; Force buffer save
	PHA 				; Save opened channels flag byte
	LDA #&00			; Don't update catalogue
	STA MA+&1086
	TYA 				; A=handle
	BNE chbuf1
	JSR CloseAllFiles
	BEQ chbuf2			; always
.chbuf1
	JSR Check_Yhandle_exists_and_close ; Bug fix to only close file and not update catalogue
.chbuf2
	PLA 				; Restore
	STA MA+&10C0
	RTS
}

.ReturnWithA0
	PHA 				; Sets the value of A
	TXA 				; restored by RememberAXY
	PHA 				; after returning from calling
	LDA #&00			; sub routine to 0
	TSX
	STA &0109,X
	PLA
	TAX
	PLA
	RTS

.ARGSV_ENTRY
{
	JSR RememberAXY
	CMP #&FF
	BEQ ChannelBufferToDisk_Yhandle_A0	; If file(s) to media
	CPY #&00
	BEQ argsv_Y0
	CMP #&03
	BCS argsv_exit			; If A>=3
	JSR ReturnWithA0
	CMP #&01
	BNE argsv_rdseqptr_or_filelen
	JMP argsv_WriteSeqPointer

.argsv_Y0
	CMP #&02			; If A>=2
	BCS argsv_exit
	JSR ReturnWithA0
	BEQ argsv_filesysnumber		; If A=0
	LDA #&FF
	STA &02,X			; 4 byte address of
	STA &03,X			; "rest of command line"
	LDA MA+&10D9			; (see *run code)
	STA &00,X
	LDA MA+&10DA
	STA &01,X
.argsv_exit
	RTS

.argsv_filesysnumber
	LDA #filesysno%			; on exit: A = filing system
	TSX
	STA &0105,X
	RTS
}

.argsv_rdseqptr_or_filelen
	JSR CheckChannel_Yhndl_exYintch	; A=0 OR A=2
	STY MA+&10C2
	ASL A				; A becomes 0 or 4
	ADC MA+&10C2
	TAY
	LDA MA+&1110,Y
	STA &00,X
	LDA MA+&1111,Y
	STA &01,X
	LDA MA+&1112,Y
	STA &02,X
	LDA #&00
	STA &03,X
	RTS

.IsHndlinUse_Yintch
{
	PHA				; Save A
	STX MA+&10C5			; Save X
	TYA
	AND #&E0
	STA MA+&10C2			; Save intch
	BEQ hndlinuse_notused_C1
	JSR A_rorx5			; ch.1-7
	TAY				; creat bit mask
	LDA #&00			; 1=1000 0000
	SEC				; 2=0100 0000 etc
.hndlinsue_loop
	ROR A
	DEY
	BNE hndlinsue_loop
	LDY MA+&10C2			; Y=intch
	BIT MA+&10C0			; Test if open
	BNE hndlinuse_used_C0
.hndlinuse_notused_C1
	PLA
	SEC
	RTS
.hndlinuse_used_C0
	PLA
	CLC
	RTS
}


;; .conv_Xhndl_intch_exYintch
;;	PHA
;;	TXA
;;	JMP conv_hndl_X_entry
.conv_Yhndl_intch_exYintch
	PHA 				; &10 to &17 are valid
	TYA
.conv_hndl_X_entry
{
	CMP #filehndl% 			; 10
	BCC conv_hndl10
	CMP #filehndl%+8		; 18
	BCC conv_hndl18
.conv_hndl10
	LDA #&08			; exit with C=1,A=0	;intch=0
.conv_hndl18
	JSR A_rolx5			; if Y<&10 or >&18
	TAY 				; ch0=&00, ch1=&20, ch2=&40
	PLA 				; ch3=&60...ch7=&E0
	RTS 				; c=1 if not valid
}

.ClearEXECSPOOLFileHandle
{
	LDA #&C6
	JSR osbyte_X0YFF		; X = *EXEC file handle
	TXA
	BEQ ClearSpoolhandle		; branch if no handle allocated
	JSR ConvertXhndl_exYintch
	BNE ClearSpoolhandle		; If Y<>?10C2
	LDA #&C6			; Clear *EXEC file handle
	BNE osbyte_X0Y0

.ClearSpoolhandle
	LDA #&C7			; X = *SPOOL handle
	JSR osbyte_X0YFF
	JSR ConvertXhndl_exYintch
	BNE clrsplhndl_exit		; If Y<>?10C2
	LDA #&C7			; Clear *SPOOL file handle
.osbyte_X0Y0
	LDX #&00
	LDY #&00
	JMP OSBYTE

.ConvertXhndl_exYintch
	TXA
	TAY
	JSR conv_Yhndl_intch_exYintch
	CPY MA+&10C2			; Owner?
.clrsplhndl_exit
	RTS
}

.fscv1_EOF_Yhndl
{
	PHA
	TYA
	PHA
	TXA
	TAY
	JSR CheckChannel_Yhndl_exYintch
	TYA
	JSR CmpPTR			; X=Y
	BNE eof_NOTEND
	LDX #&FF			; exit with X=FF
	BNE eof_exit
.eof_NOTEND
	LDX #&00			; exit with X=00
.eof_exit
	PLA
	TAY
	PLA
}
.checkchannel_okexit
	RTS

.CheckChannel_Yhndl_exYintch
	JSR conv_Yhndl_intch_exYintch
	JSR IsHndlinUse_Yintch
	BCC checkchannel_okexit
	JSR ClearEXECSPOOLFileHandle	; Next sub routine also calls this!

.errCHANNEL
	JSR ReportErrorCB
	EQUB &DE
	EQUS "Channel",0

.errEOF
	JSR ReportErrorCB
	EQUB &DF
	EQUS "EOF",0

.BGETV_ENTRY
{
	JSR RememberXYonly
	JSR CheckChannel_Yhndl_exYintch
	TYA 				; A=Y
	JSR CmpPTR
	BNE bg_notEOF			; If PTR<>EXT
	LDA MA+&1117,Y			; Already at EOF?
	AND #&10
	BNE errEOF			; IF bit 4 set
	LDA #&10
	JSR ChannelFlags_SetBits	; Set bit 4
	LDX MA+&10C5
	LDA #&FE
	SEC
	RTS 				; C=1=EOF
.bg_notEOF
	LDA MA+&1117,Y
	BMI bg_samesector1		; If buffer ok
	JSR Channel_SetDirDrive_Yintch
	JSR ChannelBufferToDisk_Yintch	; Save buffer
	SEC
	JSR ChannelBufferRW_Yintch_C1read	; Load buffer
.bg_samesector1
	JSR load_then_incSeqPtr_Yintch	; load buffer ptr into BA/BB then increments Seq Ptr
	LDA (&BA, X)			; Byte from buffer
	CLC
	RTS				; C=0=NOT EOF
}

.CalcBufferSectorForPTR
	CLC
	LDA MA+&110F,Y			; Start Sector + Seq Ptr
	ADC MA+&1111,Y
	STA &C3
	STA MA+&111C,Y			; Buffer sector
	LDA MA+&110D,Y
	AND #&03
	ADC MA+&1112,Y
	STA &C2
	STA MA+&111D,Y

.ChannelFlags_SetBit7
	LDA #&80			; Set/Clear flags (C=0 on exit)
.ChannelFlags_SetBits
	ORA MA+&1117,Y
	BNE chnflg_save
.ChannelFlags_ClearBit7
	LDA #&7F
.ChannelFlags_ClearBits
	AND MA+&1117,Y
.chnflg_save
	STA MA+&1117,Y
	CLC
	RTS

.ChannelBufferToDisk_Yintch
	LDA MA+&1117,Y
	AND #&40			; Bit 6 set?
	BEQ chnbuf_exit2		; If no exit
	CLC 				; C=0=write buffer

.ChannelBufferRW_Yintch_C1read
{
	PHP 				; Save C
	INC MA+&10DD			; Remember in case of error?
	LDY MA+&10C2			; Setup NMI vars
	LDA MA+&1113,Y			; Buffer page
	STA &BD				;Data ptr
	LDA #&FF			; \ Set load address to host
	STA MA+&1074			; \
	STA MA+&1075			; \
	LDA #&00
	STA &BC
	STA &C0				; Sector
	LDA #&01
	STA &C1
	PLP
	BCS chnbuf_read			; IF c=1 load buffer else save
	LDA MA+&111C,Y			; Buffer sector
	STA &C3				; Start sec. b0-b7
	LDA MA+&111D,Y
	STA &C2				; "mixed byte"
	JSR SaveMemBlock
	LDY MA+&10C2			; Y=intch
	LDA #&BF			; Clear bit 6
	JSR ChannelFlags_ClearBits
	BCC chnbuf_exit			; always
.chnbuf_read
	JSR CalcBufferSectorForPTR	; sets NMI data ptr
	JSR LoadMemBlock		; Load buffer
.chnbuf_exit
	DEC MA+&10DD
	LDY MA+&10C2			; Y=intch
}
.chnbuf_exit2
	RTS

.errFILELOCKED2
	JMP errFILELOCKED

.errFILEREADONLY
	JSR ReportErrorCB
	EQUB &C1
	EQUS "Read only",0

.bput_Yintchan
	JSR RememberAXY
	JMP bp_entry
.BPUTV_ENTRY
	JSR RememberAXY
	JSR CheckChannel_Yhndl_exYintch
.bp_entry
{
	PHA
	LDA MA+&110C,Y
	BMI errFILEREADONLY
	LDA MA+&110E,Y
	BMI errFILELOCKED2
	JSR Channel_SetDirDrive_Yintch
	TYA
	CLC
	ADC #&04
	JSR CmpPTR
	BNE bp_noextend			; If PTR<>Sector Count, i.e Ptr<sc
	JSR Channel_GetCatEntry_Yintch	; Enough space in gap?
	LDX MA+&10C3			; X=cat file offset
	SEC 				; Calc size of gap
	LDA MA+&0F07,X			; Next file start sector
	SBC MA+&0F0F,X			; This file start
	PHA 				; lo byte
	LDA MA+&0F06,X
	SBC MA+&0F0E,X			; Mixed byte
	AND #&03			; hi byte
	CMP MA+&111A,Y			; File size in sectors
	BNE bp_extendby100		; If must be <gap size
	PLA
	CMP MA+&1119,Y
	BNE bp_extendtogap		; If must be <gap size
	STY &B4				; Error, save intch handle
	STY MA+&10C2			; for clean up
	JSR ClearEXECSPOOLFileHandle
.errCANTEXTEND
	JSR ReportErrorCB
	EQUB &BF
	EQUS "Can't extend",0

.bp_extendby100
	LDA MA+&111A,Y			; Add maximum of &100
	CLC 				; to sector count
	ADC #&01			; (i.e. 64K)
	STA MA+&111A,Y			; [else set to size of gap]
	ASL A				; Update cat entry
	ASL A
	ASL A
	ASL A
	EOR MA+&0F0E,X			; Mixed byte
	AND #&30
	EOR MA+&0F0E,X
	STA MA+&0F0E,X			; File len 2
	PLA
	LDA #&00
.bp_extendtogap
	STA MA+&0F0D,X			; File len 1
	STA MA+&1119,Y
	LDA #&00
	STA MA+&0F0C,X			; File len 0
	JSR SaveCatToDisk
	LDY MA+&10C2			; Y=intch
.bp_noextend
	LDA MA+&1117,Y
	BMI bp_savebyte			; If PTR in buffer
	JSR ChannelBufferToDisk_Yintch	; Save buffer
	LDA MA+&1114,Y			; EXT byte 0
	BNE bp_loadbuf			; IF <>0 load buffer
	TYA
	JSR CmpPTR			; A=Y
	BNE bp_loadbuf			; If PTR<>EXT, i.e. PTR<EXT
	JSR CalcBufferSectorForPTR	; new sector!
	BNE bp_savebyte			; always
.bp_loadbuf
	SEC 				; Load buffer
	JSR ChannelBufferRW_Yintch_C1read
.bp_savebyte
	LDA #&40			; Bit 6 set = new data
	JSR ChannelFlags_SetBits
	JSR load_then_incSeqPtr_Yintch	; load buffer ptr into BA/BB then increments Seq Ptr
	PLA
	STA (&BA,X)			; Byte to buffer
	TYA
	JSR CmpPTR
	BCC bp_exit			; If PTR<EXT
	LDA #&20			; Update cat file len when closed
	JSR ChannelFlags_SetBits	; Set bit 5
	LDX #&02			; EXT=PTR
.bp_setextloop
	LDA MA+&1110,Y
	STA MA+&1114,Y
	INY
	DEX
	BPL bp_setextloop
}
.bp_exit
	RTS


.argsv_WriteSeqPointer
{
	JSR RememberAXY			; Write Sequential Pointer
	JSR CheckChannel_Yhndl_exYintch	; (new ptr @ 00+X)
	LDY MA+&10C2
.wsploop
	JSR CmpNewPTRwithEXT
	BCS SetSeqPointer_Yintch	; If EXT >= new PTR
	LDA MA+&1114,Y			; else new PTR>EXT so pad with a 0
	STA MA+&1110,Y
	LDA MA+&1115,Y			; first, actual PTR=EXT
	STA MA+&1111,Y
	LDA MA+&1116,Y
	STA MA+&1112,Y
	JSR IsSeqPointerInBuffer_Yintch	; Update flags
	LDA &B6
	PHA 				; Save &B6,&B7,&B8
	LDA &B7
	PHA
	LDA &B8
	PHA
	LDA #&00
	JSR bput_Yintchan		; Pad
	PLA 				; Restore &B6,&B7,&B8
	STA &B8
	PLA
	STA &B7
	PLA
	STA &B6
	JMP wsploop			; Loop
}

.SetSeqPointer_Yintch
	LDA &00,X			; Set Sequential Pointer
	STA MA+&1110,Y
	LDA &01,X
	STA MA+&1111,Y
	LDA &02,X
	STA MA+&1112,Y

.IsSeqPointerInBuffer_Yintch
	LDA #&6F			; Clear bits 7 & 4 of 1017+Y
	JSR ChannelFlags_ClearBits
	LDA MA+&110F,Y			; Start sector
	ADC MA+&1111,Y			; Add sequ.ptr
	STA MA+&10C4
	LDA MA+&110D,Y			; Mixed byte
	AND #&03			; Start sector bits 8&9
	ADC MA+&1112,Y
	CMP MA+&111D,Y
	BNE bp_exit
	LDA MA+&10C4
	CMP MA+&111C,Y
	BNE bp_exit
	JMP ChannelFlags_SetBit7	; Seq.Ptr in buffered sector

.CmpPTR
	TAX
	LDA MA+&1112,Y
	CMP MA+&1116,X
	BNE cmpPE_exit
	LDA MA+&1111,Y
	CMP MA+&1115,X
	BNE cmpPE_exit
	LDA MA+&1110,Y
	CMP MA+&1114,X
.cmpPE_exit
	RTS

.CmpNewPTRwithEXT
	LDA MA+&1114,Y			; Compare ctl blk ptr
	CMP &00,X			; to existing
	LDA MA+&1115,Y			; Z=1 if same
	SBC &01,X			; (ch.1=&1138)
	LDA MA+&1116,Y
	SBC &02,X
	RTS 				; C=p>=n

\ DMB: Factor out some common code from BPUT/BGET

.load_then_incSeqPtr_Yintch
{
	LDA MA+&1110,Y			; Seq.Ptr
	STA &BA
	LDA MA+&1113,Y			; Buffer page
	STA &BB
	TYA
	TAX
	INC MA+&1110,X			; Seq.Ptr+=1
	BNE samesector
	JSR ChannelFlags_ClearBit7	; PTR in new sector!
	INC MA+&1111,X
	BNE samesector
	INC MA+&1112,X
.samesector
	LDX #&00
	RTS
}

	\ *HELP MMFS
.CMD_MMFS
	TYA
	LDX #0				; cmd table 1
	LDY #cmdtab1size		; no.of commands

.Prthelp_Xtable
{
	PHA
	JSR PrintString
	EQUB 13
	SYSTEM_NAME
	EQUB 32
	STX &BF
	STY &B7				; ?&B7 = command counter

	LDX #0				; Print ROM version number
.verloop
	LDA version,X
	BEQ verex
	JSR PrintChrA
	INX
	BNE verloop

.verex
	LDA #13
	JSR PrintChrA

.help_dfs_loop
	LDA #0
	STA &B9				; ?&B9=0=print command (not error)
	LDY #1
	JSR prtcmd_Print_Y_Spaces_IfNotErr	; print "  ";
	JSR prtcmdAtBCadd1		; print cmd & parameters
	JSR PrintNewLine		; print
	DEC &B7
	BNE help_dfs_loop
	PLA				; restore Y
	TAY
}
.morehelp
	LDX #cmdtab3			; more? Eg *HELP DFS UTILS
	JMP UnrecCommandTextPointer	; start cmd @ A3 in table


	\ *HELP DUTILS
.CMD_DUTILS
	TYA
	LDX #cmdtab4
IF _INCLUDE_CMD_DABOUT_
	LDY #cmdtab4size-1
ELSE
	LDY #cmdtab4size
ENDIF
	BNE Prthelp_Xtable

IF _UTILS_ OR _ROMS_
	\ *HELP UTILS
.CMD_UTILS
	TYA
	LDX #cmdtab2			; cmd table 2
	LDY #cmdtab2size		; Don't include last command (i.e. MMFS)
	BNE Prthelp_Xtable		; always
ENDIF


.CMD_NOTHELPTBL
{
	JSR GSINIT_A
	BEQ prtcmdparamexit		; null str
.cmd_nothelptlb_loop
	JSR GSREAD_A
	BCC cmd_nothelptlb_loop		; if not end of str
	BCS morehelp			; always
}

.Param_SyntaxErrorIfNull
	JSR GSINIT_A			; (if no params then syntax error)
	BEQ errSYNTAX			; branch if not null string
	RTS

IF NOT(_MM32_)
\ Only used in DRECAT and DFREE
.Param_SyntaxErrorIfNotNull
	JSR GSINIT_A			; (if no params then syntax error)
	BNE errSYNTAX			; branch if not null string
	RTS
ENDIF

.errSYNTAX
	JSR ReportError			; Print Syntax error
	EQUB &DC
	EQUS "Syntax: "
	STX &B9				; ?&B9=&100 offset (>0)
	JSR prtcmdAtBCadd1		; add command syntax
	LDA #&00
	JSR prtcmd_prtchr
	JMP &0100			; Cause BREAK!

.prtcmdAtBCadd1
{
	LDA #7				; A=column width
	STA &B8
	LDX &BF				; X=table offset
	CPX #cmdtab4
	BCC prtcmdloop			; All table 4 commands
	LDA #&44			; start with "D"
	JSR prtcmd_prtchr
.prtcmdloop
	INX	 			; If ?&B9=0 then print
	LDA cmdtable1,X			; else it's the &100 offset
	BMI prtcmdloop_exit		; If end of str
	JSR prtcmd_prtchr
	JMP prtcmdloop

.prtcmdloop_exit
	LDY &B8
	BMI prtcmdnospcs
	JSR prtcmd_Print_Y_Spaces_IfNotErr	; print spaces
.prtcmdnospcs
	STX &BF				; ready for next time

	LDA cmdtable1,X			; paramater code
	AND #&7F
	JSR prtcmdparam			; 1st parameter
	JSR A_rorx4			; 2nd parameter

.prtcmdparam
	JSR RememberAXY
	AND #&0F
	BEQ prtcmdparamexit		; no parameter
	TAY 				; Y=parameter no.
	LDA #&20
	JSR prtcmd_prtchr		; print space
	LDX #&FF			; Got to parameter Y
.prtcmdparam_findloop
	INX 				; (Each param starts with bit 7 set)
	LDA parametertable,X
	BPL prtcmdparam_findloop
	DEY
	BNE prtcmdparam_findloop	; next parameter
	AND #&7F			; Clear bit 7 of first chr
.prtcmdparam_loop
	JSR prtcmd_prtchr		;Print parameter
	INX
	LDA parametertable,X
	BPL prtcmdparam_loop
}
.prtcmdparamexit
	RTS

.prtcmd_prtchr
	JSR RememberAXY			; Print chr
	LDX &B9
	BEQ prtcmdparam_prtchr		; If printing help
	INC &B9
	STA &0100,X
	RTS

.prtcmdparam_prtchr
	DEC &B8				; If help print chr
	JMP PrintChrA

.prtcmd_Print_Y_Spaces_IfNotErr
{
	LDA &B9
	BNE prtcmd_yspc_exit		; If printing error exit
	LDA #&20			; Print space
.prtcmd_yspc_loop
	JSR prtcmd_prtchr
	DEY
	BPL prtcmd_yspc_loop
.prtcmd_yspc_exit
	RTS
}


.parametertable
	EQUS '<' OR &80,"drive>"			;1
	EQUS '<' OR &80,"afsp>"				;2
	EQUS '(' OR &80,"L)"				;3
	EQUS '(' OR &80,"<drive>)"			;4
	EQUS '(' OR &80,"<drive>)..."			;5
	EQUS '(' OR &80,"<dir>)"			;6
IF _MM32_
	EQUS '<' OR &80,"dos name>"			;7
ELSE
	EQUS '<' OR &80,"dno>/<dsp>"			;7
ENDIF

	EQUS '<' OR &80,"fsp>"				;8
IF _MM32_
	EQUS '(' OR &80,"<dos name>)"			;9
ELSE
	EQUS 'P' OR &80,"/U/N/K/R"			;9
ENDIF
	EQUS '<' OR &80,"title>"			;A
	EQUS '(' OR &80,"<num>)"			;B
	EQUS '<' OR &80,"source> <dest.>"		;C
	EQUS '<' OR &80,"old fsp> <new fsp>"		;D
IF _MM32_
	EQUS '<' OR &80,"filter>"			;E
ELSE
	EQUS '(' OR &80,"(<f.dno>) <t.dno>) (<adsp>)"	;E
ENDIF
	EQUS '4' OR &80,"0/80"				;F
	EQUB &FF


IF _INCLUDE_CMD_COMPACT_
.CMD_COMPACT
{
	JSR Param_OptionalDriveNo
	JSR PrintString			; "Compacting :"
	EQUS "Compacting :"

	STA MA+&10D1			; Source Drive No.
	STA MA+&10D2			; Dest Drive No.
	JSR PrintNibble
	JSR PrintNewLine
	LDY #&00
	JSR CloseFile_Yintch		; Close all files
	JSR CalcRAM
	JSR LoadCurDrvCat2		; Load catalogue
	LDY FilesX8
	STY &CA				; ?CA=file offset
	LDA #&02
	STA &C8
	LDA #&00
	STA &C9				; word C8=next free sector
.compact_loop
	LDY &CA
	JSR Y_sub8
	CPY #&F8
	BNE compact_checkfile		; If not end of catalogue
	LDA MA+&0F07			; Calc & print no. free sectors
	SEC 				; (disk sectors - word C8)
	SBC &C8
	PHA
	LDA MA+&0F06
	AND #&03
	SBC &C9
	JSR PrintNibble
	PLA
	JSR PrintHex
	JSR PrintString			; " free sectors"
	EQUS " free sectors",13
	NOP
	RTS 				; Finished compacting

.compact_checkfile
	STY &CA				; Y=cat offset
	JSR prt_InfoMsg_Yoffset		; Only if messages on
	LDY &CA				; Y preserved?
	LDA MA+&0F0C,Y			; A=Len0
	CMP #&01			; C=sec count
	LDA #&00
	STA &BC
	STA &C0
	ADC MA+&0F0D,Y			; A=Len1
	STA &C4
	LDA MA+&0F0E,Y
	PHP
	JSR A_rorx4and3			; A=Len2
	PLP
	ADC #&00
	STA &C5				; word C4=size in sectors
	LDA MA+&0F0F,Y			; A=sec0
	STA &C6
	LDA MA+&0F0E,Y
	AND #&03			; A=sec1
	STA &C7				; word C6=sector
	CMP &C9				; word C6=word C8?
	BNE compact_movefile		; If no
	LDA &C6
	CMP &C8
	BNE compact_movefile		; If no
	CLC
	ADC &C4
	STA &C8
	LDA &C9
	ADC &C5
	STA &C9				; word C8 += word C4
	JMP compact_fileinfo

.compact_movefile
	LDA &C8				; Move file
	STA MA+&0F0F,Y			; Change start sec in catalogue
	LDA MA+&0F0E,Y			; to word C8
	AND #&FC
	ORA &C9
	STA MA+&0F0E,Y
	LDA #&00
	STA &A8				; Don't create file
	STA &A9
	JSR SaveCatToDisk		; save catalogue
	JSR CopyDATABLOCK		; may use buffer @ &E00	;Move file
	JSR CheckCurDrvCat
.compact_fileinfo
	LDY &CA
	JSR prt_InfoLine_Yoffset
	JMP compact_loop
}
ENDIF

IF _INCLUDE_CMD_BACKUP_ OR _INCLUDE_CMD_DESTROY_ OR _INCLUDE_CMD_FORM_VERIFY_ OR _INCLUDE_CMD_DOP_
.IsEnabledOrGo
{
	BIT CMDEnabledIf1
	BPL isgoalready
	JSR GoYN
	BEQ isgo
	PLA 				; don't return to sub
	PLA
.isgo
	RTS
}
ENDIF

.Get_CopyDATA_Drives
{
	JSR Param_DriveNo_Syntax	; Get drives & calc ram & msg
	STA MA+&10D1			; Source drive
	JSR Param_DriveNo_Syntax
	STA MA+&10D2			; Destination drive

	CMP MA+&10D1
	BEQ baddrv			; Drives must be different!

	TYA
	PHA
	JSR CalcRAM			; Calc ram available
	JSR PrintString			; Copying from:
	EQUS "Copying from :"
	LDA MA+&10D1
	JSR PrintNibble_PrintString			; to :
	EQUS " to :"
	LDA MA+&10D2
	JSR PrintNibble
	JSR PrintNewLine
	PLA
	TAY
	CLC
}
.isgoalready
	RTS
.baddrv JMP errBADDRIVE


.ConfirmYNcolon
	JSR PrintString
	EQUS " : "
	BCC ConfirmYN

.GoYN
	JSR PrintString
	EQUS "Go (Y/N) ? "		; Go (Y/N) ?
	NOP

.ConfirmYN
{
	JSR osbyte0F_flushinbuf2
	JSR OSRDCH			; Get chr
	BCS err_ESCAPE			; If ESCAPE
	AND #&5F
	CMP #&59			; "Y"?
	PHP
	BEQ confYN
	LDA #&4E			; "N"
.confYN
	JSR PrintChrA
	JSR PrintNewLine
	PLP
	RTS
}

.err_ESCAPE
	JMP ReportESCAPE
.err_DISKFULL2
	JMP errDISKFULL

IF _INCLUDE_CMD_BACKUP_
.CMD_BACKUP
{
	JSR Get_CopyDATA_Drives
	JSR IsEnabledOrGo
	LDA #&00
	STA &C7
	STA &C9
	STA &C8
	STA &C6
	STA &A8				; Don't create file

	\ Source
	LDA MA+&10D1
	STA CurrentDrv
	JSR LoadCurDrvCat
	LDA MA+&0F07			; Size of source disk
	STA &C4				; Word C4 = size fo block
	LDA MA+&0F06
	AND #&03
	STA &C5

	\ Destination
	LDA MA+&10D2
	STA CurrentDrv
	JSR LoadCurDrvCat
	LDA MA+&0F06			; Is dest disk smaller?
	AND #&03
	CMP &C5
	BCC err_DISKFULL2
	BNE backup_copy
	LDA MA+&0F07
	CMP &C4
	BCC err_DISKFULL2

.backup_copy

	JSR CopyDATABLOCK
	JSR LoadCurDrvCat

	\ Update title in disk table

	LDX #&0A
.tloop
	CPX #&08
	BCC tskip1
	LDA MA+&0EF8,X
	BCS tskip2
.tskip1
	LDA MA+&0E00,X
.tskip2
	STA titlestr%,X
	DEX
	BPL tloop

IF _MM32_
	RTS
ELSE
	; Fall into  UpdateDiskTableTitle
ENDIF
}
ENDIF

IF NOT(_MM32_) AND (_INCLUDE_CMD_TITLE_ OR _INCLUDE_CMD_BACKUP_)
	\ Update title in disk table for disk in current drive
	\ Title at titlestr%
.UpdateDiskTableTitle
{
	JSR GetDriveStatus
	LDY #&0B
.loop
	LDA titlestr%,Y
	STA (&B0),Y
	DEY
	BPL loop
	JMP SaveDiskTable
}
ENDIF


IF _INCLUDE_CMD_COPY_
.CMD_COPY
{
	JSR parameter_afsp
	JSR Get_CopyDATA_Drives
	JSR Param_SyntaxErrorIfNull
	JSR read_fspTextPointer

	\ Source
	LDA MA+&10D1
	JSR SetCurrentDrive_Adrive
	JSR getcatentry
.copy_loop1
	LDA DirectoryParam
	PHA
	LDA &B6
	STA &AB
	JSR prt_InfoLine_Yoffset
	LDX #&00
.copy_loop2
	LDA MA+&0E08,Y
	STA &C5,X
	STA MA+&1050,X
	LDA MA+&0F08,Y
	STA &BB,X
	STA MA+&1047,X
	INX
	INY
	CPX #&08
	BNE copy_loop2
	LDA &C1
	JSR A_rorx4and3
	STA &C3
	LDA &BF
	CLC
	ADC #&FF
	LDA &C0
	ADC #&00
	STA &C4
	LDA &C3
	ADC #&00
	STA &C5
	LDA MA+&104E
	STA &C6
	LDA MA+&104D
	AND #&03
	STA &C7
	LDA #&FF
	STA &A8				; Create new file
	JSR CopyDATABLOCK

	\ Source
	LDA MA+&10D1
	JSR SetCurrentDrive_Adrive
	JSR LoadCurDrvCat2
	LDA &AB
	STA &B6
	PLA
	STA DirectoryParam
	JSR get_cat_nextentry
	BCS copy_loop1
	RTS
}
ENDIF

.cd_writedest_cat
{
	JSR cd_swapvars			; create file in destination catalogue

	\ Destination
	LDA MA+&10D2
	STA CurrentDrv
	LDA DirectoryParam
	PHA
	JSR LoadCurDrvCat2		; Load cat
	JSR get_cat_firstentry80fname
	BCC cd_writedest_cat_nodel	; If file not found
	JSR DeleteCatEntry_YFileOffset
.cd_writedest_cat_nodel
	PLA
	STA DirectoryParam
	JSR LoadAddrHi2
	JSR ExecAddrHi2
	LDA &C2				; mixed byte
	JSR A_rorx4and3
	STA &C4
	JSR CreateFile_2		; Saves cat
	LDA &C2				; Remember sector
	AND #&03
	PHA
	LDA &C3
	PHA
	JSR cd_swapvars			; Back to source
	PLA 				; Next free sec on dest
	STA &C8
	PLA
	STA &C9
	RTS

.cd_swapvars
	LDX #&11			; Swap BA-CB & 1045-1056
.cd_swapvars_loop
	LDA MA+&1045,X			; I.e. src/dest
	LDY &BA,X
	STA &BA,X
	TYA
	STA MA+&1045,X
	DEX
	BPL cd_swapvars_loop
	RTS
}

IF _INCLUDE_CMD_BACKUP_ OR _INCLUDE_CMD_COMPACT_ OR _INCLUDE_CMD_COPY_
.CopyDATABLOCK
{
	LDA #&00			; *** Move or copy sectors
	STA &BC				; Word &C4 = size of block
	STA &C0
	BEQ cd_loopentry		; always
.cd_loop
	LDA &C4
	TAY
	CMP RAMBufferSize		; Size of buffer
	LDA &C5
	SBC #&00
	BCC cd_part			; IF size<size of buffer
	LDY RAMBufferSize
.cd_part
	STY &C1
	LDA &C6				; C2/C3 = Block start sector
	STA &C3				; Start sec = Word C6
	LDA &C7
	STA &C2
	LDA PAGE			; Buffer address
	STA &BD
	LDA MA+&10D1
	STA CurrentDrv

	\ Source
	JSR SetLoadAddrToHost
	JSR LoadMemBlock
	LDA MA+&10D2
	STA CurrentDrv
	BIT &A8
	BPL cd_skipwrcat		; Don't create file
	JSR cd_writedest_cat
	LDA #&00
	STA &A8				; File created!
.cd_skipwrcat
	LDA &C8				; C2/C3 = Block start sector
	STA &C3				; Start sec = Word C8
	LDA &C9
	STA &C2
	LDA PAGE			; Buffer address
	STA &BD

	\ Destination
	JSR SetLoadAddrToHost
	JSR SaveMemBlock
	LDA &C1				; Word C8 += ?C1
	CLC 				; Dest sector start
	ADC &C8
	STA &C8
	BCC cd_inc1
	INC &C9
.cd_inc1
	LDA &C1				; Word C6 += ?C1
	CLC 				; Source sector start
	ADC &C6
	STA &C6
	BCC cd_inc2
	INC &C7
.cd_inc2
	SEC	 			; Word C4 -= ?C1
	LDA &C4				; Sector counter
	SBC &C1
	STA &C4
	BCS cd_loopentry
	DEC &C5
.cd_loopentry
	LDA &C4
	ORA &C5
	BNE cd_loop			; If Word C4 <> 0
	RTS
}
ENDIF

IF _INCLUDE_CMD_FORM_VERIFY_
.CMD_VERIFY
	LDA #&00			; \\\\\ *VERIFY
	BEQ vform1

.CMD_FORM
	LDA #&FF			; \\\\\ *FORM
.vform1
;;{
{
	STA &C9
	STA &B2				; If -ve, check go ok, calc. memory
	BPL vform3_ok			; If verifying

	JSR Param_SyntaxErrorIfNull	; Get number of tracks (40/80)
	JSR Param_ReadNum		; Read tracks parameter
	BCS vform2_syntax
IF NOT(_MM32_)
	ASL A
	BNE vform2_syntax
ENDIF
	STX &B5				; no. of tracks
	CPX #&28
	BEQ vform3_ok			; If =40
	CPX #&50
	BEQ vform3_ok			; If =80

.vform2_syntax
	JMP errSYNTAX

.vform3_ok
	JSR GSINIT_A
	STY &CA
	BNE vform5_driveloop

	\ No drive param, so ask!
	BIT &C9
	BMI vform4_form			; IF formatting
	JSR PrintString
	EQUS "Verify"			; Verify
	BCC vform4_askdrive		; always
.vform4_form
	JSR PrintString
	EQUS "Format"			; Format
	NOP

.vform4_askdrive
	JSR PrintString
	EQUS " which drive ? "		; which drive ?
	NOP
	JSR OSRDCH
	BCS jmp_reportEscape
	CMP #&20
	BCC jmp_errBadDrive
	JSR PrintChrA
	SEC
	SBC #&30
	BCC jmp_errBadDrive
	CMP #&04
	BCS jmp_errBadDrive		; If >=4
	STA CurrentDrv
	JSR PrintNewLine
	LDY &CA
	JMP vform6_drivein

.vform5_driveloop
	JSR Param_DriveNo_BadDrive
.vform6_drivein
	STY &CA
	BIT &B2				; If verifying or already done don't ask!
	BPL vform7_go
IF _MM32_
	LSR &B2				; Clear bit 7
ENDIF
	JSR IsEnabledOrGo
.vform7_go
	JSR VFCurDrv
	LDY &CA
	JSR GSINIT_A
	BNE vform5_driveloop		; More drives?
	RTS

.jmp_reportEscape
	JMP ReportESCAPE
.jmp_errBadDrive
	JMP errBADDRIVE

	\\ Verify / Format current drive
.VFCurDrv
	BIT &C9
	BMI vf1				; If formatting

IF NOT(_MM32_)
	JSR CheckCurDrvFormatted
ENDIF
	JSR PrintString
	EQUS "Verifying"
	BCC vf2				; always

.vf1
IF NOT(_MM32_)
	JSR CheckCurDrvUnformatted
	JSR ClearCatalogue
ENDIF
	JSR PrintString
	EQUS "Formatting"

	LDX CurrentDrv
IF _MM32_
	STX OWCtlBlock		; drive
ELSE
	STX &B2				; clear bit 7 (enabled flag)
ENDIF

.vf2
	JSR PrintString
	EQUS " drive "
	LDA CurrentDrv
	JSR PrintNibble_PrintString
	EQUS " track   "
	NOP
IF _MM32_
	LDA #&63			; Format cmd
	LDX #5				; 5 parameters
ENDIF
	BIT &C9
	BMI vf4				; If formatting

	\ If verifying calc. no. of tracks
	JSR TracksOnDisk	; reads catalogue, pops drive
	TXA
	BEQ vf6_exit

	STA &B5				; number of tracks
IF _MM32_
	LDA #&5F			; Verify cmd
	LDX #3				; 3 parameters
ENDIF

.vf4
IF _MM32_
	\ Pop control block
	STA OWCtlBlock+6	; FDC command
	STX OWCtlBlock+5	; param count
	LDA #0
	STA &B4				; track
ELSE
	LDX #&FF
	STX CurrentCat			; Invalid catalogue
	INX
	STX &B4				; track
ENDIF

.vf5_trackloop
	LDA #&08			; print track number
	JSR PrintChrA
	JSR PrintChrA

	LDA &B4
IF _MM32_
	STA OWCtlBlock+7
ENDIF
	JSR PrintHex			; print track

IF _MM32_
	JSR OW7F_Execute
	BEQ vfx1			; If no error

	PHA
	LDA #'?'
	JSR PrintChrA
	PLA
	JMP ReportIfDiskFault

.vfx1
ELSE
	JSR RW_Track
ENDIF

	INC &B4				; track
	LDA &B4
	CMP &B5				; more tracks?
	BNE vf5_trackloop

	BIT &C9
	BPL vf6_exit			; If verifying

	\ Save new catalogue
IF NOT(_MM32_)
	JSR MarkDriveAsFormatted
ENDIF

	JSR ClearCatalogue
IF NOT(_MM32_)
	LDA &B5
	CMP #40
	BNE vf7
	LDY #&01
	LDX #&90
	BNE vf8
.vf7
	LDY #&03
	LDX #&20
.vf8
	STX MA+&F07			; Disk size in sectors
	STY MA+&F06
ENDIF
	JSR SaveCatToDisk
.vf6_exit
	JMP PrintNewLine

}
ENDIF



	\\ Reset catalogue pages
.ClearCatalogue
{
IF NOT(_MM32_)
	LDY #&FF
	STY CurrentCat			; Invalid catalogue
	INY
ELSE
	LDY #0
ENDIF
	TYA
.ccatloop
	STA MA+&0E00,Y
	STA MA+&0F00,Y
	INY
	BNE ccatloop

IF _MM32_
	LDY &B5				; Pop number of sectors

.loop2
	LDA #&0A
	CLC
	ADC MA+&0F07
	STA MA+&0F07
	BCC label3

	INC MA+&0F06

.label3	DEY
	BNE loop2
ENDIF
	RTS
}

	\ * Calc. no. of tracks on disk in curdrv *
.TracksOnDisk
{
	JSR LoadCurDrvCat		; Load catalogue
	LDA MA+&0F06			; Size of disk
	AND #&03
	TAX
	LDA MA+&0F07
	LDY #&0A			; 10 sectors/track
	STY &B0
	LDY #&FF			; Calc number of tracks
.trkloop1
	SEC
.trkloop2
	INY
	SBC &B0
	BCS trkloop2
	DEX
	BPL trkloop1
	ADC &B0
	PHA
	TYA
	TAX
	PLA
	BEQ trkex
	INX
.trkex
	RTS
}
;;}

IF _INCLUDE_CMD_FREE_MAP_
.CMD_FREE
	SEC 				; \\\\\\\\\ *FREE
	BCS Label_A7F7
.CMD_MAP
	CLC 				; \\\\\\\\\ *MAP
.Label_A7F7
{
	ROR &C6
	JSR Param_OptionalDriveNo
	JSR LoadCurDrvCat2
	BIT &C6
	BMI Label_A818_free		; If *FREE
	JSR PrintStringSPL
	EQUS "Address :  Length",13	; "Address : Length"
.Label_A818_free
	LDA MA+&0F06
	AND #&03
	STA &C5
	STA &C2
	LDA MA+&0F07
	STA &C4				; wC4=sector count
	SEC
	SBC #&02			; wC1=sector count - 2 (map length)
	STA &C1
	BCS Label_A82F
	DEC &C2
.Label_A82F
	LDA #&02
	STA &BB				; wBB = 0002 (map address)
	LDA #&00			; wBF = 0000
	STA &BC
	STA &BF
	STA &C0
	LDA FilesX8
	AND #&F8
	TAY
	BEQ Label_A86B_nofiles		; If no files
	BNE Label_A856_fileloop_entry	; always
.Label_A845_fileloop
	JSR Sub_A8E2_nextblock
	JSR Y_sub8			; Y -> next file
	LDA &C4
	SEC
	SBC &BB
	LDA &C5
	SBC &BC
	BCC Label_A86B_nofiles
.Label_A856_fileloop_entry
	LDA MA+&0F07,Y			; wC1 = File Start Sec - Map addr
	SEC
	SBC &BB
	STA &C1
	PHP
	LDA MA+&0F06,Y
	AND #&03
	PLP
	SBC &BC
	STA &C2
	BCC Label_A845_fileloop
.Label_A86B_nofiles
	STY &BD
	BIT &C6
	BMI Label_A87A_free		; If *FREE
	LDA &C1				; MAP only
	ORA &C2
	BEQ Label_A87A_free		; If wC1=0
	JSR Map_AddressLength
.Label_A87A_free
	LDA &C1
	CLC
	ADC &BF
	STA &BF
	LDA &C2
	ADC &C0
	STA &C0
	LDY &BD
	BNE Label_A845_fileloop
	BIT &C6
	BPL Label_A8BD_rst		; If *MAP
	TAY
	LDX &BF
	LDA #&F8
	SEC
	SBC FilesX8
	JSR Sub_A90D_freeinfo
	JSR PrintStringSPL
	EQUS "Free",13			; "Free"
	LDA &C4
	SEC
	SBC &BF
	TAX
	LDA &C5
	SBC &C0
	TAY
	LDA FilesX8
	JSR Sub_A90D_freeinfo
	JSR PrintStringSPL
	EQUS "Used",13			; "Used"
	NOP
.Label_A8BD_rst
	RTS
}

.Map_AddressLength
{
	LDA &BC				; Print address (3 dig hex)
	JSR PrintNibbleSPL		; (*MAP only)
	LDA &BB
	JSR PrintHexSPL
	JSR PrintStringSPL
	EQUS "     :  "
	LDA &C2				; Print length (3 dig hex)
	JSR PrintNibbleSPL
	LDA &C1
	JSR PrintHexSPL
	LDA #&0D
	JSR OSASCI
}

.Sub_A8E2_nextblock
{
	LDA MA+&0F06,Y			; wBB = start sec + len
	PHA
	JSR A_rorx4and3
	STA &BC
	PLA
	AND #&03
	CLC
	ADC &BC
	STA &BC
	LDA MA+&0F04,Y
	BEQ Label_A8FA
	LDA #&01
.Label_A8FA
	CLC
	ADC FilesX8,Y
	BCC Label_A902
	INC &BC
.Label_A902
	CLC
	ADC MA+&0F07,Y
	STA &BB
	BCC Label_A90C
	INC &BC
.Label_A90C
	RTS
}

.Sub_A90D_freeinfo
{
	JSR A_rorx3			; *FREE line
	JSR PrintBCDSPL			; A = Number of files
	JSR PrintStringSPL
	EQUS " Files "
	STX &BC				; YX = Number of sectors
	STY &BD
	TYA
	JSR PrintNibbleSPL
	TXA
	JSR PrintHexSPL
	JSR PrintStringSPL
	EQUS " Sectors "
	LDA #&00
	STA &BB
	STA &BE				; !BB = number of sectors * 256
	LDX #&1F			; i.e. !BB = number of bytes
	STX &C1				; Convert to decimal string
	LDX #&09
.Label_A941_loop1
	STA MA+&1000,X			; ?1000 - ?1009 = 0
	DEX
	BPL Label_A941_loop1
.Label_A947_loop2
	ASL &BB				; !BB = !BB * 2
	ROL &BC
	ROL &BD
	ROL &BE
	LDX #&00
	LDY #&09			; A=0
.Label_A953_loop3
	LDA MA+&1000,X
	ROL A
	CMP #&0A
	BCC Label_A95D			; If <10
	SBC #&0A
.Label_A95D
	STA MA+&1000,X
	INX
	DEY
	BPL Label_A953_loop3
	DEC &C1
	BPL Label_A947_loop2
	LDY #&20			; Print decimal string
	LDX #&05
.Label_A96C_loop4
	BNE Label_A970
	LDY #&2C
.Label_A970
	LDA MA+&1000,X
	BNE Label_A97D
	CPY #&2C
	BEQ Label_A97D
	LDA #&20
	BNE Label_A982			; always
.Label_A97D
	LDY #&2C
	CLC
	ADC #&30
.Label_A982
	JSR OSASCI
	CPX #&03
	BNE Label_A98D
	TYA
	JSR OSASCI			; Print " " or ","
.Label_A98D
	DEX
	BPL Label_A96C_loop4
	JSR PrintStringSPL
	EQUS " Bytes "
	NOP
	RTS
}

ENDIF

	\ *********** MMC ERROR CODE ***********

	\\ Report MMC error
	\\ A=MMC response
	\\ If X<>0 print sector/parameter

IF NOT(_MM32_)
errno%=&B0
errflag%=&B1
errptr%=&B8

.ReportMMCErrS
	LDX #&FF
	BNE rmmc

.ReportMMCErr
	LDX #0
.rmmc
{
	LDY #&FF
	STY CurrentCat			; make catalogue invalid
	STA errno%
	STX errflag%
	JSR ResetLEDS
	PLA
	STA errptr%
	PLA
	STA errptr%+1

	LDY #0
	STY MMC_STATE
	STY &100
.rmmc_loop
	INY
	BEQ rmmc_cont
	LDA (errptr%),Y
	STA &100,Y
	BNE rmmc_loop

.rmmc_cont
	LDA errno%
	JSR PrintHex100

	LDA errflag%
	BEQ rmmc_j100
	LDA #'/'
	STA &100,Y
	INY

	LDA par%
	JSR PrintHex100
	LDA par%+1
	JSR PrintHex100
	LDA par%+2
	JSR PrintHex100

.rmmc_j100
	LDA #0
	STA &100,Y
	JMP &100
}
ENDIF

; SFTODO: Slightly wasteful of space here
IF _BP12K_
	SKIPTO MA+&0E00
	; The tube host code can live in this region; it doesn't access our
	; workspace and we won't page in the private 12K bank when calling this.
IF _TUBEHOST_ AND (_BP12K_)
	INCLUDE "TubeHost230.asm"
ENDIF
	SKIPTO MAEND
ENDIF

	\\ *********** MMC HARDWARE CODE **********

datptr%=&BC
sec%=&BE
IF _MM32_
	skipsec%=&C1
	seccount%=&C4
	byteslastsec%=&C6
ELSE
	IF _LARGEFILES
		seccount%=&CE
	ELSE
		seccount%=&C1
	ENDIF
	skipsec%=&C2
	byteslastsec%=&C3
ENDIF
cmdseq%=MA+&1087
par%=MA+&1089

IF NOT(_MM32_)
	\ Include FAT routines here

INCLUDE "FAT.asm"
ENDIF

	\ **** Calculate Check Sum (CRC7) ****
	\ Exit: A=CRC7, X=0, Y=FF
.CalculateCRC7
{
	LDY #(CHECK_CRC7-VID-1)
	LDA #0
.c7loop1
	EOR VID,Y
	ASL A
	LDX #7
.c7loop2
	BCC c7b7z1
	EOR #&12
.c7b7z1
	ASL A
	DEX
	BNE c7loop2
	BCC c7b7z2
	EOR #&12
.c7b7z2
	DEY
	BPL c7loop1
	ORA #&01
	RTS
}

	\ Check CRC7
.CheckCRC7
{
	JSR RememberAXY
	JSR CalculateCRC7
	CMP CHECK_CRC7
	BNE errBadSum
	RTS

.errBadSum
	JSR errBAD
	EQUB &FF
	EQUS "Sum",0
}

	\ Reset CHECK_CRC7
.ResetCRC7
	JSR RememberAXY
	JSR CalculateCRC7
	STA CHECK_CRC7
IF _MM32_
	CLC		; Return code from mm32_chain_open[2]
ENDIF
	RTS

IF NOT(_MM32_)
	\\ *****  Reset MMC_SECTOR  *****
	\\ (MMC_SECTION is the card address of
	\\ Sector 0 of the image file.)

	\\ Default image: BEEB.MMB
.MMC_Sector_Reset
{
	LDX #&A
	JSR CopyDOSFilename

	\\ Search for Image File
	\\ Name at file at fatfilename%
	JSR MMC_GetCIDCRC		; YA=CRC16
	STA MMC_CIDCRC+1
	STY MMC_CIDCRC

	LDA #0
	STA MMC_SECTOR_VALID
	JSR ResetCRC7

	JSR FATLoadRootDirectory
	BCC nofat
	JSR FATSearchRootDirectory
	BCS fileerr
.nofat

	LDA sec%
	STA MMC_SECTOR
	LDA sec%+1
	STA MMC_SECTOR+1
	LDA sec%+2
	STA MMC_SECTOR+2
	LDA #&FF
	STA MMC_SECTOR_VALID

IF _LARGEMMB_
	\\ Read the 8th byte of the disk table which now  indicates it's size:
	\\ 	8th byte	DISK_TABLE_SIZE
	\\	0xA0	     	0x10	(0x01FF disks) (original value of 511)
	\\	0xA1		0x20	(0x03FE disks)
	\\	0xA2		0x30	(0x05FD disks)
	\\      ...
	\\      0xAE		0xF0	(0x1DF1 disks)
	\\      0xAF		0x00	(0x1FF0 disks)
	\\ Any other value default to 511
	\\
	\\ Load the first sector of the disk table
	LDA #&00
	JSR LoadDiskTable

	\\ Process the new MMB length byte
	LDX #&00       	        ; default NUM_CHUNKS for 511 disk MMB
	LDA MA+&0E08		; new MMB size byte A0..AF
	EOR #&A0		; 00..0F
	CMP #&10		; support upto 16x 511 disks
	BCS skip		; skip TAX if out of range
	TAX 			; accept the value
.skip
	INX			; 01..10
	STX NUM_CHUNKS		; save in CRC protected area

	\\ Process the new MMB chunk base byte
	LDX #&00
	LDA MA+&0E09		; new MMB chunk base byte
	CMP NUM_CHUNKS		; compare with the number of chunks
	BCS skip2		; skip TAX if equal or greater
	TAX 			; accept the value
.skip2
	STX CHUNK_BASE
ENDIF
	JMP ResetCRC7

.fileerr
	JSR ReportError
	EQUB &FF
	EQUS "Image not found!",0

}

	\\ Copy filename to search for to fatfilename%
.CopyDOSFilename
{
	LDY #&A
.loop	LDA filemmb,Y
	STA fatfilename%,Y
	DEY
	BPL loop
	RTS

.filemmb
	EQUS "BEEB    MMB"
}

	\\ **** Check drive not write protected ****
.CheckWriteProtect
	LDX CurrentDrv
	LDA DRIVE_INDEX4,X		; Bit 6 set = protected
	ASL A
	BMI errReadOnly
	RTS

	\\ *** Set word &B8 to disk in current drive ***
	\\ Check: C=0 drive loaded with formatted disk
	\\        C=1 drive loaded with unformatted disk
.SetCurrentDiskC
	JSR chkdrv1
	LDA DRIVE_INDEX0,X
	STA &B8
	LDA DRIVE_INDEX4,X
	MASK_DISKNO
	STA &B9
	RTS

	\\ * Check drive loaded with unformatted disk *
.CheckCurDrvUnformatted
	SEC
	BCS chkdrv1

	\\ * Check drive loaded with formatted disk *
.CheckCurDrvFormatted
	CLC
.chkdrv1
{
	LDX CurrentDrv
	LDA DRIVE_INDEX4,X
	BPL errNoDisk			; Bit 7 clear = no disk
IF _LARGEMMB_
	AND #&20			; Bit 5 set = unformatted
ELSE
	AND #&08			; Bit 3 set = unformatted
ENDIF
	BCS chkdrv2
	BNE errNotFormatted		; Bit 3/5 set = unformatted
	RTS
.chkdrv2
	BEQ errFormatted		; Bit 3/5 clear = formatted
	RTS
}					; exit: X=drive no

.errReadOnly
	JSR errDISK
	EQUB &C9
	EQUS "read only",0

.errNoDisk
	JSR ReportError
	EQUB &C7
	EQUS "No disc",0

.errNotFormatted
	JSR errDISK
	EQUB &C7
	EQUS "not formatted",0

.errFormatted
	JSR errDISK
	EQUB &C7
	EQUS "already formatted",0

IF _LARGEMMB_
dmret%=&B2
.calculate_div_mod_511_zp_x
{
	\\ Calculate:
	\\    DD = D DIV 511 (0x1FF)
	\\    DM = D MOD 511 (0x1FF)
	\\
	\\ Algorithm (with thanks to Toby Lobster):
	\\    DD = D DIV 512
	\\    Correct DD by +1 in a small number of cases (0x1FF, 0x3FE-F, 0x5FD-F, 0x7FC-F, ...)
	\\    DM = (D + DD) MOD 512
	\\    See: https://stardot.org.uk/forums/viewtopic.php?p=336092#p336092
	\\
	\\ Implemenentation:
	\\     26 bytes
	\\     37/48 cycles
	\\
	\\ On Exit:
	\\                  A = DD (the quotient)
	\\    dmret%+1/dmret% = DM (the remainder)

	LDA 1, X	; 3  3
	LSR A		; 2  2	; approximate the quotient to D DIV 512
	PHA 		; 3  3	; push the approximation
	BCC done	; 3  2	; branch if approximation is accurate (D = xxx0 xxxx xxxx)
	ADC 0, X	;    3	; after this add, C = 1 if correction is needed
	PLA    		;    4	; pop the approximation
	ADC #0		;    2	; correct the it by +1
	PHA 		;    3	; push the approximation
.done
	ADC 0, X	; 3  3	; A = quotient, now calculate the remainder
	STA dmret%	; 3  3
	LDA 1, X	; 3  3
	ADC #0 		; 2  2
	AND #1		; 2  2
	STA dmret%+1	; 3  3
	PLA 		; 4  4
	CLC		; 2  2
	ADC CHUNK_BASE	; 4  4
	CMP NUM_CHUNKS	; 4  4
	RTS		; 6  6
			;-- --
}			;47 58
ENDIF

	\\ **** Calc first MMC sector of disk ****
	\\ sec% = MMC_SECTOR + 32 + drvidx * 800
	\\ Call after MMC_BEGIN
	\\
	\\ On Exit
	\\    A, X, Y corrupted

	\\ Current drive
.DiskStart
	JSR CheckCurDrvFormatted	; X=drive
.DiskStartX
IF _LARGEMMB_
{
	\\ Start at sector 0
	LDY #0
	STY sec%
	STY sec%+1
	STY sec%+2

	\\ If X < 0 the calculate the address of disk 0; this is just used by DRECAT
	TXA
	BMI skip_drive

	\\ Set sec% to the drive number in the drive table
	LDA DRIVE_INDEX0,X
	STA sec%
	LDA DRIVE_INDEX4,X
	MASK_DISKNO
	STA sec%+1

.skip_drive

	\\ Calculate:
	\\     A      = DrvNo (sec%) DIV 511
	\\     dmret% = DrvNo (sec%) MOD 511
	LDX #sec%
	JSR calculate_div_mod_511_zp_x

	PHA	; A = chunk

	\\ Multiply drive index by 800
	\\ Note: these are 256b sectors
	\\ 800 = 32 + 256 + 256 + 256

	\\ sec% = drvindx
	LDA dmret%+1
	STA sec%+1
	LDA dmret%
	\\ Loop1: sec% *= 32
	LDY #5
.dsxloop1
	ASL A
	ROL sec%+1
	ROL sec%+2
	DEY
	BNE dsxloop1
	STA sec%
	\\ Loop2: sec% += 3 * 256 * drvidx
	LDY #3
.dsxloop2
	LDA dmret%
	CLC \\ Don't thing this is needed, as sec% cannot overflow
	ADC sec%+1
	STA sec%+1
	LDA dmret%+1
	ADC sec%+2
	STA sec%+2
	DEY
	BNE dsxloop2

	PLA
	TAY	; y = chunk

	\\ sec% += MMC_SECTOR + Y * 0x63D00
	\\ uses only A and Y, and no zp
	JSR add_chunk_sector

	\\ Add 32
	LDA #&20
	CLC
	ADC sec%
	STA sec%
	BCC done
	INC sec%+1
	BNE done
	INC sec%+2
.done
	RTS
}

ELSE

	\\ 58 bytes

	LDA DRIVE_INDEX4,X
	ROR A				; C = bit 0
	LDA DRIVE_INDEX0,X

	\\ A=drvidx, C=bit 8
	\\ S=I*512+I*256+I*32
.DiskStartA
	PHP 				;\ 1
	TAX
	LDA #0
	STA sec%
	ROL A
	PHA 				;\ 2
	STA sec%+2
	TXA
	ASL A
	ROL sec%+2			; C=0
	STA sec%+1
	TXA
	ADC sec%+1
	STA sec%+1
	PLA				;\ 2
	ADC #0				; C=0
	ADC sec%+2
	STA sec%+2
	ROR sec%
	TXA
	PLP				;\ 1
	ROR A
	ROR sec%
	LSR A
	ROR sec%
	LSR A
	ROR sec%
	ADC sec%+1
	STA sec%+1
	LDA sec%+2
	ADC #0
	STA sec%+2

	\\ add offset + 32
	SEC
	LDA sec%
	ORA #&1F			; 32
	ADC MMC_SECTOR
	STA sec%
	LDA sec%+1
	ADC MMC_SECTOR+1
	STA sec%+1
	LDA sec%+2
	ADC MMC_SECTOR+2
	STA sec%+2
	RTS

ENDIF

	\\ **** Initialise VARS for MMC R/W ****
	\\ Call only after MMC_BEGIN
	\\ Note: Values in BC-C5 copied to 1090-1099
	\\ Also checks disk loaded/formatted
.CalcRWVars
{
	JSR DiskStart

	\\ add start sector on disk
	CLC
	LDA MA+&1097
	ADC sec%
	STA sec%
	LDA MA+&1096
	AND #3
	PHA
	ADC sec%+1
	STA sec%+1
	BCC cvskip
	INC sec%+2

	\\ calc sector count
.cvskip
	LDA MA+&1095
	STA seccount%
	LDA MA+&1096			; mixed byte
	LSR A
	LSR A
	LSR A
	LSR A
	AND #3
IF _LARGEFILES
	STA seccount%+1
ELSE
	BNE errBlockSize
ENDIF
	LDA MA+&1094
	STA byteslastsec%
	BEQ cvskip2
	INC seccount%
IF _LARGEFILES
	BNE cvskip2
	INC seccount%+1
ELSE
	BEQ errBlockSize
ENDIF

	\\ check for overflow
.cvskip2
	CLC
	LDA MA+&1097
	ADC seccount%
	TAX
	PLA
IF _LARGEFILES
	ADC seccount%+1
ELSE
	ADC #0
ENDIF
	CMP #3
	BCC cvnoof
	BNE errOverflow
	CPX #&21
	BCS errOverflow
.cvnoof
	RTS

IF NOT(_LARGEFILES)
.errBlockSize
	JSR ReportError
	EQUB &FF
	EQUS "Block too big",0
ENDIF

.errOverflow
	JSR errDISK
	EQUB &FF
	EQUS "overflow",0
}
ENDIF ;NOT(_MM32_)

IF _SWRAM_
	\ Check for exception - don't allow loading to memory >=&8000
	\ i.e. don't overwrite filing system in SWRAM
.CheckForException
{
IF _MM32_
	b=&BC
ELSE
	b=MA+&1090
ENDIF

	LDA MA+&1074
	AND MA+&1075
	ORA TubePresentIf0
	EOR #&FF
	BNE noexception			; If Tube Xfer

IF _MM32_
	LDA b+6
	AND #&30
	BNE errException		; If len >= &10000
	LDA b+5
	BMI errException		; If len >= &8000
ENDIF

	CLC
	LDA b
	ADC b+4
	TAX
	LDA b+1
	BMI errException		; Start >= &8000
	ADC b+5
	BPL noexception			; OK if start+len <= &8000, i.e. start+len-1 < &8000
	ASL A
	BNE errException		; if start + len > &8000
	TXA
	BNE errException		; if start + len <> &8000
.noexception
	RTS
.errException
	JSR ReportError
	EQUB &FF
	EQUS "Not allowed",0
}
ENDIF

	\\ **** Check if loaded catalogue is that
	\\ of the current drive, if not load it ****
.CheckCurDrvCat
	LDA CurrentCat
	CMP CurrentDrv
	BNE LoadCurDrvCat
	RTS

.Set_CurDirDrv_ToDefaults_and_load
	JSR Set_CurDirDrv_ToDefaults
.LoadCurDrvCat2
	JSR RememberAXY

	\\ **** Load catalogue of current drive ****
.LoadCurDrvCat
IF _MM32_
	LDA #&53
	BNE exec_cat_rw
ELSE
	JSR MMC_BEGIN1
	JSR DiskStart
	JSR MMC_ReadCatalogue

.rwcatexit
	LDA CurrentDrv
	STA CurrentCat
	JMP MMC_END
ENDIF

	\\ **** Save catalogue of current drive ****
.SaveCatToDisk
	LDA MA+&0F04			; Increment Cycle Number
	CLC
	SED
	ADC #&01
	STA MA+&0F04
	CLD

IF _MM32_
	LDA #&4B
	;BNE exec_cat_rw
ELSE
	JSR MMC_BEGIN1
	JSR DiskStart
	JSR CheckWriteProtect
	JSR MMC_WriteCatalogue
	JMP rwcatexit
ENDIF


IF _MM32_
\\ Read/Write catalogue
\\ On entry: A=FDC command
.exec_cat_rw
{
	PHA
	LDA CurrentDrv
	LDX #0

.loop
	STA OWCtlBlock,X
	LDA datax,X
	INX
	CPX #10
	BNE loop

	PLA
	STA OWCtlBlock+6

	JSR OW7F_Execute
	JSR ReportIfDiskFault

	LDA CurrentDrv
	STA CurrentCat
	RTS

.datax	EQUB &00, MP+&0E, &FF, &FF, &03, &53, &00, &00, &22
}


\\ Call OSWORD &7F
\\ On exit A=FDC result, Z=1 if A=0
.OW7F_Execute
{
	LDA &B0
	PHA
	LDA &B1
	PHA

	LDA #LO(OWCtlBlock)
	STA &B0
	LDA #HI(OWCtlBlock)
	STA &B1
	JSR Osword7F_8271_Emulation

	PLA
	STA &B1
	PLA
	STA &B0

	LDX OWCtlBlock+5
	LDA OWCtlBlock+7,X		; A = result
	RTS
}


\\ Report 'disk fault'
\\ On entry: A=FDC result, Z=1 if A=0
.ReportIfDiskFault
{
	trk = &CA
	sec = &CB

	BNE l0

	RTS

.l0	CMP #Rdrvnotrdy
	BNE l1

	JSR ReportErrorCB
	EQUB &C7
	EQUS "Drive empty",0

.l1	CMP #Rwritepro
	BNE l2

	JSR errDISK
	EQUB &C9
	EQUS "read only",0

	\ Assume sector not found
.l2
	\ Get track & sector
	LDA CurrentDrv
	STA OWCtlBlock
	LDX #6

.loop1
	LDA spec,X
	STA OWCtlBlock+1,X
	DEX
	BPL loop1

	JSR OW7F_Execute
	PHA

	LDA CurrentDrv
	AND #2			; Side
	ASL A
	ASL A			; C=0, A=0 OR 8
	ADC #&12
	STA OWCtlBlock+7
	JSR OW7F_Execute
	PHA

	JSR errDISK
	BRK
	NOP

	JSR ErrCONTINUE
	EQUS &C7, "sector not found at "
	NOP

	LDA #':'
	JSR mm32_PrintChr100

	LDA CurrentDrv
	JSR PrintNib100

	LDA #' '
	JSR mm32_PrintChr100

	PLA				; Track
	JSR PrintHex100

	LDA #'/'
	JSR mm32_PrintChr100

	PLA				; Sector
	JSR PrintHex100

	JSR ErrCONTINUE	;BREAK
	EQUB &C7, 0
	NOP

\ Read registers to get track/sector

.spec
	EQUB &00,&00,&00,&00,&01,&7D,&06	; Read special register (SCAN SECTOR)
}
ENDIF


IF NOT(_MM32_)
	\ **** Read / Write 'track' ****
	\ ?&C9 : -ve = write, +ve = read
	\ ?&B4 : track number
	\ (Used by CMD_VERIFY / CMD_FORM)
.RW_Track
{
	LDA &B4
	BNE rwtrk1
	LDX CurrentDrv
	JSR DiskStartX

.rwtrk1
	LDA #5
	STA &B6
.rwtrk2_loop
	BIT &C9
	BMI rwtrk3
	JSR MMC_ReadCatalogue		; verify
	JMP rwtrk4
.rwtrk3
	JSR MMC_WriteCatalogue		; format
.rwtrk4
	INC sec%
	INC sec%
	BNE rwtrk5
	INC sec%+1
	BNE rwtrk5
	INC sec%+2
.rwtrk5
	DEC &B6
	BNE rwtrk2_loop
	RTS
}


IF _LARGEMMB_
	\\ **** Calc disk table sec & offset ****
	\\ Entry: D = Disk no (B8/B9)
	\\ Exit: (B0) = &E00 + (D + 1) x 16
	\\     : A=Table Sector Code
	\\ Note; D = 511 not valid
	\\
	\\ Sector Code is in 512b units = 32 disks
	\\
	\\    DD  = (D DIV 511)	     (4 bits)
	\\    DM  = (D MOD 511)       (9 bits)
	\\    DM' = DM+1
	\\    A   = (DD << 4) | (DM' >> 5)
	\\    B1  = (DM' AND &10) ? &0F : &0E
	\\    B0  = (DM' AND &0F) << 4
	\\
	\\ Needs to preserve B8/9

.GetIndex
{

	LDX #&B8
	JSR calculate_div_mod_511_zp_x

	\\ Calculate DD << 4 into tmp (B0)
	  		;  0 :  0  0  0  0 D3 D3 D1 D0
	DO_ASLA_X4	;  0 : D3 D2 D1 D0  0  0  0  0

	\\ Calculate DM' = DM + 1 (9 bits) into C and A
	LDY dmret%		;
	INY             ;
	STA &B0		;      D3 D2 D1 D0  0  0  0  0 ==> tmp
	ROR dmret%+1    ; C = MSN (DM)
	TYA             ;
	BNE skip
	SEC
.skip			; M8 : M7 M6 M5 M4 M3 M2 M1 M0
	\\ A =  (DD << 4) | (DM' >> 5)
	ROL A           ; M7 : M6 M5 M4 M3 M2 M1 M0 M8
	ROL A           ; M6 : M5 M4 M3 M2 M1 M0 M8 M7
	ROL A           ; M5 : M4 M3 M2 M1 M0 M8 M7 M6
	ROL A           ; M4 : M3 M2 M1 M0 M8 M7 M6 M5
	PHA 		; M4 : M3 M2 M1 M0 M8 M7 M6 M5
	AND #&0F	; M4 :  0  0  0  0 M8 M7 M6 M5
	ORA &B0         ; M4 : D3 D3 D1 D0 M8 M7 M6 M5 <== tmp
	TAY		; M4 : D3 D3 D1 D0 M8 M7 M6 M5 ==> Final A (saved in Y)
	PLA             ; M4 : M3 M2 M1 M0 M8 M7 M6 M5
	AND #&F0        ; M4 : M3 M2 M1 M0  0  0  0  0
	STA &B0         ; M4 : M3 M2 M1 M0  0  0  0  0 ==> Final B0
	LDA #&00        ; M4 :  0  0  0  0  0  0  0  0
	ADC #MP+&0E     ;  0 :  0  0  0  0  1  1  1 M4
	STA &B1		;  0 :  0  0  0  0  1  1  1 M4 ==> Final B1
  	TYA
	\\ A  = (DD << 4) | (DM' >> 5)
	\\ B1 = (DM' AND &10) ? &0F : 0E
	\\ B0 = (DM' AND &0F) << 4
	RTS
}
ELSE
	\\ **** Calc disk table sec & offset ****
	\\ Entry: D = Disk no (B8)
	\\ Exit: (B0) = &E00 + (D + 1) x 16
	\\     : A=Table Sector Code
	\\ Note; D = 511 not valid
.GetIndex
{
	LDA &B9
	ROR A
	LDY &B8
	INY
	TYA
	BNE gix1
	SEC

.gix1
	ROL A
	ROL A
	ROL A
	ROL A
	ROL A
	PHA				;\ 1
	AND #&1F
	TAY
	PLA				;\ 1
	ROR A
	AND #&F0

	STA &B0
	TYA
	AND #&01
	ORA #MP+&0E
	STA &B1

	TYA				; A = table sector code
	AND #&FE
	ORA #&80
	RTS				; X unchanged
}

ENDIF

	\\ Return status of disk in current drive
.GetDriveStatus
	CLC				; check loaded with formatted disk
.GetDriveStatusC
	JSR SetCurrentDiskC


	\\ &B8 = disk no
	\\ On exit; A=disk status byte
	\\ from Disk Table
	\\ &B0 points to location in table (cat)
	\\ Z & N set on value of A
	\\ Disk Table sector
	\\ for disk in cat area
.GetDiskStatus
{
	JSR GetIndex
	JSR CheckDiskTable
	LDY #15
	LDA (&B0),Y
	CMP #&FF
	BEQ ErrNotValid
	TAX				; reset flags
	RTS
	\\ Type: 00=RO, 0F=RW, F0=Unformatted, FF=Invalid
	\\ Z=1=RO, N=1=Unformatted else RW

.ErrNotValid
	JSR errDISK
	EQUB &C7
	EQUS "number not valid",0
}


	\\ **** If disk in any drive, unload it ****
	\\ Word &B8=diskno (X,Y preserved)
	\\ Doesn't check/update CRC7
.UnloadDisk
{
	TXA
	PHA
	LDX #3
.uldloop
	LDA DRIVE_INDEX0,X
	CMP &B8
	BNE uldskip
	LDA DRIVE_INDEX4,X
	MASK_DISKNO
	CMP &B9
	BNE uldskip
	STA DRIVE_INDEX4,X		; Reset bit 7
.uldskip
	DEX
	BPL uldloop
	PLA				; Restore X
	TAX
	RTS
}

	\\ **** Load current drive with disk ****
	\\ Word &B8 = Disc number
.LoadDrive
	LDX CurrentDrv
.LoadDriveX
{
	TXA
	PHA
	LDA #&C0
	STA &B7
	JSR GetDiskStatus
	BEQ ldiskro			; 00 = read only
	BPL ldiskrw			; 0F = read/write
	\CMP #&F0			; F0 = unformatted
	\BNE [.notvaliderr]		; Disk number not valid
IF _LARGEMMB_
	\\ b7 = loaded, b6 = writeprot, b5=unformatted
	LDA #&E0
ELSE
	\\ b7 = loaded, b6 = writeprot, b3=unformatted
	LDA #&C8
ENDIF
	BNE ldisknf			; not formatted
.ldiskrw
	LDA #&80
.ldisknf
	STA &B7
.ldiskro
	JSR CheckCRC7
	\ Make sure disk is not in another drive
	JSR UnloadDisk
	PLA
	TAX
	LDA &B8
	STA DRIVE_INDEX0,X
	LDA &B9
	ORA &B7	; Loaded
	STA DRIVE_INDEX4,X
	JMP ResetCRC7
}

	\\ **** Calculate disk table sector ****
.DiskTableSec
IF _LARGEMMB_
	\\ DiskTableIndex (di) is a 8-bit value
	\\
	\\
	\\ sec% = MMC_SECTOR + (di >> 4) * 0x63D00 + (di & 0x0F) * 2

	\\ A = (di & 0x0F)
	\\ Y = (di >> 4)

	LDA DiskTableIndex
	PHA
	LSR A
	LSR A
	LSR A
	LSR A
	TAY
	PLA
	AND #&0F
	ASL A

	\\ sec% = (di & 0x0F) * 2
	STA sec%+0
	LDA #0
	STA sec%+1
	STA sec%+2
	\\ Fall through to...

.add_chunk_sector
{
	TYA
	BEQ done
	\\ sec% += chunk * 0x63D00 by repeated addition
	CLC
.loop
	\\ Loop can never overflow, so do CLC out side of loop
	LDA #&3D
	ADC sec%+1
	STA sec%+1
	LDA #&06
	ADC sec%+2
	STA sec%+2
	DEY
	BNE loop
.done
	\\ Fall through to...
}

	\\ sec% += MMC_SECTOR
.add_mmc_sector
	LDA MMC_SECTOR
	CLC
	ADC sec%
	STA sec%
	LDA MMC_SECTOR+1
	ADC sec%+1
	STA sec%+1
	LDA MMC_SECTOR+2
	ADC sec%+2
	STA sec%+2
ELSE
	\\ A=sector code (sector + &80)
	\\
	\\ Disk table at the start of the MMB File
	\\ 512 * 16 = 8192 bytes = 32 sectors
	\\
	\\ A is in units of 256b sectors
	\\
	\\ sec% = MMC_SECTOR + (A & &7E)
	AND #&7E
	CLC
	ADC MMC_SECTOR
	STA sec%
	LDA MMC_SECTOR+1
	ADC #0
	STA sec%+1
	LDA MMC_SECTOR+2
	ADC #0
	STA sec%+2
ENDIF
.ldtloaded
	RTS

IF _LARGEMMB_
.CheckDiskTable
	\\ 10xxxxxx in CurrentCat indicates DiskTableIndex valid
	BIT CurrentCat
	BPL LoadDiskTable
	BVS LoadDiskTable
	CMP DiskTableIndex
	BEQ ldtloaded

	\\ A=sector code
.LoadDiskTable
	STA DiskTableIndex
	LDA #&80
	STA CurrentCat
	JSR DiskTableSec
	JMP MMC_ReadCatalogue

.SaveDiskTable
	JSR DiskTableSec
	JMP MMC_WriteCatalogue

ELSE

	\\ A=sector code (sector or &80)
.CheckDiskTable
	CMP CurrentCat
	BEQ ldtloaded

	\\ A=sector code
.LoadDiskTable
	STA CurrentCat
	JSR DiskTableSec
	JMP MMC_ReadCatalogue

.SaveDiskTable
	LDA CurrentCat
	JSR DiskTableSec
	JMP MMC_WriteCatalogue
ENDIF


	\\ GetDisk, returns name of disks
	\\ in DiskTable (used by *DCAT)
	\\ for disks with no's in range
	\\ On exit C clear if disk found
	\\ and A contains disk status

	\\ Set up and get first disk
	\\ Word &B8=first disk no
	\\ If ?&B7=0, skip unformatted disks

	\\ Return ALL disks
.GetDiskFirstAllX
	STX gdopt%
.GetDiskFirstAll
	LDA #0
	STA gddiskno%
	STA gddiskno%+1

.GetDiskFirst
	JSR GetIndex
	STA gdsec%
	JSR CheckDiskTable
	JMP gdfirst

\\ Enter with Y = the ZP address of the disk count
.print_pluralized_disks
{
	TYA
	PHA
	LDX #0
	JSR DecNo_Print_zp_y
	JSR PrintString
	EQUS " disc"
	NOP
	PLA
	TAX
	LDA 1, X
	BNE NotOne
	DEC 0, X
	BEQ nos
.NotOne
	LDA #&73			; ASC("s")
	JSR PrintChrA
.nos
	JMP PrintString
}
.gdnextloop
	CMP #&FF
	BEQ gdfin
	BIT gdopt%			; Return unformatted disks?
	BMI gdfrmt			; If yes

	\\ Get next disk
.GetDiskNext
	JSR CheckESCAPE
	CLC
	LDA gdptr%
	ADC #16
	STA gdptr%
	BNE gdx1
	LDA gdptr%+1
	EOR #1
	STA gdptr%+1
	ROR A
	BCS gdx1
IF _LARGEMMB_
	\\ Compare gdsec against num_chunks<<4 - 1
	LDA NUM_CHUNKS	 	 	; 01,02,...,0F,10
	DO_ASLA_X4			; 10,20,...,F0,00
	CLC
	SBC gdsec%			; subtract gdsec+1
	BEQ gdfin
	INC gdsec%
	LDA gdsec%
	JSR CheckDiskTable
	LDA gdsec%			; Have we moved to a new chunk?
	AND #&0F
	BNE gdx1			; No
	LDA #&10
	STA gdptr%			; Skip the blank entry
ELSE
	LDA gdsec%
	ADC #2
	CMP #&A0			; (&80 OR 32)
	BEQ gdfin
	STA gdsec%
	JSR CheckDiskTable
ENDIF
.gdx1
	INC gddiskno%
	BNE gdx50
	INC gddiskno%+1
.gdx50
\\ Don't maintain a shadow decimal version as it's inefficient and unnecessary
\\ It also prevents GetDiskNext being used by DRECAT due to a ZP conflict

.gdfirst
	LDY #&F
	LDA (gdptr%),Y
	BMI gdnextloop			; If invalid / unformatted

	\ Disk found
.gdfrmt
	CLC
	RTS

	\ No more disks
.gdfin
	LDA #&FF
	STA gddiskno%+1
	SEC
	RTS
ENDIF ;NOT(_MM32_)


IF _UTILS_ OR NOT(_MM32_)
\\ 16-bit BCD increment on ZP,X and ZP+1,X
.bcd_inc16_zp_x
{
	SED
	CLC
	LDA 0, X
	ADC #1
	STA 0, X
	LDA 1, X
	ADC #0
	STA 1, X
	CLD
	RTS
}
ENDIF

\\ Include Low Level MMC Code here

IF _DEVICE_="U"
	_TURBOMMC=FALSE
   _VIA_BASE=?&FE60
	INCLUDE "MMC_UserPort.asm"
ELIF _DEVICE_="U2"
	_TURBOMMC=FALSE
   _VIA_BASE=?&FE80
	INCLUDE "MMC_UserPort.asm"
ELIF _DEVICE_="U3"
	_TURBOMMC=FALSE
   _VIA_BASE=?&FEA0
	INCLUDE "MMC_UserPort.asm"
ELIF _DEVICE_="T"
	_TURBOMMC=TRUE
   _VIA_BASE=?&FE60
	INCLUDE "MMC_UserPort.asm"
ELIF _DEVICE_="T2"
	_TURBOMMC=TRUE
   _VIA_BASE=?&FE80
	INCLUDE "MMC_UserPort.asm"
ELIF _DEVICE_="T3"
	_TURBOMMC=TRUE
   _VIA_BASE=?&FEA0
	INCLUDE "MMC_UserPort.asm"
ELIF _DEVICE_="M"
	INCLUDE "MMC_MemoryMapped.asm"
ELIF _DEVICE_="E"
	INCLUDE "MMC_ElkPlus1.asm"
ELIF _DEVICE_="P"
   _VIA_BASE=&FE60
	INCLUDE "MMC_BeebPrinter.asm"
ELIF _DEVICE_="G"
    IF _USE_MGC_SHIFTREG
        INCLUDE "MMC_MGCII_ShiftReg.asm"
    ELSE
        INCLUDE "MMC_MGCII_BitBang.asm"
    ENDIF
ELIF _DEVICE_="1"
	INCLUDE "MMC_Pi1MHz.asm"
ENDIF

.errWrite2
	TYA
IF _MM32_
	JSR mm32_MMC_error
	EQUS "Write response fault ",0
ELSE
	JSR ReportMMCErrS
	EQUB &C5
	EQUS "MMC Write response fault "
	BRK
ENDIF

	\\ Include high level MMC code here

INCLUDE "MMC.asm"



IF NOT(_MM32_)
	\\ *DRECAT
	\\ Refresh disk table with disc titles

	\ load first sector of disk table
IF _INCLUDE_CMD_DRECAT_
.CMD_DRECAT
{
	\ error if any params are specified
	JSR Param_SyntaxErrorIfNotNull

	LDX #&FF
	JSR DiskStartX

	\ set read16sec% to first disk
	LDX #2
.drc_loop1
	LDA sec%,X
	STA read16sec%,X
	DEX
	BPL drc_loop1

	; GetDisk returns unformatted disks
	JSR GetDiskFirstAllX

.drc_loop2
	\ read disc title
	JSR MMC_ReadDiscTitle

	\ copy title to table
	LDY #&0B
.drc_loop4
	LDA read16str%,Y
	STA (gdptr%),Y
	DEY
	BPL drc_loop4

	\ Test if we are at the end of the disk table sector
	LDA gdptr%+1
	CMP #MP+&0F
	BCC skipsave
	LDA gdptr%
	CMP #&F0
	BCC skipsave
	JSR SaveDiskTable

IF _LARGEMMB_
	\ Test if we are at the end of the disk table chunk
	LDA gdsec%
	AND #&0F			; Are we at the end of a chunk?
	CMP #&0F
	BCC skipsave			; No
	\ C=1, so add 3F
	LDA #&3F			; Add 340 rather than 320 sectors
	EQUB &2C			; skip the next 2-byte instruction (BIT abs)
ENDIF
.skipsave
	\ read16sec% += 0x320 or 0x340 depending on if we are at a chunk boundary
	LDA #&20
	ADC read16sec%
	STA read16sec%
	LDA #&03
	ADC read16sec%+1
	STA read16sec%+1
	BCC drc_label3
	INC read16sec%+2
.drc_label3

	JSR GetDiskNext
	BCC drc_loop2

	\ Force a save at the end, in case we are part way through the last sector
	JMP SaveDiskTable
}
ENDIF

	\\ *** Set up the string to be compared ***
	\\ The match string is at (txtptr%)+Y
	\\ Max length=12 chrs (but allow 0 terminator)
dmStr%=MA+&1000		; location of string
dmLen%=MA+&100D		; length of string
dmAmbig%=MA+&100E	; string terminated with *

.DMatchInit
{
	LDX #0
	STX dmAmbig%

	CLC
	JSR GSINIT
	BEQ dmiExit			; null string

.dmiLoop
	JSR GSREAD
	BCS dmiExit
	CMP #'*'			; ASC("*")
	BEQ dmiStar			; if ="*"

	\ UCASE
	CMP #&61			; ASC("a")
	BCC dmiUcase			; if <"a"
	CMP #&7B			;ASC("z")+1
	BCS dmiUcase			; if >"z"
	EOR #&20
.dmiUcase
	STA dmStr%,X

	INX
	CPX #12
	BNE dmiLoop

	\ Make sure at end of string
.dmiEnd
	JSR GSREAD
	BCC ErrBadString		; If not end of string
.dmiExit
	CMP #&0D
	BNE dmiSyntax			; Syntax?

	LDA #0
	STA dmStr%,X
	STX dmLen%
	RTS

	\ Wildcard found, must be end of string
.dmiStar
	STA dmAmbig%
	BEQ dmiEnd			; always

.ErrBadString
	JSR ReportError
	EQUB &FF
	EQUS "Bad string",0

.dmiSyntax
	JMP errSYNTAX
}

	\\ *** Perform string match ****
	\\ String at (gdptr%)+Y
	\\ C=0 if matched
.DMatch
{
	LDY #0
	LDX dmLen%
	BEQ dmatend

.dmatlp
	LDA (gdptr%),Y
	BEQ dmatnomatch

	CMP #&61			; ASC("a")
	BCC dmnotlc
	CMP #&7B			; ASC("z")+1
	BCS dmnotlc
	EOR #&20
.dmnotlc
	CMP dmStr%,Y
	BNE dmatnomatch

	INY
	DEX
	BNE dmatlp
.dmatend
	LDA (gdptr%),Y
	BEQ dmmatyes
	LDA dmLen%
	CMP #12
	BEQ dmmatyes
	LDA dmAmbig%
	BEQ dmatnomatch
.dmmatyes
	CLC
	RTS
.dmatnomatch
	SEC
	RTS
}

	\\ **** Print disk no & title ****
.PrintDCat
{
	BCS pdcnospc
	LDA #&20
	JSR PrintChrA

.pdcnospc
	\\ Covert gddisk% to BCD in decno% for printing
	JSR DecNo_BIN2BCD
	LDX #&20
	JSR DecNo_Print
	LDA #&20
	JSR PrintChrA

	LDY #15
	LDA (gdptr%),Y
	BMI pdcnotform

	LDY #0
.pdcloop
	LDA (gdptr%),Y
	BEQ pdcspc
	JSR PrintChrA
	INY
	CPY #12
	BNE pdcloop
.pdcspc
	LDA #&20
.pdcspclp
	JSR PrintChrA
	INY
	CPY #13
	BNE pdcspclp
	TAX
	LDY #15
	LDA (gdptr%),Y
	BNE pdcnoprot
	LDX #&50			; ASC("P")
.pdcnoprot
	TXA
	JMP PrintChrA

.pdcnotform
	LDY #13
	JSR prt_Yspaces
	LDA #&55
	JMP PrintChrA
}

	\ Print 4 dig disk number in Drive X
.PrtDiskNo
	JSR RememberXYonly
	LDA DRIVE_INDEX0,X
	STA &B8
	LDA DRIVE_INDEX4,X
	MASK_DISKNO
	STA &B9
	JSR DecNo_BIN2BCD
	LDX #0

	\ Print 4 dig decno% padded with chr X
.DecNo_Print
	LDY #decno%

.DecNo_Print_zp_y
	LDA 0, Y
	PHA
	LDA 1, Y
	LDY #4
	JSR PrintDec
	PLA

.PrintDec
{
	PHA
	LSR A
	LSR A
	LSR A
	LSR A
	JSR pdec1
	PLA
.pdec1
	AND #&F
	BEQ pdec2
	LDX #&30
	CLC
	ADC #&30
	JMP PrintChrA
.pdec2
	DEY
	BNE pdec3
	LDX #&30
.pdec3
	TXA
	JMP PrintChrA
}
ENDIF ;NOT(_MM32_)


IF _INCLUDE_CMD_DABOUT_
	\\ *DABOUT -  PRINT INFO STRING from ROM header
.CMD_DABOUT
{
	LDX #&00
.loop
	LDA title, X
	BNE print
	LDA #&0D
.print
	JSR OSASCI
	INX
	CPX #header_end - title
	BNE loop
	RTS
}
ENDIF


IF NOT(_MM32_)
IF _INCLUDE_CMD_DBASE_
	\\ *DBASE <dno>
	\\ Takes effect on next Ctrl-BREAK
.CMD_DBASE
	JSR GSINIT_A
	BEQ info
	JSR Param_ReadNum
	BCS invalid
	BNE invalid
	CPX NUM_CHUNKS
	BCS invalid
	STX CHUNK_BASE
	JSR ResetCRC7
	LDA #0
	JSR LoadDiskTable
	LDA CHUNK_BASE
	STA MA+&E09
	JMP SaveDiskTable
.info
	JSR PrintString
	EQUS "MMB Base: "
	LDA CHUNK_BASE
	JSR printdec
	JSR PrintString
	EQUS "MMB Size: "
	LDA NUM_CHUNKS
.printdec
	JSR BinaryToBCD
	LDX #0
	LDY #2
	JSR PrintDec
	JMP PrintNewLine
.invalid
	JMP errBADOPTION
ENDIF

	\\ *DBOOT <dno>/<dsp>
.CMD_DBOOT
	JSR Param_SyntaxErrorIfNull
	LDA #0
	STA CurrentDrv
	JSR Param_Disk			; CurrentDrv=drive / B8=disk no.
	JSR LoadDrive
	LDA #&00
	JMP initMMFS

	\\ *DIN (<drive>)
	\\ Load drive
.CMD_DIN
	JSR Param_DriveAndDisk
	\stx CurrentDrv
	JMP LoadDrive 	; CA

	\\ *DOUT (<drive>)
	\\ Unload drive
	\\ Note: No error if drive not loaded
.CMD_DOUT
	JSR Param_OptionalDriveNo
.unloaddrive
	JSR CheckCRC7
	LDX CurrentDrv
	TXA
	STA DRIVE_INDEX4,X
	JMP ResetCRC7

	\\ *DCAT ((<f.dno>) <t.dno>) (<adsp>)
dcEnd%=&A8	; last disk in range
dcCount%=&AA	; number of disks found

IF _INCLUDE_CMD_DCAT_
.CMD_DCAT
{
	LDA #0
	STA gdopt%			; GetDisk excludes unformatted disks
	STA dcCount%
	STA dcCount%+1

	JSR Param_ReadNum		; rn% @ B0
	BCS dc_1			; not number
	STX dcEnd%
	STX gddiskno%
	STA dcEnd%+1
	STA gddiskno%+1

	JSR Param_ReadNum		; rn% @ B0
	BCS dc_2			; not number
	STX dcEnd%
	STA dcEnd%+1

	CPX gddiskno%
	SBC gddiskno%+1
	BPL dc_3

.badrange
	JSR ReportError
	EQUB &FF
	EQUS "Bad range",0

.dc_1
	LDX #&FE
	STX dcEnd%
	INX
	STX dcEnd%+1

.dc_2
	LDA #0
	STA gddiskno%
	STA gddiskno%+1

.dc_3
	INC dcEnd%
	BNE dc_4
	INC dcEnd%+1

.dc_4
	JSR DMatchInit
	JSR GetDiskFirst

	LDX #0
	LDA dmLen%
	BNE dclp
	DEX
	STX dmAmbig%

.dclp
	LDA gddiskno%+1
	BMI dcfin

	LDA gddiskno%
	CMP dcEnd%
	LDA gddiskno%+1
	SBC dcEnd%+1
	BCS dcfin

	JSR DMatch
	BCS dcnxt
	JSR PrintDCat

	LDX #dcCount%
	JSR bcd_inc16_zp_x
.dcnxt
	JSR GetDiskNext
	JMP dclp

.dcfin
	LDA #&86
	JSR OSBYTE			; get cursor pos
	CPX #0
	BEQ dcEven
	JSR PrintNewLine
.dcEven
	LDY #dcCount%
	JSR print_pluralized_disks
	EQUS " found"
	NOP
	JMP PrintNewLine
}
ENDIF

	\\ *DFREE
dfFree%=&A8	; number of unformatted disks
dfTotal%=&AA	; total number of disks

IF _INCLUDE_CMD_DFREE_
.CMD_DFREE
{
	\ error if any params are specified
	JSR Param_SyntaxErrorIfNotNull

	LDX #0
	STX dfFree%
	STX dfFree%+1
	STX dfTotal%
	STX dfTotal%+1
	DEX

	; GetDisk returns unformatted disk
	JSR GetDiskFirstAllX
.dfreelp
	BPL dffmted
	LDX #dfFree%
	JSR bcd_inc16_zp_x
.dffmted
	LDX #dfTotal%
	JSR bcd_inc16_zp_x
	JSR GetDiskNext
	BCC dfreelp
.dffin
	LDX #0
	LDY #dfFree%
	JSR DecNo_Print_zp_y
	JSR PrintString
	EQUS " of "
	LDY #dfTotal%
	JSR print_pluralized_disks
	EQUS " free (unformatted)"
	NOP
	JMP PrintNewLine
}
ENDIF


IF _INCLUDE_CMD_DONBOOT_
	\\ 32 byte, including the command table
.CMD_DONBOOT
	JSR Param_DriveAndDisk
	\\ Exit: CurrentDrv=drive, Word &B8=disk no.
	JSR LoadBaseSector
	LDX CurrentDrv
	LDA &B8
	STA MA+&0E00, X
	LDA &B9
	STA MA+&0E04, X
	JMP SaveDiskTable
ENDIF

	\\ *DDRIVE (<drive>)
	\\ List disks in drives
IF _INCLUDE_CMD_DDRIVE_
.CMD_DDRIVE
{
	STY &B3
	LDA #&FF
	STA gdopt%			; GetDisk returns unformatted disks
	LDA #3
	STA CurrentDrv			; Last drive to list
	LDY &B3
	JSR GSINIT_A
	BEQ ddsknoparam
	JSR Param_DriveNo_BadDrive
	BCC ddskloop			; always
.ddsknoparam
	LDA #0
	\\ A = drive
.ddskloop
	PHA
	TAX
	\\ print drive no
	LDA #&3A			; ASC(":")
	JSR OSWRCH
	TXA
	ORA #&30
	JSR OSWRCH

	LDA DRIVE_INDEX4,X
	BPL ddcont			; drive not loaded

	MASK_DISKNO
	STA &B9
	LDA DRIVE_INDEX0,X
	STA &B8
	JSR GetDiskFirst
	CMP #&FF
	BEQ ddcont
	\\ C=0 at this point
IF NOT(_LARGEMMB_)
	SEC
ENDIF
	JSR PrintDCat

.ddcont
	JSR OSNEWL
	PLA
	CMP CurrentDrv
	BEQ ddskexit
	ADC #1
	BCC ddskloop			; always
.ddskexit
	RTS
}
ENDIF

	\\ Mark disk in current drive as formatted
	\\ and clear its disk catalogue entry
	\\ Used by *FORM
IF _INCLUDE_CMD_FORM_VERIFY_
.MarkDriveAsFormatted
{
	SEC				; disk must be unformatted
	JSR GetDriveStatusC
	BPL jmpErrFormatted
	LDA #0
	TAY
.masf_loop
	STA (&B0),Y			; clear title in catalogue
	INY
	CPY #15
	BNE masf_loop
	TYA				; A=&0F Unlocked disk
	BNE masf_status
}
ENDIF

IF _INCLUDE_CMD_DOP_
	\\ Mark disk as read only
.dop_Protect
	LDA #&00
	EQUB &2C			; skip the next 2-byte instruction (BIT abs)
	\\ Mark disk as writable
.dop_Unprotect
	LDA #&0F
.dlul
	PHA
.dkconfirmed
	JSR GetDriveStatus
	BMI jmpErrNotFormatted
.drestore
	PLA
	LDY #15
.masf_status
	STA (&B0),Y
	JSR SaveDiskTable
	JMP LoadDrive			; reload disk

.jmpErrNotFormatted
	JMP errNotFormatted

ENDIF

IF _INCLUDE_CMD_DOP_ OR _INCLUDE_CMD_FORM_VERIFY_
.jmpErrFormatted
	JMP errFormatted
ENDIF

IF _INCLUDE_CMD_DOP_
	\\ Mark disk as unformatted
.dop_Kill
{
	JSR IsEnabledOrGo
	JSR GetDriveStatus
	JSR CheckWriteProtect
	JSR GetDiskFirst
	JSR PrintString
	EQUS "Kill"
	NOP
IF _LARGEMMB_
	\\ Force an extra space before the drive number
	CLC
ELSE
	SEC
ENDIF
	JSR PrintDCat
	JSR PrintString
	EQUS " : "
	NOP
	JSR ConfirmYN
	BNE dkcancel
	LDA #&F0			; Unformatted disk
	PHA
	BNE dkconfirmed
.dkcancel
	RTS
}

	\\ Mark disk as formatted (without reformatting)
.dop_Restore
	LDA #&0F
	PHA
	SEC 				; disk must be unformatted
	JSR GetDriveStatusC
	BMI drestore
	BPL jmpErrFormatted

	\\ Find first unformatted disk and load in drive
.dop_New
{
	LDX #&FF			; GetDisk returns unformatted disk
	JSR GetDiskFirstAllX
.fdkloop
	BCS ErrNoFreeDisks		; no free disks
	CMP #&F0			; Is it formatted?
	BEQ fdkfound			; No!
	JSR GetDiskNext
	JMP fdkloop
.fdkfound
	\ Covert gddisk% to BCD in decno% for printing
	JSR DecNo_BIN2BCD
	\ load unformatted disk
	JSR LoadDrive
	\ message: disk# in drv#:
	JSR PrintString
	EQUS "Disc "
	LDX #0
	JSR DecNo_Print
	JSR PrintString
	EQUS " in :"
	LDA CurrentDrv
	ORA #&30
	JSR OSWRCH
	JMP OSNEWL

.ErrNoFreeDisks
	JSR ReportError
	EQUB &FF
	EQUS "No free discs",0
}
	\\ *DOP (P/U/N/K/R) (<drive>)
	\\ Options: P=Protect, U=Unprotect, N=New, K=Kill, R=Restore
.CMD_DOP
{
	JSR GSINIT_A
	BEQ opterr

	AND #&DF
	LDX #(dopexhi-dop)
.optloop
	CMP dop,X
	BEQ optok
	DEX
	BPL optloop

.opterr
	JMP errBADOPTION

.optok
	LDA dopexhi,X
	PHA
	LDA dopexlo,X
	PHA
	INY
	JMP Param_OptionalDriveNo

.dop
	EQUS "RKNUP"

.dopexhi
	EQUB HI(dop_Restore-1)
	EQUB HI(dop_Kill-1)
	EQUB HI(dop_New-1)
	EQUB HI(dop_Unprotect-1)
	EQUB HI(dop_Protect-1)

.dopexlo
	EQUB LO(dop_Restore-1)
	EQUB LO(dop_Kill-1)
	EQUB LO(dop_New-1)
	EQUB LO(dop_Unprotect-1)
	EQUB LO(dop_Protect-1)
}
ENDIF

ENDIF ;NOT(_MM32_)

	\ Include OSWORD emulation routines here

IF NOT(_MM32_)
	INCLUDE "OSWORD7F.asm"
ENDIF

IF _MM32_ ; MM 11/05/19
	INCLUDE "MM32.asm"
ENDIF

	\ Optional extras!
IF _ROMS_
	INCLUDE "ROMS.asm"
ENDIF
IF _UTILS_
	INCLUDE "Utilities.asm"
ENDIF
IF _TUBEHOST_ AND NOT(_BP12K_)
	INCLUDE "TubeHost230.asm"
ENDIF

IF _BP12K_
	\ This code must be outside the 12K private RAM bank, so it can run
	\ successfully without us having copied our code into that bank.
	IF P%<&B000
	    SKIPTO &B000
	ENDIF

.Init12K
{
	PHP
	PHA
	TXA
	PHA
	TYA
	PHA
	LDA #0
	STA &B0
	LDA #&7F
	STA &B1

	LDA PagedRomSelector_RAMCopy
	ORA #&80
	TAX
	LDY #255
	BNE start_loop
.loop
	LDA (&B0),Y
	STX PagedRomSelector_RAMCopy
	STX &FE30
	STA (&B0),Y
.start_loop
	TXA
	AND #&7F
	STA PagedRomSelector_RAMCopy
	STA &FE30
	INY
	BNE loop
	INC &B1
	LDA &B1
	CMP #&B0
	BEQ done
	CMP #HI(MA+&0E00)
	BNE loop
	LDA #HI(MAEND)
	STA &B1
	BNE loop

.done
	PLA
	TAY
	PLA
	TAX
	PLA
	PLP
	RTS
}

.PageIn12K
	PHP
	PHA
	LDA PagedRomSelector_RAMCopy
	ORA #&80
	STA PagedRomSelector_RAMCopy
	STA &FE30
	PLA
	PLP
	RTS

.PageOut12K
	PHP
	PHA
	LDA PagedRomSelector_RAMCopy
	AND #&7F
	STA PagedRomSelector_RAMCopy
	STA &FE30
	PLA
	PLP
	RTS
ENDIF

PRINT "    code ends at",~P%," (",(guard_value - P%), "bytes free )"

IF _DEVICE_="G"
SAVE &8000, P%
ELSE
SAVE &8000, &C000
ENDIF
