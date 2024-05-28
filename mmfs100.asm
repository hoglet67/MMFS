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

\\ Fast OSGBPB code
_FASTGBPB_=TRUE

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
_MM32_DRENAME=_COMMANDS_ ; NOT(_BP12K_)

IF _MASTER_
	CPU 1				; 65C12
	MA=&C000-&0E00			; Offset to Master hidden static workspace
	_NON_WS_BUILD_COM = FALSE
	guard_value=&C000
ELIF _BP12K_
	; Memory at &Axxx in the 12K private RAM has the (to us) undesirable
	; property that code running there accesses the display RAM, whether
	; that's main or shadow RAM. We therefore can't have any code which
	; needs to access user memory there. To use as much of it as possible up
	; harmlessly, we situate our workspace in that range.
	MA=&A200-&0E00
	;UTILSBUF=(&BF-&B6)+HI( MA+&E00)
	UTILSBUF=(&BF-&B7)+HI( MA+&E00) ; not actually used
	MAEND=(UTILSBUF+1)<<8
	_NON_WS_BUILD_COM = TRUE ; doesn't need workspace
	guard_value=&C000
ELIF _SWRAM_
	MA=&B700-&0E00
	_NON_WS_BUILD_COM = TRUE ; doesn't need workspace
	; UTILSBUF=&BF			; Utilities buffer page
	guard_value=&B6FE
;; Add a special marker that ZMMFS uses to identify an already installed SWMMFS
   	org &B6FE
   	EQUB MAGIC0
   	EQUB MAGIC1
ELSE
	_NON_WS_BUILD_COM = FALSE
	MA=0
	guard_value=&C000
ENDIF

IF _SWRAM_ AND NOT(_MM32_)
	disccataloguebuffer% = MA+&E00
	workspace% = MA+ &1000
	tempbuffer% = MA+ &1100

ELSE
	disccataloguebuffer% = MA+&E00
	workspace% = MA+ &1000
	tempbuffer% = MA+ &1000
ENDIF

MP=HI(MA)

tempfilename1 = MA+ &1000
tempfilename2 = MA+ &1007


buf%=disccataloguebuffer%
cat%=disccataloguebuffer%
FilesX8=disccataloguebuffer%+&105

; Channel data buffer each channel is 32 bytes
; 0th channel isn't
; channel 1 to 5 are used.
; channel 6 and 7 aren't used

; this is reorganised to be more logical and save space
channeldata = MA + &1100
channeldata_filename = channeldata
channeldata_filename6_readonly = channeldata+6
channeldata_directory_locked = channeldata+7
chenneldata_attributes = channeldata+8
channeldata_load = channeldata+&8
channeldata_exec = channeldata+&A
channeldata_length = channeldata+&C
channeldata_mixedbyte = channeldata+&E
channeldata_sector = channeldata+&F
channeldata_ptr = channeldata+&10 ; 3 bytes
channeldata_bufferpage = channeldata + &13 ;
channeldata_ext	= channeldata+&14 ; 3 bytes
channeldata_drive_flags = channeldata + &17 ; b7:data in buffer b6 : new data: b5:Extended b4:EOF b1-b0 drive
; &18 not used
channeldata_sectorcount = channeldata + &19 ; 2 bytes
channeldata_channelbit = channeldata + &1B ; bit mask
channeldata_sectorinbuffer = channeldata + &1C ; 2 bytes
;1e unused
;1f unused


INCLUDE "VERSION.asm"
INCLUDE "SYSVARS.asm"			; OS constants

DirectoryParam=&CC
CurrentDrv=&CD

CurrentCat=workspace%+&82

IF _MM32_
	TubeNoTransferIf0=workspace%+&AE
	MMC_STATE=workspace%+&AF
ELSE
	TubeNoTransferIf0=workspace%+&9E
	MMC_STATE=workspace%+&9F			; Bit 6 set if card initialised
ENDIF


;Zero Page allocations
; A8 - AF temporay * commands
; B0 - BF FileSystem temporay workspace
; C0 - CF current File system workspace

FSMessagesOffIfZero=workspace%+&C6
CMDEnabledIf1=workspace%+&C7
DEFAULT_DIR=workspace%+&C9
DEFAULT_DRIVE=workspace%+&CA
LIB_DIR=workspace%+&CB
LIB_DRIVE=workspace%+&CC
PAGE=workspace%+&CF
RAMBufferSize=workspace%+&D0			; HIMEM-PAGE
ForceReset=workspace%+&D3
TubePresentIf0=workspace%+&D6
CardSort=workspace%+&DE

IF _LARGEMMB_
	DiskTableIndex=workspace%+&D4
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



\\ TODO: CardSort should be protected by VID...

IF _MM32_
	VID=workspace%+&E0				; VID
	VID2=VID				; 14 bytes
	MMC_CIDCRC=VID2+&E			; 2 bytes
	CHECK_CRC7=VID2+&10			; 1 byte
ELSE
IF _LARGEMMB_
	VID=workspace%+&DF				; VID
	CHUNK_BASE=VID+&E			; 1 byte
	NUM_CHUNKS=VID+&F    			; 1 byte
	CHECK_CRC7=VID+&10			; 1 byte
ELSE
	VID=workspace%+&E0				; VID
	CHECK_CRC7=VID+&E			; 1 byte
ENDIF
	DRIVE_INDEX0=VID 			; 4 bytes
	DRIVE_INDEX4=VID+4			; 4 bytes
	MMC_SECTOR=VID+8			; 3 bytes
	MMC_SECTOR_VALID=VID+&B			; 1 bytes
	MMC_CIDCRC=VID+&C			; 2 bytes
ENDIF

IF _MM32_
	OWCtlBlock = workspace%+&B0		; 16 bytes
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

.BootOptions
	EQUS "L.!BOOT",13
	EQUS "E.!BOOT",13

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
	LDA workspace%+&DD			; Error while writing
	BNE brk100_notbuf		; channel buffer?
	JSR ClearEXECSPOOLFileHandle
.brk100_notbuf
	LDA #&FF
	STA CurrentCat
	STA workspace%+&DD			; Not writing buffer

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
	\ uses ZP &AE &AF &B3
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
	LDA #&EC			; Read character destination
	JSR osbyte_X0YFF
	TXA 				; X = chr destination
	PHA
	ORA #&10
	TAX
	JSR osbyte03_Xoutstream		; Disable spooled output
	PLA
	TAX
	PLA
	JSR OSASCI			; Output chr
	; Restore previous setting
.osbyte03_Xoutstream
	LDA #3
	JMP OSBYTE

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
	JMP OSWRCH

	\\ Print spaces, exit C=0 A preserved
.Print2SpacesSPL
	JSR PrintSpaceSPL		; Print 2 spaces
.PrintSpaceSPL
	PHA				; Print space
	LDA #&20
	JSR OSWRCH
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
	STA workspace%+&72,X
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
.parameter_afsp_Param_SyntaxErrorIfNull_read_fspTextPointer
	JSR parameter_afsp
.Param_SyntaxErrorIfNull_read_fspTextPointer
	JSR Param_SyntaxErrorIfNull
;.read_fspTextPointer
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
	STA tempfilename1
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
	STA tempfilename1,X
	INX
	JSR GSREAD_A
	BCS rdafsp_padX			; IF end of string
	CPX #&07
	BNE rdafsp_rdfnloop
	BEQ errBadName			; always
}

.Rdafsp_padall
	LDX #&01			; Pad all with spaces
.rdafsp_padX
{
	CPX #7
	BEQ rdafsp_cpyfnstart
	LDA #&20			; Pad with spaces
.rdafsp_padloop
	STA tempfilename1,X
	INX
	CPX #7			; Why &40? : Wildcards buffer! only 7 bytes needed
	BNE rdafsp_padloop
.rdafsp_cpyfnstart
	DEX				; Copy from &1000 to &C5
.rdafsp_cpyfnloop
	LDA tempfilename1,X			; 7 byte filename
	STA &C5,X
	DEX
	BPL rdafsp_cpyfnloop
	RTS
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

.prt_filename_Yoffset
{
	JSR RememberAXY
	LDA disccataloguebuffer%+&0F,Y
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
	LDA disccataloguebuffer%+&08,Y
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
	JSR Param_SyntaxErrorIfNull_read_fspTextPointer  ; string is 7 chars with a space at 8th
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
	STA tempfilename1
	JSR Rdafsp_padall
	JSR parameter_afsp
	JSR getcatentry
	BCS cmd_info_loop ; always
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

.read_fspBA_reset_get_cat_firstentry80
	JSR read_fspBA_reset
.get_cat_firstentry80
.get_cat_firstentry81
	JSR CheckCurDrvCat		; Get cat entry
	LDX #LO(tempfilename1) 	; now first byte @ &1000+X
	BEQ getcatentry2		; always

.get_cat_nextentry
	LDX #LO(tempfilename1)	; Entry: wrd &B6 -> first entry
	BEQ getcatsetupB7	; always ( almost certainly could be getcatloop2 )

.get_cat_firstentry80fname
	;LDX #&07			; copy filename from &C5 to &1058
	;LDA #&20			; set last char to " "
	;BNE getcatloopentry ; always
	LDX #6				; no need to have any padding
.getcatloop1
	LDA &C5,X
.getcatloopentry
	STA tempfilename2,X
	DEX
	BPL getcatloop1

	JSR CheckCurDrvCat		; catalogue entry matching
	LDX #LO(tempfilename2) 				; string was at &1058
.getcatentry2
	LDA #LO(disccataloguebuffer%)		; word &B6 = &E00 = PTR
	STA &B6
.getcatsetupB7
	LDA #HI(disccataloguebuffer%)		; string at &E00+A
	STA &B7
.getcatloop2
	LDY #&00
	LDA &B6
	CMP FilesX8			; ( MA+&F05) number of files *8
	BCS matfn_exitC0		; If >FilesX8 Exit with C=0
	ADC #&08
	STA &B6				; word &B6 += 8
	TXA
	PHA
	JSR MatchFilename
	PLA
	TAX
	BCC getcatloop2			; not a match, try next file
	LDA DirectoryParam
	LDY #&07
	JSR matchcharentry
	BCC getcatloop2			; If directory doesn't match
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

.NotCmdTable2
	RTS

.MatchFilename
{

.matchloop
	LDA tempfilename1,X
	INX
.^matchcharentry
	CMP workspace%+&CE
	BEQ matchfound		; eg. If "*"
	CMP workspace%+&CD
	BEQ matchr_exit		; eg. If "#"
	JSR IsAlphaChar
	EOR (&B6),Y
	BCS matchr_notalpha	; IF not alpah char
	AND #&5F
.matchr_notalpha
	AND #&7F
	BNE matfn_exitC0
.matchr_exit
	INY
	CPY #7
	BCC matchloop
.matchfound
	SEC
	RTS
}
.matfn_exitC0
	CLC 				; exit with C=0
	RTS


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
	LDA disccataloguebuffer%+&10,Y
	STA disccataloguebuffer%+&08,Y
	LDA disccataloguebuffer%+&100+&10,Y
	STA disccataloguebuffer%+&100+&08,Y
	INY
	CPY FilesX8
	BCC delcatloop
	TYA
	SBC #&08
	STA FilesX8
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
	BIT FSMessagesOffIfZero		; Print message
	BPL print_infoline_exit
.prt_InfoLine_Yoffset
	JSR RememberAXY			; Print info
	JSR prt_filename_Yoffset
	TYA 				; Save offset
	PHA
	LDA #LO(workspace%+&60)			; word &B0=1060
	STA &B0
	LDA #HI(workspace%+&60)
	STA &B1
	JSR ReadFileAttribsToB0_Yoffset	; create no. str
	LDY #&02
	JSR PrintSpaceSPL		; print "  "
	JSR PrintHex3Byte		; Load address
	JSR PrintHex3Byte		; Exec address
	JSR PrintHex3Byte		; Length
	PLA
	TAY
	LDA disccataloguebuffer%+&100+&0E,Y			; First sector high bits
	AND #&03
	JSR PrintNibble
	LDA disccataloguebuffer%+&100+&0F,Y			; First sector low byte
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
	LDA workspace%+&62,Y
	JSR PrintHex
	DEY
	DEX
	BNE printhex3byte_loop
	JSR Y_add7
	JMP PrintSpaceSPL
}


.ReadFileAttribsToWSDB_Yoffset
	JSR SetParamBlockPointerB0
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
	LDA disccataloguebuffer%+&100+&08,X	; copy low bytes of
	STA (&B0),Y		; load/exec/length
	INX
	INY

	LDA disccataloguebuffer%+&100+&08,X
	STA (&B0),Y
	INX
	INY

	INY
	INY
	CPY #&0E
	BNE readfileattribs_copyloop

	PLA
	TAX
	LDA disccataloguebuffer%+&0F,X
	BPL readfileattribs_notlocked	; If not locked
	LDA #&08
	STA (&B0),Y			; pwsp+&E=8
.readfileattribs_notlocked
	LDA disccataloguebuffer%+&100+&0E,X			; mixed byte
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
	LDA disccataloguebuffer%+&00,Y			; print disk title
	CPY #&08
	BCC cat_titlelo
	LDA disccataloguebuffer%+&F8,Y
.cat_titlelo
	JSR PrintChrA
	INY
	CPY #&0C
	BNE cat_titleloop
	JSR PrintString			; Print " (n) "; n=cycle no.
	EQUS " ("			; Print "Drive "

IF _MM32_
	LDA disccataloguebuffer%+&100+&04
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
	LDA disccataloguebuffer%+&100+&06
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
	LDA disccataloguebuffer%+&0F,Y
	EOR DEFAULT_DIR
	AND #&5F
	BNE cat_curdirnext		; If not current dir
	LDA disccataloguebuffer%+&0F,Y			; Set dir to null, sort=>first
	AND #&80			; Keep locked flag (bit 7)
	STA disccataloguebuffer%+&0F,Y
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
	LDA disccataloguebuffer%+&08,Y
	BMI cat_getnextunmarkedfile_loop	; If marked file
.cat_exit
	RTS

.cat_printfilename
	STY &AB				; save Y=cat offset
	LDX #&00
.cat_copyfnloop
	LDA disccataloguebuffer%+&08,Y			; Copy filename to 1060
	JSR UcaseA2
	STA workspace%+&60,X
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
	LDA disccataloguebuffer%+&0E,Y			; compare filenames
	JSR UcaseA2			; (catfn-memfn)
	SBC workspace%+&60,X
	DEY
	DEX
	BPL cat_comparefnloop2

	JSR Y_add7
	LDA disccataloguebuffer%+&0F,Y			; compare dir
	JSR UcaseA2			; (clrs bit 7)
	SBC workspace%+&67
	BCC cat_printfilename		; If catfn<memfn
	JSR Y_add8
	BCS cat_comparefnloop1		; else memfn>catfn
.cat_printfn
	LDY &AB				; Y=cat offset
	LDA disccataloguebuffer%+&08,Y			; mark file as printed
	ORA #&80
	STA disccataloguebuffer%+&08,Y
	LDA workspace%+&67			; dir
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
	LDY &A8				; [if ?&A8<>0 = first column]
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
	BMI gocmdcode2
.dommcinit
	JSR MMC_BEGIN2
	ORA #&80
.gocmdcode2
	PHA				; Push sub address and
	LDA cmdaddr1,X			; return to it!
	PHA
	RTS
}

IF _INCLUDE_CMD_WIPE_
.CMD_WIPE
{
	JSR parameter_afsp_Param_SyntaxErrorIfNull_getcatentry_fspTxtP
.wipeloop
	LDA disccataloguebuffer%+&0F,Y
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
	LDA disccataloguebuffer%+&0F,Y			; Print list of matching files
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
	LDA disccataloguebuffer%+&0F,Y
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
	JSR read_fspBA_reset_get_cat_firstentry80	; Get Load Addr etc.
	JSR ReadFileAttribsToWSDB_Yoffset	; from catalogue (Just for info?)

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
	LDA disccataloguebuffer%+&100+&0E,Y 			; mixed byte
	STA &C2
	JSR LoadAddrHi2

.load_copyfileinfo_loop
	LDA disccataloguebuffer%+&100+&08,Y
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

.osfile0_savememblock
	JSR CreateFile_FSP
	JSR ReadFileAttribsToWSDB_Yoffset
	; fall into SaveMemBlock

	\\ **** Save block of memory ****
.SaveMemBlock
IF _MM32_
	LDA #&A5
	;BNE exec_block_rw
ELSE
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
	LDA workspace%+&74-1,X
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

	JSR OW7F_Execute_and_ReportIfDiskFault

	LDA #1
	RTS
}
ENDIF


.fscv2_4_11_starRUN
	JSR SetTextPointerYX		; ** RUN
.NotCmdTable4
{

	LDA #&FF
	STA &BE
	LDA TextPointer
	STA &BA
	LDA TextPointer+1
	STA &BB

	STY workspace%+&DA			; Y=0
	JSR read_fspBA_reset		; Look in default drive/dir
	STY workspace%+&D9			; Y=text ptr offset
	JSR get_cat_firstentry81
	BCS runfile_found		; If file found
	LDY workspace%+&DA
	LDA LIB_DIR			; Look in library
	STA DirectoryParam
	LDA LIB_DRIVE
	STA CurrentDrv
	JSR read_fspBA			; **** is this really required? can't we just reset the pointers?
	JSR get_cat_firstentry81
	BCS runfile_found		; If file found

.errBADCOMMAND
	JSR errBAD
	EQUB &FE
	EQUS "command",0

.runfile_found
	LDA disccataloguebuffer%+&100+&0E,Y			; New to DFS
	JSR A_rorx6and3			; If ExecAddr=&FFFFFFFF *EXEC it
	CMP #&03
	BNE runfile_run			; If ExecAddr<>&FFFFFFFF
	LDA disccataloguebuffer%+&100+&0A,Y
	AND disccataloguebuffer%+&100+&0B,Y
	CMP #&FF
	BNE runfile_run			; If ExecAddr<>&FFFFFFFF
	LDX #&06			; Else *EXEC file  (New to DFS)

.runfile_exec_loop
	LDA tempfilename1,X			; Move filename
	STA tempfilename2,X
	DEX
	BPL runfile_exec_loop

	LDA #&0D
	STA tempfilename2+7
	LDA #&45
	STA tempfilename1			; "E"
	LDA #&3A			; ":"
	STA tempfilename1+2
	LDA CurrentDrv
	ORA #&30
	STA tempfilename1+3			; Drive number X
	LDA #&2E			; "."
	STA tempfilename1+1
	STA tempfilename1+4
	STA tempfilename1+6
	LDA DirectoryParam		; Directory D
	STA tempfilename1+5
	LDX #LO(tempfilename1)			; "E.:X.D.FILENAM"
	LDY #HI(tempfilename1)
	JMP OSCLI

.runfile_run
	JSR LoadFile_Ycatoffset	; Load file (host|sp)
	CLC
	LDA workspace%+&D9			; Word &10D9 += text ptr
	TAY 				; i.e. -> parameters
	ADC TextPointer
	STA workspace%+&D9
	LDA TextPointer+1
	ADC #&00
	STA workspace%+&DA
	LDA workspace%+&76			; Execution address hi bytes
	AND workspace%+&77
	ORA TubePresentIf0
	CMP #&FF
	BEQ runfile_inhost		; If in Host
	LDA &BE				; Copy exec add low bytes
	STA workspace%+&74
	LDA &BF
	STA workspace%+&75
	JSR TUBE_CLAIM
	LDX #LO(workspace%+&74)			; Tell second processor
	\ assume tube code doesn't change sw rom
	LDY #HI(workspace%+&74)
	LDA #&04			; (Exec addr @ 1074)
	JMP TubeCode			; YX=addr,A=0:initrd,A=1:initwr,A=4:strexe
.runfile_inhost
	LDA #&01			; Execute program
	JMP (&00BE)
}

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
	LDA workspace%+&00,Y
	BCS stat_Y_gtreqC0
.stat_Y_lessC0
	LDA channeldata,Y
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
	STA CurrentDrv
	RTS ; Drive 0!

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

titlestr%=tempfilename1

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
	STA disccataloguebuffer%+&F8,X
	RTS
.setdisttit_page
	STA disccataloguebuffer%+&00,X
	RTS
}
ENDIF



IF _INCLUDE_CMD_ACCESS_
.CMD_ACCESS
{
	JSR parameter_afsp_Param_SyntaxErrorIfNull_read_fspTextPointer
	LDX #&00			; X=locked mask
	JSR GSINIT_A
	BNE cmdac_getparam		; If not null string
.cmdac_flag
	STX &AA
	JSR getcatentry
.cmdac_filefound
	JSR CheckFileNotOpenY		; Error if it is!
	LDA disccataloguebuffer%+&0F,Y			; Set/Reset locked flag
	AND #&7F
	ORA &AA
	STA disccataloguebuffer%+&0F,Y
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
{
	JSR RememberAXY
	CPX #&04
	BEQ SetBootOption_Yoption
	CPX #&05
	BEQ DiskTrapOption
	CPX #&02
	BCS errBADOPTION		; If A>=2
.opts0_1					; *OPT 0,Y or *OPT 1,Y
	TYA
	BEQ	opts0_1_Y0
	LDA #&FF
.opts0_1_Y0
	STA FSMessagesOffIfZero		; = Y=0, I.e. &00 =messages off
	RTS

.SetBootOption_Yoption
	TYA 				; *OPT 4,Y
	PHA
	JSR Set_CurDirDrv_ToDefaults
	JSR LoadCurDrvCat		; load cat
	PLA
	JSR A_rolx4
	EOR disccataloguebuffer%+&100+&06
	AND #&30
	EOR disccataloguebuffer%+&100+&06
	STA disccataloguebuffer%+&100+&06
	JMP SaveCatToDisk		; save cat

.DiskTrapOption

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

.errBADOPTION
	JSR errBAD
	EQUB &CB
	EQUS "option",0

.errDISKFULL
	JSR errDISK
	EQUB &C6
	EQUS "full",0

.CreateFile_FSP
{
	JSR read_fspBA_reset_get_cat_firstentry80 ; loads cat/does file exist?
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
	LDA workspace%+&7A
	SBC workspace%+&78
				; C=B-A
	JSR CreateFile_2
	LDA workspace%+&79			; Load Address=Start Address
	STA workspace%+&75			; (4 bytes)
	LDA workspace%+&78
	STA workspace%+&74
	PLA
	STA &BD
	PLA
	STA &BC
	RTS
}

.CreateFile_3
	STA CurrentDrv
	LDA DirectoryParam
	PHA
	JSR LoadCurDrvCat2		; Load cat ( BC to CB preserved)
	JSR get_cat_firstentry80fname ; use filename @ &C5 which is copied to &1058
	BCC cd_writedest_cat_nodel	; If file not found
	JSR DeleteCatEntry_YFileOffset
.cd_writedest_cat_nodel
	PLA
	STA DirectoryParam ; cant't see how this gets corrupted so why stack and restore ?
	JSR LoadandEexeAddrHi2
	LDA &C2				; mixed byte
	JSR A_rorx4and3
	; this will be stored in C4 ( top bits of length)
.CreateFile_2
{
	STA &C4				; top bits of length ( C4 is a temp variable)
	LDA #&00			; NB Cat stored in
	STA &C2				; desc start sec order
	LDA #&02			; (file at 002 last)
	STA &C3				; wC2=&200=sector
	LDY FilesX8			; find free block
	CPY #&F8			; big enough
	BCC Getfirstblock_Yoffset

.errCATALOGUEFULL
	JSR ReportErrorCB
	EQUB &BE
	EQUS "Cat full",0

.cfile_loop
	BEQ errDISKFULL
	JSR Y_sub8

	LDA disccataloguebuffer%+&100+&0E,Y
	JSR A_rorx4and3
	STA &C2				;len byte 3
	CLC
	LDA #&FF			; -1
	ADC disccataloguebuffer%+&100+&0C,Y			; + len byte 1
	LDA disccataloguebuffer%+&100+&0F,Y			; + start sec byte 1
	ADC disccataloguebuffer%+&100+&0D,Y			; + len byte 2
	STA &C3
	LDA disccataloguebuffer%+&100+&0E,Y			; start sec byte 2
	AND #&03
	ADC &C2				; calc. next "free" sector
	STA &C2				; wC2=start sec + len - 1
.Getfirstblock_Yoffset
	SEC
	LDA disccataloguebuffer%+&100+&07,Y			; secs on disk
	SBC &C3				; or start sec of prev.
	PHA 				; file
	LDA disccataloguebuffer%+&100+&06,Y			; - end of prev. file (wC2)
	AND #&03
	SBC &C2
	TAX
	LDA #&00
	CMP &C0
	PLA 				; ax=secs on disk-next blk
	SBC &C1
	TXA 				; req'd=c0/c1/c4
	SBC &C4				; big enough?

	TYA
	BCC cfile_loop			; If not big enough
	STY &B0				; Else block found
	LDY FilesX8			; Insert space into catalogue
.cfile_insertfileloop
	CPY &B0
	BEQ cfile_atcatentry		; If at new entry
	LDA disccataloguebuffer%+&07,Y
	STA disccataloguebuffer%+&0F,Y
	LDA disccataloguebuffer%+&100+&07,Y
	STA disccataloguebuffer%+&100+&0F,Y
	DEY
	BCS cfile_insertfileloop
.cfile_atcatentry

	LDA workspace%+&76			; Exec address b17,b16
	AND #&03
	ASL A
	ASL A
	EOR &C4				; Length
	AND #&FC
	EOR &C4
	ASL A
	ASL A
	EOR workspace%+&74			; Load address
	AND #&FC
	EOR workspace%+&74
	ASL A
	ASL A
	EOR &C2				; Sector
	AND #&FC
	EOR &C2
	STA &C2				; C2=mixed byte

	LDX #&00
	TYA
	PHA
.cfile_copyfnloop
	LDA &C5,X			; Copy filename from &C5
	STA disccataloguebuffer%+&08,Y
	LDA &BC,X			; Copy attributes
	STA disccataloguebuffer%+&100+&08,Y
	INY
	INX
	CPX #&08
	BNE cfile_copyfnloop
	PLA
	TAY
	PHA
	JSR prt_InfoMsg_Yoffset

	LDY FilesX8
	JSR Y_add8
	STY FilesX8			; FilesX+=8
	JSR SaveCatToDisk		; save cat
	PLA
	TAY
	RTS
}



IF _INCLUDE_CMD_ENABLE_
.CMD_ENABLE
	LDA #&01
	STA CMDEnabledIf1
	RTS
ENDIF


.LoadAddrHi2
	LDA #&00
	STA workspace%+&75
	LDA &C2
	AND #&08
	STA workspace%+&74
	BEQ ldadd_nothost
.SetLoadAddrToHost
	LDA #&FF
	STA workspace%+&75
	STA workspace%+&74
.ldadd_nothost
	RTS

.LoadandEexeAddrHi2
	JSR LoadAddrHi2
.ExecAddrHi2
{
	LDA #&00
	STA workspace%+&77
	LDA &C2
	JSR A_rorx6and3
	CMP #&03
	BNE exadd_nothost
	LDA #&FF
	STA workspace%+&77
.exadd_nothost
	STA workspace%+&76
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
	JSR Param_SyntaxErrorIfNull_read_fspTextPointer ; filename at tempfilename1
	TYA
	PHA
	JSR getcatentry		; serach for file name at tempfilename1
	JSR CheckFileNotLockedOrOpenY
	STY &A8
	PLA
	TAY
	LDA CurrentDrv
	PHA
	JSR Param_SyntaxErrorIfNull_read_fspTextPointer
	PLA
	CMP CurrentDrv
	BNE jmpBADDRIVE
	JSR get_cat_firstentry80
	BCC rname_ok		; second filename doesn't exist
	CPY &A8				; if both names are the same
	BEQ rname_ok		; then just do the rename anyway
.errFILEEXISTS
	JSR ReportErrorCB
	EQUB &C4
	EQUS "Exists",0
.jmpBADDRIVE
	JMP errBADDRIVE
.rname_ok
	LDY &A8				; Copy filename
	;JSR Y_add8			; from C5 to catalog
	LDX #&07
.rname_loop
	LDA &C5,X
	STA disccataloguebuffer%+&07+8,Y
	DEY
	DEX
	BPL rname_loop			; else Save catalogue
	JMP SaveCatToDisk
}
ENDIF

.TUBE_CheckIfPresent
	LDA #&EA			; Tube present?
	JSR osbyte_X0YFF	; X=FF if Tube present
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
	LDA #&7E
	JSR OSBYTE
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
	STX CurrentDrv			; curdrv=0
	STX MMC_STATE			; Uninitialised

	LDX #&0F			; vectors claimed!
	LDA #&8F
	JSR OSBYTE

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
	STA workspace%+&00,Y
	BCS copyfromPWS2
.copyfromPWS1
	STA channeldata,Y
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
	STA channeldata_sectorinbuffer+1,Y	; invalidate sector in buffer by putting it out of range
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
ENDIF

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
	STY workspace%+&C0
	STY FSMessagesOffIfZero
	DEY				; Y=&FF
	STY CMDEnabledIf1

	STY workspace%+&DD
}

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
	LDA disccataloguebuffer%+&100+&06			; Get boot option
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
	TXA
	JSR PrintHex
	JSR PrintString
	EQUB ";Y="
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
{
	LDY &EF				; Y = Osword call
	BMI exit			; Y > &7F
	CPY #&7D
	BCC exit			; Y < &7D

	\ Test if MMFS by checking OSFILE vector.

	LDA &213			; Check of the low OSFILE vector is pointing
	CMP #&FF            ; to the corresponding extended vector.
	BNE notMMFS
	LDA &212
	CMP #&1B
	BNE notMMFS
	LDA &0DBC			; Rom number in extended vector.
	CMP &F4				; Is it our ROM?
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
.notMMFS
.exit
}
	RTS

.notOSWORD7F
	JSR Set_CurDirDrv_ToDefaults_and_load		; Load catalogue
	INY
	BMI OSWORD7E

	LDY #&00			; OSWORD &7D return cycle no.
	LDA disccataloguebuffer%+&100+&04
	STA (&B0),Y
	RTS

.OSWORD7E
	LDA #&00			; OSWORD &7E
	TAY
	STA (&B0),Y
	INY
	LDA disccataloguebuffer%+&100+&07			; sector count LB
	STA (&B0),Y
	INY
	LDA disccataloguebuffer%+&100+&06			; sector count HB
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


.FILEV_ENTRY
{
	JSR RememberXYonly
	PHA
	JSR parameter_fsp

	STX &B0				; XY -> parameter block
	STX workspace%+&DB
	STY &B1
	STY workspace%+&DC

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
	RTS

.GBPBV_ENTRY
{
	CMP #&09
	BCS gbpbv_unrecop
	JSR RememberAXY
	JSR ReturnWithA0
	STX workspace%+&7D		; Save pointer to command block
	STY workspace%+&7E
	TAY					; Call number
IF _FASTGBPB_
	JMP fastgb
.*upgbpb
ENDIF
	JSR gbpb_gosub
	PHP
	BIT workspace%+&81
	BPL gbpb_nottube
	JSR TUBE_RELEASE_NoCheck
.gbpb_nottube
	PLP
.gbpbv_unrecop
	RTS
}

.gbpb_gosub
{
	LDA gbpbv_table1,Y
	STA workspace%+&D7
	LDA gbpbv_table2,Y
	STA workspace%+&D8
	LDA gbpbv_table3,Y		; 3 bit flags: bit 2=tube op
	LSR A
	PHP 				; Save bit 0 (0=write new seq ptr)
	LSR A
	PHP 				; Save bit 1 (1=read/write seq ptr)
	STA workspace%+&7F			; Save Tube operation
IF _FASTGBPB_
ELSE
	JSR gbpb_wordB4_word107D	; (B4) -> param blk
ENDIF
	LDY #&0C

.gbpb_ctlblk_loop
	LDA (&B4),Y			; Copy param blk to 1060
	STA workspace%+&60,Y
	DEY
	BPL gbpb_ctlblk_loop
;DFS 2.45 9E30
	LDA workspace%+&63			; Data ptr bytes 3 & 4
	AND workspace%+&64
	ORA TubePresentIf0
	CLC
	ADC #&01
	BEQ gbpb_nottube1		; If not tube

	JSR TUBE_CLAIM
	CLC

	LDA #&FF

.gbpb_nottube1
	STA workspace%+&81			; GBPB to TUBE IF >=&80
	LDA workspace%+&7F			; Tube op: 0 or 1
	BCS gbpb_nottube2		; If not tube
	LDX #LO(workspace%+&61)
	LDY #HI(workspace%+&61)

	JSR TubeCode 			; (YX=addr,A=0:initrd,A=1:initwr,A=4:strexe) ; Init TUBE addr @ 1061
.gbpb_nottube2
	PLP 				; Bit 1
	BCS gbpb_rw_seqptr
	PLP 				; Bit 0, here always 0
}
.gbpb_jmpsub
	JMP (workspace%+&D7)

.gbpb_rw_seqptr
{
	LDX #&03			; GBPB 1,2,3 or 4
.gbpb_seqptr_loop1
	LDA workspace%+&69,X			; !B6=ctl blk seq ptr
	STA &B6,X
	DEX
	BPL gbpb_seqptr_loop1		; on exit A=file handle=?&1060

	LDX #&B6
	LDY workspace%+&60
	LDA #&00
	PLP				; bit 0
	BCS gpbp_dontwriteseqptr
	JSR argsv_WriteSeqPointer	; If GBPB 1 & 3
.gpbp_dontwriteseqptr
	JSR argsv_rdseqptr_or_filelen	; read seq ptr to &B6
	LDX #&03
.gbpb_seqptr_loop2
	LDA &B6,X			; ctl blk seq prt = !B6
	STA workspace%+&69,X
	DEX
	BPL gbpb_seqptr_loop2
}

.gbpb_rwdata
{
	JSR gbpb_bytesxferinvert	; Returns with N=1
	BMI gbpb_data_loopin		; always
.gbpb_data_loop
	LDY workspace%+&60			; Y=file handle
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
		 						; Copy parameter back
	JSR gbpb_wordB4_word107D	; (B4) -> param blk
	LDY #&0C
.gbpb_restorectlblk_loop
	LDA workspace%+&60,Y
	STA (&B4),Y
	DEY
	BPL gbpb_restorectlblk_loop
	PLP 				; C=1=txf not completed
	RTS 				; **** END GBPB 1-4
}

	\\ READ FILENAMES IN CURRENT CAT
.gbpb8_rdfilescurdir
	; GBPB 8
	JSR Set_CurDirDrv_ToDefaults_CheckCurDrvCat
	LDA #LO(gbpb8_getbyte)
	STA workspace%+&D7
	LDA #HI(gbpb8_getbyte)
	STA workspace%+&D8
	BNE gbpb_rwdata			; always

.gbpb8_getbyte
{
	LDY workspace%+&69			; GBPB 8 - Get Byte
.gbpb8_loop
	CPY FilesX8
	BCS gbpb8_endofcat		; If end of catalogue, C=1
	LDA disccataloguebuffer%+&0F,Y			; Directory
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
	LDA disccataloguebuffer%+&08,Y			; Copy fn
	JSR gbpb_gb_SAVEBYTE
	INY
	DEC &B0
	BNE gbpb8_copyfn_loop
	CLC 				; C=0=more to follow
.gbpb8_endofcat
	STY workspace%+&69			; Save offset (seq ptr)
	LDA disccataloguebuffer%+&100+&04
	STA workspace%+&60			; Cycle number (file handle)
	RTS 				; **** END GBPB 8
}


	\\ GET MEDIA TITLE
.gbpb5_getmediatitle
{
		; GBPB 5
	JSR Set_CurDirDrv_ToDefaults_CheckCurDrvCat
	LDA #&0C			; Length of title
	JSR gbpb_gb_SAVEBYTE
	LDY #&00
.gbpb5_titleloop
	CPY #&08			; Title
	BCS gbpb5_titlehi
	LDA disccataloguebuffer%+&00,Y
	BCC gbpb5_titlelo
.gbpb5_titlehi
	LDA disccataloguebuffer%+&F8,Y
.gbpb5_titlelo
	JSR gbpb_gb_SAVEBYTE
	INY
	CPY #&0C
	BNE gbpb5_titleloop
	LDA disccataloguebuffer%+&100+&06			; Boot up option
	JSR A_rorx4
	JSR gbpb_gb_SAVEBYTE
	LDA CurrentDrv			; Current drive
	JMP gbpb_gb_SAVEBYTE
}

	\\ READ CUR DRIVE/DIR
.gbpb6_rdcurdirdevice
	JSR gbpb_SAVE_01		; GBPB 6
	LDA DEFAULT_DRIVE		; Length of dev.name=1
	JSR gbpb_gb_SAVEBYTE_and_gbpb_SAVE_01 ; Lendgh of dir.name=1
	LDA DEFAULT_DIR			; Directory
	BNE gbpb_gb_SAVEBYTE

	\\ READ LIB DRIVE/DIR
.gbpb7_rdcurlibdevice
	JSR gbpb_SAVE_01		; GBPB 7
	LDA LIB_DRIVE			; Length of dev.name=1
	JSR gbpb_gb_SAVEBYTE_and_gbpb_SAVE_01		; Lendgh of dir.name=1
	LDA LIB_DIR			; Directory
	BNE gbpb_gb_SAVEBYTE

.gpbp_B8memptr
	; Set word &B8 to
	LDX workspace%+&61			; ctl blk mem ptr (host)
	STX &B8
	LDX workspace%+&62
	STX &B9
	LDX #&00
	RTS

.gbpb_bytesxferinvert
{
	LDX #&03			; Bytes to tranfer XOR &FFFF
.gbpb_bytesxferinvert_loop
	LDA #&FF
	EOR workspace%+&65,X
	STA workspace%+&65,X
	DEX
	BPL gbpb_bytesxferinvert_loop
	RTS
}

.gbpb_wordB4_word107D
	LDA workspace%+&7D
	STA &B4
	LDA workspace%+&7E
	STA &B5
	RTS

.gbpb_gb_SAVEBYTE_and_gbpb_SAVE_01
	ORA #&30			; Drive no. to ascii
	JSR gbpb_gb_SAVEBYTE
.gbpb_SAVE_01
	LDA #&01
	BNE gbpb_gb_SAVEBYTE		; always
.gbpb_getbyteSAVEBYTE
	JSR BGETV_ENTRY
	BCS gbpb_incdblworkd_exit			; If EOF
.gbpb_gb_SAVEBYTE
	BIT workspace%+&81
	BPL gBpb_gb_fromhost
	STA TUBE_R3_DATA		; fast Tube Bget
	BMI gbpb_incDataPtr
.gBpb_gb_fromhost
	JSR gpbp_B8memptr
	STA (&B8,X)

.gbpb_incDataPtr
	JSR RememberAXY			; Increment data ptr
	LDX #&01
.gbpb_incdblword1060X
{
	LDY #&04			; Increment double word
.gbpb_incdblword_loop
	INC workspace%+&60,X
	BNE gbpb_incdblworkd_exit
	INX
	DEY
	BNE gbpb_incdblword_loop
}
.gbpb_incdblworkd_exit
	RTS


.gbpb_putbytes
	JSR gpbp_pb_LOADBYTE
	JSR BPUTV_ENTRY
	CLC
	RTS 				; always ok!
.gpbp_pb_LOADBYTE
	BIT workspace%+&81
	BPL gbpb_pb_fromhost
	LDA TUBE_R3_DATA		; fast Tube Bput
	JMP gbpb_incDataPtr
.gbpb_pb_fromhost
	JSR gpbp_B8memptr
	LDA (&B8,X)
	JMP gbpb_incDataPtr


IF _FASTGBPB_

\\ Workspace

mainws      =       (workspace%+&00)

dosram      =       mainws+$0060	;copy of OSGBPB/OSFILE ctrl block; temp filename in *CAT
acc         =       dosram+$000D	;temporary OSGBPB call number
ltemp0      =       dosram+$000E	;temporary count of bytes remaining to transfer
ldlow       =       mainws+$0072	;4 bytes; load address passed to OSFILE; Tube tx addr
dcby        =       mainws+$00C2	;channel workspace pointer for current open file
seqsem      =       mainws+$00DD	;$00=*SPOOL/*EXEC critical, close files on error

seqmap      =       mainws+$0100	;workspaces for channels $11..$15
seqcat      =       seqmap+$0000	;when accessing the catalogue entry
;seqlh       =       seqcat+$000D	;top bits exec/length/load/LBA in catalogue entry
;seqloc      =       seqcat+$000F	;LSB of starting LBA in catalogue entry
;seqpl       =       seqmap+$0010	;LSB of sequential pointer (PTR)
;channeldata_ptr+1       =       seqmap+$0011	;2MSB of sequential pointer
;channeldata_ptr+2       =       seqmap+$0012	;MSB of sequential pointer
;seqlma      =       seqmap+$0015	;2MSB of open file's extent
;seqlha      =       seqmap+$0016	;MSB of open file's extent
;seqdah      =       seqmap+$001D	;MSB of starting LBA

\\ Zeri Page

atemp       =       $00B4		;2 bytes
work        =       $00BA
wrkcat      =       work  +$0002	;load/exec/length/start sector in catalogue format
lodlo       =       work  +$0002	;LSB load address in OSFILE
lodhi       =       work  +$0003	;3MSB load address in OSFILE
lbahi       =       work  +$0008	;MSB LBA in OSFILE
lbalo       =       work  +$0009	;LSB LBA in OSFILE
lenlo       =       work  +$0006	;LSB file length in OSFILE
lenhi       =       work  +$0007	;2MSB file length in OSFILE

\\   JMP     LAE35		;raise "Illegal address" error

.fastgb
	JSR gbpb_wordB4_word107D;set up pointer to user's OSGBPB block
	LDA gbpbv_table3,Y	;get microcode byte from table
	AND #$03		;test bit 1 = transfer data
	LSR A			;set C=1 iff preserving PTR
	BEQ chain		;if not a transfer then pass call downstream
	STY acc			;else save call number
	LDY #$0C		;13 bytes to copy, $0C..$00:
.copyl0
	LDA (atemp),Y		;copy user's OSGBPB block
	STA dosram,Y		;to workspace
	DEY			;loop until 13 bytes copied
	BPL copyl0
	TAY			;file handle to Y
	LDX #$03		;4 bytes to copy, $03..$00:
.initl
	LDA dosram+$05,X	;copy L in OSGBPB block
	STA ltemp0,X		;to working L
	DEX			;loop until 4 bytes copied
	BPL initl
	LDA dosram+$09		;get LSB of P, initial PTR to use for transfer
	BCC dcptr		;if calls 1 or 3 then use P; else 2 or 4 use current PTR
	TYA			;convert file handle to workspace pointer
	JSR A_rolx5		;(file handle validated later)
	TAY			;to Y as offset
	LDA channeldata_ptr,Y		;get LSB of file pointer PTR
	CLC			;clear carry flag for two's complement
.dcptr
	EOR #$FF		;take two's complement
	ADC #$01		;=no. bytes from start of transfer to a page boundary
	LDY #$FC		;reverse counter, 4 bytes to set:
.hdrext					;do header/extender OSGBPB call
	STA dosram+$05-$FC,Y	;replace L field with number of bytes to move
	LDA #$00		;set MSB,2MSB,(3MSB) of user's L to zero
	INY			;increment offset
	BNE hdrext		;loop until 3 (4) bytes of L replaced
	JSR subwk		;subtract L from working L
	BCC trailr		;if underflow then within one sector, do trailer call
	BNE align		;else if remaining working L >= 256 then enter loop
.trailr					;else combine header/extender with trailer:
	JSR addtol		;add working L to L
.drain
	LDA #<dosram		;point to OSGBPB block in workspace
	STA atemp+$00		;-read user's block once
	LDA #>dosram		;upgbpb will write it back once
	STA atemp+$01
	LDY acc			;restore call number
.chain
	JMP upgbpb		;and pass call downstream

.morfst				;done a fast transfer, user's L = $00xxxx00
	LDA ltemp0+$03		;get MSB of working L
	JSR testl		;test MSB, 2MSB, 3MSB of working L
	BNE dofast		;if >= 256 then try another fast transfer
	STA dosram+$06		;else set user's L = 0
	STA dosram+$07
	LDA ltemp0+$00		;if 0 < working L < 256
	BNE trailr		;then do trailer call with L = working L
	CLC			;else C=0, no bytes remaining:
.fgbfin				;working L reached zero or something went wrong:
	PHP			;save carry flag that says which
	JSR addtol		;add remaining request to bytes not transferred
	PLP			;restore carry flag returned from OSGBPB call
	JSR gbpb_wordB4_word107D;set up pointer to user's OSGBPB block
	LDY #$0C		;copy 13 bytes of OSGBPB control block
.retnl
	LDA dosram,Y		;from DFS workspace
	STA (atemp),Y		;to user's address
	DEY			;loop until 13 bytes copied
	BPL retnl
	RTS			;return C=0 OSGBPB succeeded/C=1 OSGBPB failed

.setmax				;set fast transfer request = maximum transfer size
	STA dosram+$07		;set MSB request = MSB maximum
	ORA dosram+$06
	BNE sectr1		;if maximum > 0 then transfer sectors
.throw				;working L >= 256, L = $00xxxx00, maximum = 0
	LDY #$FD		;set user's L = 256
	LDA #$01
	BNE hdrext		;do extender OSGBPB call to extend file (always)

.align
	JSR drain		;call OSGBPB on workspace control block.
				;this validates the file handle, sets PTR from P,
				;aligns it to a sector boundary, and sets L=0
	BCS fgbfin		;return if call failed else continue transfer
.dofast				;working L >= 256, L = $00xxxx00, PTR on sector bdy
	LDX ltemp0+$03		;test working L - are there 16 MiB or more to move?
	BEQ sclamp		;if not then move 1..65535 sectors
	LDX #$FF		;else transfer first 65535 sectors of remainder
.sclamp
	TXA
	ORA ltemp0+$02
	STA dosram+$07		;set MSB of transfer length = 2MSB of L
	TXA
	ORA ltemp0+$01
	TAX			;hold LSB of transfer length in X
	LDA dcby		;get channel workspace offset
	SEC
	ADC acc			;add 1+call number, 2..5 to workspace offset
	EOR #$04		;bit 2 = 1 if writing
	AND #$E4		;if writing then point to allocation instead of EXT
	TAY
	LDA channeldata_ext+1,Y		;get 2MSB of channel EXT
	SEC
	SBC dosram+$0A		;subtract 2MSB of PTR
	STA dosram+$06		;=LSB maximum transfer size
	LDA channeldata_ext+2,Y		;get MSB of EXT
	SBC dosram+$0B		;subtract MSB of PTR = MSB maximum
	BCC throw		;if maximum<0 throw back
	CMP dosram+$07		;else compare MSB maximum - MSB request
	BCC setmax		;if maximum < request then request = maximum
	BNE sectr0		;if maximum > request then transfer sectors
	CPX dosram+$06		;else compare LSB request - LSB maximum
	BCS setmax		;if request >= maximum then request = maximum
.sectr0					;transfer one or more sectors.
	STX dosram+$06		;x=request, hold in 3MSB of L
.sectr1
	LDY dcby		;undo EXT/allocation fudge
	JSR Channel_SetDirDrv_GetCatEntry_Yintch ;ensure open file still in drive
	JSR ChannelBufferToDisk_Yintch		;ensure buffer up-to-date on disc L6
	LDA #$3F		;b7=0 buffer does not contain PTR, b6=0 buffer not changed
	STA channeldata_sectorinbuffer+1,Y		;set buffer LBA out of range to force re-reading
	JSR ChannelFlags_ClearBits		;clear b7,b6 of channel flags
	LDA dosram+$01		;copy OSGBPB transfer address
	STA lodlo		;to load address in OSFILE block
	LDA dosram+$02
	STA lodhi
	LDA dosram+$03
	STA ldlow+$02
	LDA dosram+$04
	STA ldlow+$03
	LDA channeldata_sector,Y ;get LSB LBA of start of open file
	CLC
	ADC dosram+$0A		;add 2MSB of PTR
	STA lbalo		;store LSB target LBA in OSFILE block
	LDA channeldata_mixedbyte,Y		;get MSB LBA
	ADC dosram+$0B		;add MSB of PTR
	AND #$03		;mask MSB of target LBA
	STA lbahi		;store MSB target LBA in OSFILE block
	LDA #$00		;clear LSB file length in OSFILE block
	STA lenlo
	LDA dosram+$06		;copy transfer length
	STA lenhi		;to file length in OSFILE block
	LDA dosram+$07		;get MSB transfer length
	JSR A_rolx4		;shift b1..b0 to b5..b4
	ORA lbahi		;combine with LSB target LBA
	STA wrkcat+$06		;pack into last byte of OSFILE block
	LDA acc			;(L8826 needs load+exec unpacked, but length packed)
	INC seqsem		;set *SPOOL/*EXEC critical flag (now $00)
	JSR docmd		;transfer ordinary file L5
	DEC seqsem		;clear *SPOOL/*EXEC critical flag (now $FF)
	JSR subwk		;subtract amount transferred from working L
	LDY dosram+$06		;get and hold LSB number of sectors transferred
	TYA
	CLC			;add to OSGBPB address field
	ADC dosram+$02
	STA dosram+$02
	LDX dosram+$07		;get and hold MSB number of sectors transferred
	TXA
	ADC dosram+$03		;add to OSGBPB address field
	STA dosram+$03
	BCC updp		;carry out to high byte
	INC dosram+$04
.updp
	TYA			;set A=LSB transfer size in sectors
	LDY dcby		;set Y=channel workspace offset
	CLC			;add to open file's pointer
	ADC channeldata_ptr+1,Y
	STA channeldata_ptr+1,Y		;update PTR
	STA dosram+$0A		;update OSGBPB control block in workspace
	TXA			;add MSB transfer size to MSB PTR
	ADC channeldata_ptr+2,Y
	STA channeldata_ptr+2,Y
	STA dosram+$0B		;(MSB OSGBPB P field cleared by upgbpb)
	TYA
	JSR CmpPTR		;compare PTR - EXT
	BCC doneit		;if file not extended then loop
	BEQ doneit		;if PTR = EXT, at EOF then loop
				;else PTR > EXT only possible if writing
	JSR updext		;clamp PTR to 0..EXT by raising EXT
.doneit
	JMP morfst		;loop to transfer more sectors

.subwk				;Subtract L from working L
	SEC			;set carry flag for subtract
	LDX #$FC		;reverse counter, 4 bytes to subtract
.subwkl
	LDA ltemp0-$FC,X	;get byte of working L
	SBC dosram+$05-$FC,X	;subtract byte of L in OSGBPB block
	STA ltemp0-$FC,X	;update byte of working L
	INX			;loop until 4 bytes updated:
	BNE subwkl
.testl				;Test whether working L >= 256
	ORA ltemp0+$02		;a=MSB, OR with 2MSB
	ORA ltemp0+$01		;or with 3MSB
	RTS			;return Z=1 iff working L < 256

.addtol				;add working L to L in OSGBPB block
	CLC			;clear carry flag for add
	LDX #$FC		;reverse counter, 4 bytes to add:
.addl
	LDA ltemp0-$FC,X	;get byte of working L
	ADC dosram+$05-$FC,X	;add to byte of L in OSGBPB block
	STA dosram+$05-$FC,X	;update byte of L
	INX			;loop until 4 bytes added
	BNE addl
	RTS

.docmd
	CMP #3
	BCC dowrcmd
.dowrcmd
	JMP LoadMemBlock        ; Commands 3/4 as reads
.dordcmd
	JMP SaveMemBlock        ; Command 1/2 are writes

ENDIF


.fscv_osabouttoproccmd
	BIT CMDEnabledIf1
	BMI parameter_fsp
	DEC CMDEnabledIf1
.parameter_fsp
	LDA #&FF
	STA workspace%+&CE
.param_out
	STA workspace%+&CD
	RTS

.parameter_afsp
	LDA #&2A	; "*"
	STA workspace%+&CE
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
	JMP osfile_savecat_retA_1

.osfile1_updatecat
	JSR CheckFileExists		; UPDATE CAT ENTRY
	JSR osfile_update_loadaddr_Xoffset
	JSR osfile_update_execaddr_Xoffset
	BVC osfile_updatelocksavecat ;  always

.osfile3_wrexecaddr
	JSR CheckFileExists		; WRITE EXEC ADDRESS
	JSR osfile_update_execaddr_Xoffset
	BVC osfile_savecat_retA_1 ; always

.osfile2_wrloadaddr
	JSR CheckFileExists		; WRITE LOAD ADDRESS
	JSR osfile_update_loadaddr_Xoffset
	BVC osfile_savecat_retA_1 ;always

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
	STA disccataloguebuffer%+&100+&08,X
	INY
	LDA (&B0),Y
	STA disccataloguebuffer%+&100+&09,X
	INY
	LDA (&B0),Y
	ASL A
	ASL A
	EOR disccataloguebuffer%+&100+&0E,X
	AND #&0C
	BPL osfile_savemixedbyte	; always

.osfile_update_execaddr_Xoffset
	JSR RememberAXY			; Update exec address
	LDY #&06
	LDA (&B0),Y
	STA disccataloguebuffer%+&100+&0A,X
	INY
	LDA (&B0),Y
	STA disccataloguebuffer%+&100+&0B,X
	INY
	LDA (&B0),Y
	ROR A
	ROR A
	ROR A
	EOR disccataloguebuffer%+&100+&0E,X
	AND #&C0
.osfile_savemixedbyte
	EOR disccataloguebuffer%+&100+&0E,X			; save mixed byte
	STA disccataloguebuffer%+&100+&0E,X
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
	EOR disccataloguebuffer%+&0F,X
	AND #&80
	EOR disccataloguebuffer%+&0F,X
	STA disccataloguebuffer%+&0F,X
	RTS

.CheckFileNotLocked
	JSR read_fspBA_findcatentry	; exit:X=Y=offset
	BCC ExitCallingSubroutine
.CheckFileNotLockedY
	LDA disccataloguebuffer%+&0F,Y
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
	JSR read_fspBA_reset_get_cat_firstentry80
	BCC checkexit
	TYA
	TAX 				; X=Y=offset
.SetParamBlockPointerB0
	LDA workspace%+&DB			; Ptr to OSFILE param block
	STA &B0
	LDA workspace%+&DC
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
	LDA #&8F
	JSR OSBYTE		; Issue service request &A

	JSR SetPrivateWorkspacePointerB0
	LDY #<ForceReset
	LDA #&FF
	STA (&B0),Y			; Data valid in SWS
	STA ForceReset
	INY
	STA (&B0),Y			; Set pws is "empty"
	RTS

.SetPrivateWorkspacePointerB0
	 				; Set word &B0 to
	LDA #&00
	STA &B0
	LDX PagedRomSelector_RAMCopy	; point to Private Workspace
	LDA PagedROM_PrivWorkspaces,X
IF NOT(_MASTER_)
	AND #&3F			; bits 7 & 6 are used as flags
ENDIF
	STA &B1
	RTS
ENDIF

;;.osbyteFF_startupopts
;;	LDA #&FF
.osbyte_X0YFF
	LDX #&00
.osbyte_YFF
	LDY #&FF
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

	\ OSFILE tables
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

.Channel_SetDirDrv_GetCatEntry_Yintch
	JSR Channel_SetDirDrive_Yintch
.Channel_GetCatEntry_Yintch
{
	LDX #&06			; Copy filename from
.chnl_getcatloop
	LDA channeldata_filename6_readonly,Y			; channel info to &C5
	STA &C5,X
	;DEY
	DEY
	DEX
	BPL chnl_getcatloop
	JSR get_cat_firstentry80fname
	BCC errDISKCHANGED		; If file not found
	STY workspace%+&C3			; ?&10C3=cat file offset
	LDY workspace%+&C2			; Y=intch
}
.chkdskchangexit
	RTS

.Channel_SetDirDrive_Yintch
	LDA channeldata_directory_locked,Y			; Directory
	AND #&7F
	STA DirectoryParam
	LDA channeldata_drive_flags,Y			; Drive
	JMP SetCurrentDrive_Adrive

.CheckForDiskChange
	JSR RememberAXY
	LDA disccataloguebuffer%+&100+&04
	JSR LoadCurDrvCat2
	CMP disccataloguebuffer%+&100+&04
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
}
	; Fall into ....

.CloseFiles_Yhandle
	TYA
	BEQ CloseAllFiles_Osbyte77	; If y=0 Close all files
	JSR CheckChannel_Yhndl_exYintch

.CloseFile_Yintch
{
	PHA 				; Save A
	TXA : PHA
	JSR IsHndlinUse_Yintch		;
	BCS closefile_exit		; If file not open
	LDA channeldata_channelbit,Y			; bit mask
	EOR #&FF
	AND workspace%+&C0
	STA workspace%+&C0			; Clear 'open' bit
	LDA channeldata_drive_flags,Y			; A=flag byte
	AND #&60
	BEQ closefile_exit		; If bits 5&6=0
	JSR Channel_SetDirDrv_GetCatEntry_Yintch
	LDA channeldata_drive_flags,Y			; If file extended and not
	AND #&20				; forcing buffer to disk
	BEQ closefile_buftodisk		; update the file length
	LDX workspace%+&C3			; X=cat offset
	LDA channeldata_ext+0,Y			; File lenth = EXTENT
	STA disccataloguebuffer%+&100+&0C,X			; Len lo
	LDA channeldata_ext+1,Y
	STA disccataloguebuffer%+&100+&0D,X			; Len mi
	LDA channeldata_ext+2,Y
	JSR A_rolx4			; Len hi
	EOR disccataloguebuffer%+&100+&0E,X			; "mixed byte"
	AND #&30
	EOR disccataloguebuffer%+&100+&0E,X
	STA disccataloguebuffer%+&100+&0E,X
	JSR SaveCatToDisk		; Update catalog
	LDY workspace%+&C2
.closefile_buftodisk
	JSR ChannelBufferToDisk_Yintch	; Restores Y
.closefile_exit
	PLA: TAX
	PLA 				; Restore A
	RTS
}

.findvnot0_openfile
{
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
	LDA #&00
	PLP
	BVC findv_createfile		; If not read only = write only
; A=0=file not found
	RTS 				; EXIT

.findv_createfile
	PHP 				; Clear data
	; A=0 BC-C3=0
	LDX #&07			; 1074-107B=0
.findv_loop1
	STA &BC,X
	STA workspace%+&74,X
	DEX
	BPL findv_loop1
	DEC &BE
	DEC &BF
	DEC workspace%+&76
	DEC workspace%+&77
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
	LDA channeldata_filename6_readonly,Y
	BPL errFILEOPEN			; If already opened for writing
	PLP
	PHP
	BMI errFILEOPEN			; If opening again to write
	JSR IsFileOpenContinue		; ** File can only be opened  **
	BCS findv_loop2			; ** once if being written to **
.findv_openchannel
	LDY workspace%+&C2			; Y=intch
	BNE SetupChannelInfoBlock_Yintch

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
	STA workspace%+&C4
.chnlblock_loop1
	LDA disccataloguebuffer%+&08,X			; Copy file name & attributes
	STA channeldata_filename,Y			; to channel info block
	LDA disccataloguebuffer%+&100+&08,X
	STA chenneldata_attributes,Y
	LDA #0
	STA channeldata_ptr,Y
	STA channeldata_ptr+8,Y
	INY
	INX
	DEC workspace%+&C4
	BNE chnlblock_loop1

	LDA workspace%+&C2			; A=intch
	TAY
	JSR A_rorx5
	ADC #HI(channeldata)
	STA channeldata_bufferpage,Y			; Buffer page
	LDA workspace%+&C1
	STA channeldata_channelbit,Y			; Mask bit
	ORA workspace%+&C0
	STA workspace%+&C0			; Set bit in open flag byte
	LDA channeldata_length,Y			; Length0
	ADC #&FF			; If Length0>0 C=1
	LDA channeldata_length+1,Y			; Length1
	ADC #&00
	STA channeldata_sectorcount,Y			; Sector count
	LDA channeldata_mixedbyte,Y			; Mixed byte
	ORA #&0F
	ADC #&00			; Add carry flag
	JSR A_rorx4and3			; Length2
	STA channeldata_sectorcount+1,Y
	PLP
	BVC chnlblock_setBit5		; If not read = write
	BMI chnlblock_setEXT		; If updating
	LDA #&80			; Set Bit7 = Read Only
	ORA channeldata_filename6_readonly,Y
	STA channeldata_filename6_readonly,Y
.chnlblock_setEXT
	LDA channeldata_length,Y			; EXTENT=file length
	STA channeldata_ext,Y
	LDA channeldata_length+1,Y
	STA channeldata_ext+1,Y
	LDA channeldata_mixedbyte,Y
	JSR A_rorx4and3
	STA channeldata_ext+2,Y
.chnlblock_cont
	LDA CurrentDrv			; Set drive
	ORA channeldata_drive_flags,Y
	STA channeldata_drive_flags,Y
	TYA 				; convert intch to handle
	JSR A_rorx5
	ORA #filehndl% 			; &10
	RTS 				; RETURN A=handle

.chnlblock_setBit5
	LDA #&20			; Set Bit5 = Update cat file len
	STA channeldata_drive_flags,Y			; when channel closed
	BNE chnlblock_cont		; always
}


.IsFileOpenContinue
	TXA 				; Continue looking for more
	PHA 				; instances of file being open
	JMP fop_nothisfile

.IsFileOpen_Yoffset
	LDA #&00
	STA workspace%+&C2
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
	BIT workspace%+&C0
	BEQ fop_channelnotopen		; If channel not open
	LDA channeldata_drive_flags,Y
	EOR CurrentDrv
	AND #&03
	BNE fop_nothisfile		; If not current drv?
.fop_cmpfn_loop
	LDA disccataloguebuffer%+&08,X			; Compare filename
	EOR channeldata_filename,Y
	AND #&7F
	BNE fop_nothisfile
	INX
	INY
	;INY
	DEC &B2
	BNE fop_cmpfn_loop
	SEC
	BCS fop_matchifCset		; always

.fop_channelnotopen
	STY workspace%+&C2			; Y=intch = allocated to new channel
	STA workspace%+&C1			; A=Channel Flag Bit
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
	LDA workspace%+&C0			; Force buffer save
	PHA 				; Save opened channels flag byte
	TYA 				; A=handle
	BNE chbuf1
	JSR CloseAllFiles
	BEQ chbuf2			; always
.chbuf1
	JSR CloseFiles_Yhandle
.chbuf2
	PLA 				; Restore
	STA workspace%+&C0
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
	CMP #&04
	BCS argsv_exit			; If A>=3
	JSR ReturnWithA0
	CMP #&03
	BEQ argsv3
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
	LDA workspace%+&D9			; (see *run code)
	STA &00,X
	LDA workspace%+&DA
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
	STY workspace%+&C2
	ASL A				; A becomes 0 or 4
	ADC workspace%+&C2
	TAY
	LDA channeldata_ptr+0,Y
	STA &00,X
	LDA channeldata_ptr+1,Y
	STA &01,X
	LDA channeldata_ptr+2,Y
	STA &02,X
	LDA #&00
	STA &03,X
	RTS

; args 3
; Change EXT of file
;
; if new EXT > old EXT pad file with zeros
; if new EXT <= old EXT truncate file and ensure ptr in <= new EXT

.argsv3
{
	JSR CheckChannel_Yhndl_exYintch
	JSR cmptoEXT
	BCS truncate ; if new EXT <= EXT
	LDA channeldata_ptr+0,Y	; save current ptr
	PHA
	LDA channeldata_ptr+1,Y
	PHA
	LDA channeldata_ptr+2,Y
	PHA
	JSR argsextendloop
	PLA
	STA channeldata_ptr+2,Y
	PLA
	STA channeldata_ptr+1,Y
	PLA
	STA channeldata_ptr+0,Y
	JSR IsSeqPointerInBuffer_Yintch
.truncate
	LDA channeldata_filename6_readonly,Y
	BMI filereadonly
	ORA channeldata_directory_locked,Y
	BMI filelocked
	LDA #&20
	JSR ChannelFlags_SetBits

	LDA 0,X		; COPY new EXT to EXT
	STA channeldata_ext+0,Y
	LDA 1,X
	STA channeldata_ext+1,Y
	LDA 2,X
	STA channeldata_ext+2,Y
	TXA			; save Args block
	PHA
	JSR TYA_CmpPTR
	PLA
	TAX
	BCC dontchangeptr; PTR<EXT
	JSR SetSeqPointer_Yintch
.dontchangeptr
	LDA #&EF ; Clear b4
	JMP ChannelFlags_ClearBits

.filereadonly
	JMP errFILEREADONLY

.filelocked
	JMP errFILELOCKED

}

.IsHndlinUse_Yintch
{
	TYA
	AND #&E0
	STA workspace%+&C2			; Save intch
	BEQ hndlinuse_notused_C1
	JSR A_rorx5			; ch.1-7
	TAY				; creat bit mask
	LDA #&00			; 1=1000 0000
	SEC				; 2=0100 0000 etc
.hndlinsue_loop
	ROR A
	DEY
	BNE hndlinsue_loop
	; Carry = 0
	LDY workspace%+&C2			; Y=intch
	BIT workspace%+&C0			; Test if open
	BNE hndlinuse_used_C0
.hndlinuse_notused_C1
	SEC
.hndlinuse_used_C0
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
	CPY workspace%+&C2			; Owner?
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
	JSR CheckChannel_Yhndl_exYintch_TYA_CmpPTR		; X=Y
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
	PHA
	JSR IsHndlinUse_Yintch
	PLA
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
	JSR CheckChannel_Yhndl_exYintch_TYA_CmpPTR	; A=Y
	BEQ	bg_EOF 				; If PTR=EXT

	LDA channeldata_drive_flags,Y
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

.bg_EOF
	LDA channeldata_drive_flags,Y			; Already at EOF?
	AND #&10
	BNE errEOF			; IF bit 4 set
	LDA #&10
	JSR ChannelFlags_SetBits	; Set bit 4
;	LDX workspace%+&C5 ; no idea what this is doing as XY are preserved
	LDA #&FE 		   ; A is undefined when EOF
	SEC
	RTS 				; C=1=EOF
}

.CalcBufferSectorForPTR
	CLC
	LDA channeldata_sector,Y			; Start Sector + Seq Ptr
	ADC channeldata_ptr+1,Y
	STA &C3
	STA channeldata_sectorinbuffer,Y			; Buffer sector
	LDA channeldata_mixedbyte,Y
	AND #&03
	ADC channeldata_ptr+2,Y
	STA &C2
	STA channeldata_sectorinbuffer+1,Y

.ChannelFlags_SetBit7
	LDA #&80			; Set/Clear flags (C=0 on exit)
.ChannelFlags_SetBits
	ORA channeldata_drive_flags,Y
	BNE chnflg_save
.ChannelFlags_ClearBit7
	LDA #&7F
.ChannelFlags_ClearBits
	AND channeldata_drive_flags,Y
.chnflg_save
	STA channeldata_drive_flags,Y
	CLC
	RTS

.ChannelBufferToDisk_Yintch
	LDA channeldata_drive_flags,Y
	AND #&40			; Bit 6 set?
	BEQ chnbuf_exit2		; If no exit
	CLC 				; C=0=write buffer

.ChannelBufferRW_Yintch_C1read
{
	INC workspace%+&DD			; Remember in case of error?
	LDY workspace%+&C2			; Setup NMI vars
	LDA channeldata_bufferpage,Y			; Buffer page
	STA &BD				;Data ptr
	LDA #&FF			; \ Set load address to host
	STA workspace%+&74			; \
	STA workspace%+&75			; \
	LDA #&00
	STA &BC
	STA &C0				; Sector
	LDA #&01
	STA &C1
	BCS chnbuf_read			; IF c=1 load buffer else save
	LDA channeldata_sectorinbuffer,Y			; Buffer sector
	STA &C3				; Start sec. b0-b7
	LDA channeldata_sectorinbuffer+1,Y
	STA &C2				; "mixed byte"
	JSR SaveMemBlock
	LDY workspace%+&C2			; Y=intch
	LDA #&BF			; Clear bit 6
	JSR ChannelFlags_ClearBits
	BCC chnbuf_exit			; always
.chnbuf_read
	JSR CalcBufferSectorForPTR	; sets NMI data ptr
	JSR LoadMemBlock		; Load buffer
.chnbuf_exit
	DEC workspace%+&DD
	LDY workspace%+&C2			; Y=intch
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
	LDA channeldata_filename6_readonly,Y
	BMI errFILEREADONLY
	LDA channeldata_directory_locked,Y
	BMI errFILELOCKED2
	JSR Channel_SetDirDrive_Yintch
	TYA
	CLC
	ADC #channeldata_ext-channeldata_ptr
	JSR CmpPTR
	BNE bp_noextend			; If PTR<>Sector Count, i.e Ptr<sc
	JSR Channel_GetCatEntry_Yintch	; Enough space in gap?
	LDX workspace%+&C3			; X=cat file offset
	SEC 				; Calc size of gap
	LDA disccataloguebuffer%+&100+&07,X			; Next file start sector
	SBC disccataloguebuffer%+&100+&0F,X			; This file start
	PHA 				; lo byte
	LDA disccataloguebuffer%+&100+&06,X
	SBC disccataloguebuffer%+&100+&0E,X			; Mixed byte
	AND #&03			; hi byte
	CMP channeldata_sectorcount+1,Y			; File size in sectors
	BNE bp_extendby100		; If must be <gap size
	PLA
	CMP channeldata_sectorcount+0,Y
	BNE bp_extendtogap		; If must be <gap size
	STY &B4				; Error, save intch handle
	STY workspace%+&C2			; for clean up
	JSR ClearEXECSPOOLFileHandle

	JSR ReportErrorCB
	EQUB &BF
	EQUS "Can't extend",0

.bp_extendby100
	LDA channeldata_sectorcount+1,Y			; Add maximum of &100
	CLC 				; to sector count
	ADC #&01			; (i.e. 64K)
	STA channeldata_sectorcount+1,Y			; [else set to size of gap]
	ASL A				; Update cat entry
	ASL A
	ASL A
	ASL A
	EOR disccataloguebuffer%+&100+&0E,X			; Mixed byte
	AND #&30
	EOR disccataloguebuffer%+&100+&0E,X
	STA disccataloguebuffer%+&100+&0E,X			; File len 2
	PLA
	LDA #&00
.bp_extendtogap
	STA disccataloguebuffer%+&100+&0D,X			; File len 1
	STA channeldata_sectorcount+0,Y
	LDA #&00
	STA disccataloguebuffer%+&100+&0C,X			; File len 0
	JSR SaveCatToDisk
	LDY workspace%+&C2			; Y=intch
.bp_noextend
	LDA channeldata_drive_flags,Y
	BMI bp_savebyte			; If PTR in buffer
	JSR ChannelBufferToDisk_Yintch	; Save buffer
	LDA channeldata_ext+0,Y			; EXT byte 0
	BNE bp_loadbuf			; IF <>0 load buffer
	JSR TYA_CmpPTR			; A=Y
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
	JSR TYA_CmpPTR
	BCC bp_exit			; If PTR<EXT
IF _FASTGBPB_
.*updext
ENDIF
	LDA #&20			; Update cat file len when closed
	JSR ChannelFlags_SetBits	; Set bit 5
	LDX #&02			; EXT=PTR
.bp_setextloop
	LDA channeldata_ptr+0,Y
	STA channeldata_ext+0,Y
	INY
	DEX
	BPL bp_setextloop
}
.bp_exit
	RTS


.cmptoEXT
	LDA channeldata_ext+0,Y			; Compare ctl blk ptr
	CMP &00,X			; to existing EXT
	LDA channeldata_ext+1,Y			; Z=1 if same
	SBC &01,X			; (ch.1=&1138)
	LDA channeldata_ext+2,Y
	SBC &02,X
	RTS

.argsv_WriteSeqPointer

	JSR RememberAXY			; Write Sequential Pointer
	JSR CheckChannel_Yhndl_exYintch	; (new ptr @ 00+X)

.wsploop
	JSR cmptoEXT
	 				; C=p>=n
	BCS SetSeqPointer_Yintch	; If EXT >= new PTR

.argsextendloop
	LDA channeldata_ext+0,Y			; else new PTR>EXT so pad with a 0
	STA channeldata_ptr+0,Y
	LDA channeldata_ext+1,Y			; first, actual PTR=EXT
	STA channeldata_ptr+1,Y
	LDA channeldata_ext+2,Y
	STA channeldata_ptr+2,Y
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

.SetSeqPointer_Yintch
	LDA &00,X			; Set Sequential Pointer
	STA channeldata_ptr+0,Y		; PTR #
	LDA &01,X
	STA channeldata_ptr+1,Y
	LDA &02,X
	STA channeldata_ptr+2,Y

.IsSeqPointerInBuffer_Yintch
	LDA #&6F			; Clear bits 7 & 4 of 1017+Y
	JSR ChannelFlags_ClearBits
	LDA channeldata_sector,Y			; Start sector
	ADC channeldata_ptr+1,Y			; Add sequ.ptr
	STA workspace%+&C4
	LDA channeldata_mixedbyte,Y			; Mixed byte
	AND #&03			; Start sector bits 8&9
	ADC channeldata_ptr+2,Y
	CMP channeldata_sectorinbuffer+1,Y
	BNE bp_exit
	LDA workspace%+&C4
	CMP channeldata_sectorinbuffer,Y
	BNE bp_exit
	JMP ChannelFlags_SetBit7	; Seq.Ptr in buffered sector

.CheckChannel_Yhndl_exYintch_TYA_CmpPTR
	JSR CheckChannel_Yhndl_exYintch
.TYA_CmpPTR
	TYA
.CmpPTR
	TAX
	LDA channeldata_ptr+2,Y ; PTR#
	CMP channeldata_ext+2,X ; EXT#
	BNE cmpPE_exit
	LDA channeldata_ptr+1,Y
	CMP channeldata_ext+1,X
	BNE cmpPE_exit
	LDA channeldata_ptr+0,Y
	CMP channeldata_ext+0,X
.cmpPE_exit
	RTS

\ DMB: Factor out some common code from BPUT/BGET

.load_then_incSeqPtr_Yintch
{
	LDA channeldata_ptr,Y			; Seq.Ptr
	STA &BA
	LDA channeldata_bufferpage,Y			; Buffer page
	STA &BB
	TYA
	TAX
	INC channeldata_ptr+0,X			; Seq.Ptr+=1
	BNE samesector
	JSR ChannelFlags_ClearBit7	; PTR in new sector!
	INC channeldata_ptr+1,X
	BNE samesector
	INC channeldata_ptr+2,X
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
	JSR PrintNewLine
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

	STA &AE			; Source Drive No.
	STA &AF			; Dest Drive No.
	JSR PrintNibble
	JSR PrintNewLine
	LDY #&00
	JSR CloseFiles_Yhandle		; Close all files ; BUG fix
	JSR CalcRAM
	JSR LoadCurDrvCat2		; Load catalogue
	LDY FilesX8
	STY &CA				; ?CA=file offset
	LDA #&02
	STA &AC
	LDA #&00
	STA &AD				; word C8=next free sector
.compact_loop
	LDY &CA
	JSR Y_sub8
	CPY #&F8
	BNE compact_checkfile		; If not end of catalogue
	LDA disccataloguebuffer%+&100+&07			; Calc & print no. free sectors
	SEC 				; (disk sectors - word C8)
	SBC &AC
	PHA
	LDA disccataloguebuffer%+&100+&06
	AND #&03
	SBC &AD
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
	LDA disccataloguebuffer%+&100+&0C,Y			; A=Len0
	CMP #&01			; C=sec count
	LDA #&00
	STA &BC
	STA &C0
	ADC disccataloguebuffer%+&100+&0D,Y			; A=Len1
	STA &A8
	LDA disccataloguebuffer%+&100+&0E,Y
	PHP
	JSR A_rorx4and3			; A=Len2
	PLP
	ADC #&00
	STA &A9				; word C4=size in sectors
	LDA disccataloguebuffer%+&100+&0F,Y			; A=sec0
	STA &AA
	LDA disccataloguebuffer%+&100+&0E,Y
	AND #&03			; A=sec1
	STA &AB				; word C6=sector
	CMP &AD				; word C6=word C8?
	BNE compact_movefile		; If no
	LDA &AA
	CMP &AC
	BNE compact_movefile		; If no
	CLC
	ADC &A8
	STA &AC
	LDA &AD
	ADC &A9
	STA &AD				; word C8 += word C4
	JMP compact_fileinfo

.compact_movefile
	LDA &AC				; Move file
	STA disccataloguebuffer%+&100+&0F,Y			; Change start sec in catalogue
	LDA disccataloguebuffer%+&100+&0E,Y			; to word C8
	AND #&FC
	ORA &AD
	STA disccataloguebuffer%+&100+&0E,Y

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
.isgoalready
	RTS
}
ENDIF

.Get_CopyDATA_Drives
{
	JSR Param_DriveNo_Syntax	; Get drives & calc ram & msg
	STA &A8			; Source drive
	JSR Param_DriveNo_Syntax
	STA &A9			; Destination drive

	CMP &A8
	BEQ baddrv			; Drives must be different!
	TYA
	PHA
	JSR CalcRAM			; Calc ram available
	JSR PrintString			; Copying from:
	EQUS "Copying from :"
	LDA &A8
	JSR PrintNibble_PrintString			; to :
	EQUS " to :"
	LDA &A9
	JSR PrintNibble
	JSR PrintNewLine
	PLA
	TAY
	RTS

.baddrv
	JMP errBADDRIVE
}

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

.osbyte0F_flushinbuf2
	JSR RememberAXY
	LDA #&0F
	LDX #&01
	LDY #&00
	JMP OSBYTE


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
	STA &AB
	STA &AD
	STA &AC
	STA &AA

	\ Source
	LDA &A8
	STA CurrentDrv
	JSR LoadCurDrvCat
	LDA disccataloguebuffer%+&100+&07			; Size of source disk
	STA &AE				; Word C4 = size of block
	LDA disccataloguebuffer%+&100+&06
	AND #&03
	STA &AF

	\ Destination
	LDA &A9
	STA CurrentDrv
	JSR LoadCurDrvCat
	LDA disccataloguebuffer%+&100+&06			; Is dest disk smaller?
	AND #&03
	CMP &AF
	BCC err_DISKFULL2
	BNE backup_copy
	LDA disccataloguebuffer%+&100+&07
	CMP &AE
	BCC err_DISKFULL2

.backup_copy

	JSR CopyDATABLOCK
	JSR LoadCurDrvCat

	\ Update title in disk table

	LDX #&0A
.tloop
	CPX #&08
	BCC tskip1
	LDA disccataloguebuffer%+&F8,X
	BCS tskip2
.tskip1
	LDA disccataloguebuffer%+&00,X
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
	; fail in to SaveDiskTable
}
ENDIF

IF NOT(_MM32_)
.SaveDiskTable

IF NOT(_LARGEMMB_)
	LDA CurrentCat
ENDIF

	JSR DiskTableSec
	JMP MMC_WriteCatalogue
ENDIF

IF _INCLUDE_CMD_COPY_
.CMD_COPY
{
	; AE AF used in printstring

	;JSR parameter_afsp ; &10CD = `#` &10CE =`*`
	JSR Get_CopyDATA_Drives ; &A8 = source drive : &A9 destination drive
	JSR parameter_afsp_Param_SyntaxErrorIfNull_read_fspTextPointer; &1000 = filename
	\ Source
	LDA &A8 ; Already ranged checked drive number
	STA CurrentDrv
	JSR getcatentry ; check if source file exists filname @ &1000
					; Returns Y and &B6=Y+8
.copy_loop1
	LDA DirectoryParam
	PHA
	LDA &B6	 ; setup by getcatentry pointer to next file.
	PHA
	JSR prt_InfoLine_Yoffset
	LDX #&00
.copy_loop2
	LDA disccataloguebuffer%+&08,Y
	STA &C5,X  ; store filename &C5-&CC

	LDA disccataloguebuffer%+&100+&08,Y
	STA &BC,X	; load address, exec .... &BC- &C3

	INX
	INY
	CPX #&08
	BNE copy_loop2

	LDA &C2 ; get high bits
	JSR A_rorx4and3 ; isolate length top two bits ( bits 16 and 17)
	TAX

	LDA &C0		; load bits 7-0	 of length
	CMP #&1 ; C = 1 if file includes Partial sector
				; round up number of sectors required
	LDA &C1		; load bits 15-8 of length
	ADC #&00
	STA &AE

	TXA

	ADC #&00
	STA &AF

	LDA &C3 ; get start sector bits 7-0
	STA &AA
	LDA &C2 ; get start sector bits 9-8
	AND #&03
	STA &AB

; create file in destination catalogue

	LDA &A9			; destination drive

	JSR CreateFile_3		; Saves cat. ( pass in Drive)

	LDA &C2				; Remember sector
	AND #&03
	STA &AD
	LDA &C3
	STA &AC

	JSR CopyDATABLOCK

	\ Source
	LDA &A8
	STA CurrentDrv
	JSR LoadCurDrvCat2
	PLA
	STA &B6	; restore next file pointer
	PLA
	STA DirectoryParam
	JSR get_cat_nextentry
	BCS copy_loop1
	RTS
}
ENDIF

IF _INCLUDE_CMD_BACKUP_ OR _INCLUDE_CMD_COMPACT_ OR _INCLUDE_CMD_COPY_
.CopyDATABLOCK
{
; Entry
;  &AE &AF Size in sectors
;  &AA &AB Start sector
;  &AC &AD destination sector
;  &A8 source drive
;  &A9 destination drive

; ZP Usage
; &BC &BD start addres of buffer
; &C0 always zero ( bytes in last sector)
; &C1 ; number of sectors to copy limited by ram size
; &C2 &C3 ; first sector of current block to read or write
; &AE &AF ; number of sectors left to copy ( because we have more sectors than ram)
; &AA &AB ; Start source sector ( local )
; &AC &AD ; next free sector for destination (local)
; uses RAM for copying ( PAGE to HIMEM corrupt)

	LDA #&00			; *** Move or copy sectors
	STA &BC				; Word &AE = size of block
	STA &C0
	LDA PAGE			; Buffer address
	STA &BD
	BEQ cd_loopentry		; always
.cd_loop
	LDY &AE
	CPY RAMBufferSize		; Size of buffer
	LDA &AF
	SBC #&00
	BCC cd_part			; IF size<size of buffer
	LDY RAMBufferSize
.cd_part
	STY &C1				; number of sectors to copy in this pass

	LDA &AA				; C2/C3 = Block start sector
	STA &C3				; Start sec = Word C6
	LDA &AB
	STA &C2

	LDA &A8		; Source drive
	STA CurrentDrv

	\ Source
	JSR SetLoadAddrToHost ; &1074 = &1075 = 255
	JSR LoadMemBlock    ; pass in BC BD C2 C3, C1 C0

	LDA &A9		; desination drive
	STA CurrentDrv

	LDA &AC				; C2/C3 = Block start sector
	STA &C3				; Start sec = Word C8
	LDA &AD
	STA &C2

	\ Destination
	JSR SetLoadAddrToHost ; &1074 = &1075 = 255
	JSR SaveMemBlock

	LDA &C1				; Word C8 += ?C1
	CLC 				; Dest sector start
	ADC &AC
	STA &AC
	BCC cd_inc1
	INC &AD
.cd_inc1

	LDA &C1				; Word C6 += ?C1
	CLC 				; Source sector start
	ADC &AA
	STA &AA
	BCC cd_inc2
	INC &AB
.cd_inc2

	SEC	 			; Word C4 -= ?C1
	LDA &AE				; Sector counter
	SBC &C1
	STA &AE
	BCS cd_loopentry
	DEC &AF
.cd_loopentry

	LDA &AE
	ORA &AF
	BNE cd_loop			; If Word C4 <> 0
	RTS
}
ENDIF

IF _INCLUDE_CMD_FORM_VERIFY_
.CMD_VERIFY
	CLC			; \\\\\ *VERIFY
	BCC vform1

.CMD_FORM
	SEC			; \\\\\ *FORM
.vform1
;;{
{
	ROR &AD
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
	BIT &AD
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
	BIT &AD
	BMI vf1				; If formatting

IF NOT(_MM32_)
	JSR CheckCurDrvFormatted
ENDIF
	JSR PrintString
	EQUS "Verifying"
	BCC vf2				; always

.vf1
IF NOT(_MM32_)
	SEC
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
	BIT &AD
	BMI vf4				; If formatting

	\ If verifying calc. no. of tracks
	; reads catalogue, pops drive

	\ * Calc. no. of tracks on disk in curdrv *
{
	JSR LoadCurDrvCat		; Load catalogue
	LDA disccataloguebuffer%+&100+&06			; Size of disk
	AND #&03
	TAX
	LDA disccataloguebuffer%+&100+&07
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
}

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
	LDX #0
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

	BIT &AD
	BPL vf6_exit			; If verifying

	\ Save new catalogue
IF NOT(_MM32_)
	JSR MarkDriveAsFormatted
ENDIF

	JSR ClearCatalogue

	LDY #&01
	LDX #&90

	LDA &B5				; 40 or 80 tracks
	CMP #40
	BEQ vf8
.vf7
	LDY #&03
	LDX #&20
.vf8
	STX MA+&F07			; Disk size in sectors
	STY MA+&F06

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
	STA disccataloguebuffer%+&00,Y
	STA disccataloguebuffer%+&100+&00,Y
	INY
	BNE ccatloop

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
	; ZP usage
	; &AA Bit 7 Selects between *FREE and * MAP
	; &A8 &A9 ; total number of sectors on disk
	; &AA-&AF used as digit buffer
	; &BB &BC
	; &BD
	; &BF &C0
	; &C1 &C2
	;

	ROR &AA
	JSR Param_OptionalDriveNo
	JSR LoadCurDrvCat2
	BIT &AA
	BMI Label_A818_free		; If *FREE
	JSR PrintStringSPL
	EQUS "Address :  Length",13	; "Address : Length"
.Label_A818_free
	LDA disccataloguebuffer%+&100+&06
	AND #&03			; get high bits of total number of sectors
	STA &A9
	STA &C2
	LDA disccataloguebuffer%+&100+&07		; LSB of total number of sectors
	STA &A8				; wC4=sector count

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
	AND #&F8			; shouldn't be needed
	TAY
	BEQ Label_A86B_nofiles		; If no files
	BNE Label_A856_fileloop_entry	; always

.Label_A845_fileloop
	JSR Sub_A8E2_nextblock
	JSR Y_sub8			; Y -> next file
	LDA &A8
	SEC
	SBC &BB
	LDA &A9
	SBC &BC
	BCC Label_A86B_nofiles

.Label_A856_fileloop_entry
	LDA disccataloguebuffer%+&100+&07,Y			; wC1 = File Start Sec - Map addr
	SEC
	SBC &BB
	STA &C1
	LDA disccataloguebuffer%+&100+&06,Y			; high bits of sector
	AND #&03
	SBC &BC
	STA &C2
	BCC Label_A845_fileloop

.Label_A86B_nofiles
	STY &BD
	BIT &AA
	BMI Label_A87A_free		; If *FREE
	LDA &C1				; MAP only
	ORA &C2
	BEQ Label_A87A_free		; If wC1=0

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
	JSR OSNEWL

	JSR Sub_A8E2_nextblock

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

	BIT &AA
	BPL Label_A8BD_rst		; If *MAP

	TAY
	LDX &BF

	LDA #&F8				; max number of files *8
	SEC
	SBC FilesX8				; Actual number of files *8

	JSR Sub_A90D_freeinfo
	EQUS "Free",13			; "Free"
	LDA &A8

	SEC
	SBC &BF
	TAX
	LDA &A9
	SBC &C0

	TAY
	LDA FilesX8
	JSR Sub_A90D_freeinfo
	EQUS "Used",13			; "Used"
	NOP
.Label_A8BD_rst
	RTS



.Sub_A8E2_nextblock
{
	LDA disccataloguebuffer%+&100+&06,Y			; wBB = start sec + len
	PHA
	JSR A_rorx4and3
	STA &BC
	PLA
	AND #&03
	CLC
	ADC &BC
	STA &BC
	LDA disccataloguebuffer%+&100+&04,Y
	CMP #1		; carry C=0 if whole sector
	LDA #0
	ADC FilesX8,Y
	BCC Label_A902
	INC &BC
.Label_A902
	CLC
	ADC disccataloguebuffer%+&100+&07,Y
	STA &BB
	BCC Label_A90C
	INC &BC
.Label_A90C
	RTS
}

.Sub_A90D_freeinfo
{
	; YX number of sectors
	; A number of files *8
	; ZP usage
	; &BB &BC &BD ; used digit buffer
	; &BE &C1 was used
	; use AA for string buffer ( 6 bytes)

numberofdecdigits%=6
numberofbits%=24
digitbuffer% = &AA
								; *FREE line
	LSR A					; Divide by 8 to file the number of files
	LSR A
	LSR A
	JSR PrintBCDSPL			; A = Number of files
	JSR PrintStringSPL
	EQUS " Files "

	STX &BC					; YX = Number of sectors
	STY &BD

	TYA
	JSR PrintNibbleSPL		; high nibble of sector count
	TXA
	JSR PrintHexSPL		;	 low byte of sector count
	JSR PrintStringSPL
	EQUS " Sectors "

	LDA #&00				; create a 3 byte word of sectors*256
	STA &BB

	LDY #numberofbits%-1

	LDX #numberofdecdigits%-1	; clear digits
.Label_A941_loop1
	STA digitbuffer%,X		; A = 0
	DEX
	BPL Label_A941_loop1

.Label_A947_loop2
	ASL &BB					; !BB = !BB * 2
	ROL &BC
	ROL &BD

	LDX #numberofdecdigits%-1	; loop digit times
.Label_A953_loop3
	LDA digitbuffer%,X
	ROL A					; *2 + carry
	CMP #&0A
	BCC Label_A95D			; If <10
	SBC #&0A
.Label_A95D
	STA digitbuffer%,X
	DEX
	BPL Label_A953_loop3
	DEY
	BPL Label_A947_loop2

; Print decimal string
	LDY #&20				; print space for leading zero suppression
	INX 					; X = 0
.Label_A96C_loop4
	LDA digitbuffer%,X
	BNE Label_A97D			; non zero so print

	CPX #numberofdecdigits%-1
	BEQ printzero			; print last zero

.Label_A970

	CPY #&2C
	BEQ printzero			; if we have printed non zero then always print
	TYA						; print space
	BNE printchar			; always

.Label_A97D
	LDY #&2C 				; ","
.printzero
	ORA #&30
.printchar
	JSR OSWRCH
	CPX #numberofdecdigits%-1-3	; if we are at the 4th digit
	BNE Label_A98D
	TYA
	JSR OSWRCH				; Print " " or ","

.Label_A98D
	INX
	CPX #numberofdecdigits%
	BNE Label_A96C_loop4

	JSR PrintStringSPL
	EQUS " Bytes "
	NOP
	JMP PrintStringSPL
}
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
	extraspace = disccataloguebuffer%+&00 - P%
	SKIPTO disccataloguebuffer%+&00
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
cmdseq%=workspace%+&87
par%=workspace%+&89

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
IF NOT(_MM32_)
.storeDRIVE_INDEX4_ResetCRC7
	STA DRIVE_INDEX4,X
ENDIF
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
	LDY #&A
.loop
	LDA filemmb,Y
	STA fatfilename%,Y
	DEY
	BPL loop

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
	LDA disccataloguebuffer%+&08		; new MMB size byte A0..AF
	EOR #&A0		; 00..0F
	CMP #&10		; support upto 16x 511 disks
	BCS skip		; skip TAX if out of range
	TAX 			; accept the value
.skip
	INX			; 01..10
	STX NUM_CHUNKS		; save in CRC protected area

	\\ Process the new MMB chunk base byte
	LDX #&00
	LDA disccataloguebuffer%+&09		; new MMB chunk base byte
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

	\\ * Check drive loaded with formatted disk *
.CheckCurDrvFormatted
	CLC
.CheckCurDrvUnformatted
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
	JSR MMC_BEGIN1
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
	\\ Note: Values in BC-C5 copied to 1090-1099
	\\ Also checks disk loaded/formatted
.CalcRWVars
{
	JSR DiskStart

	\\ add start sector on disk
	CLC
	LDA workspace%+&97 ; was C3
	ADC sec%
	STA sec%
	LDA workspace%+&96 ; was c2
	AND #3
	PHA
	ADC sec%+1
	STA sec%+1
	BCC cvskip
	INC sec%+2

	\\ calc sector count
.cvskip
	LDA workspace%+&95			; was C1
	STA seccount%
	LDA workspace%+&96			; C2 mixed byte
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
	LDA workspace%+&94			; was C0 bytes in last sector
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
	LDA workspace%+&97     ; was c3
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
	b=workspace%+&90
ENDIF

	LDA workspace%+&74
	AND workspace%+&75
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

.Set_CurDirDrv_ToDefaults_CheckCurDrvCat
	JSR Set_CurDirDrv_ToDefaults
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
	JSR DiskStart
	JSR MMC_ReadCatalogue

.rwcatexit
	LDA CurrentDrv
	STA CurrentCat
	JMP MMC_END
ENDIF

	\\ **** Save catalogue of current drive ****
.SaveCatToDisk
	LDA disccataloguebuffer%+&100+&04			; Increment Cycle Number
	CLC
	SED
	ADC #&01
	STA disccataloguebuffer%+&100+&04
	CLD

IF _MM32_
	LDA #&4B
	;BNE exec_cat_rw
ELSE
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

	JSR OW7F_Execute_and_ReportIfDiskFault

	LDA CurrentDrv
	STA CurrentCat
	RTS

.datax	EQUB &00, HI(disccataloguebuffer%), &FF, &FF, &03, &53, &00, &00, &22
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

.OW7F_Execute_and_ReportIfDiskFault
	JSR OW7F_Execute
\\ Report 'disk fault'
\\ On entry: A=FDC result, Z=1 if A=0
.ReportIfDiskFault
{
	;trk = &CA
	;sec = &CB

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
	\ ?&AD : -ve = write, +ve = read
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
	BIT &AD
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
	ADC #HI(disccataloguebuffer%)     ;  0 :  0  0  0  0  1  1  1 M4
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
IF 	(disccataloguebuffer%) AND 1
	CLC
	ADC #HI(disccataloguebuffer%)
ELSE
	ORA #HI(disccataloguebuffer%)
ENDIF
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
	\\ *** Set word &B8 to disk in current drive ***
	\\ Check: C=0 drive loaded with formatted disk
	\\        C=1 drive loaded with unformatted disk
;.SetCurrentDiskC
	JSR chkdrv1
	LDA DRIVE_INDEX0,X
	STA &B8
	LDA DRIVE_INDEX4,X
	MASK_DISKNO
	STA &B9

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
	\\ **** If disk in any drive, unload it ****
	\\ Word &B8=diskno (X,Y preserved)
	\\ Doesn't check/update CRC7
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

}
	PLA
	TAX
	LDA &B8
	STA DRIVE_INDEX0,X
	LDA &B9
	ORA &B7	; Loaded
	JMP storeDRIVE_INDEX4_ResetCRC7
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
ELSE

	\\ A=sector code (sector or &80)
.CheckDiskTable
	CMP CurrentCat
	BEQ ldtloaded

	\\ A=sector code
.LoadDiskTable

ENDIF
	STA CurrentCat
	JSR DiskTableSec
	JMP MMC_ReadCatalogue


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
	PLA
	JSR PrintString		; preserves AXY
	EQUS " disc"
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
	EOR #HI(disccataloguebuffer%) EOR (HI(disccataloguebuffer%+&100))
	STA gdptr%+1
	ROR A
IF 	(HI(disccataloguebuffer%)) AND 1
	BCC gdx1
ELSE
	BCS gdx1
ENDIF
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
.bcd_inc16_zp_x_A8
	LDX #&A8
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
read16sec%=&B4	; 3 byte sector value
read16str%=tempbuffer%+&00
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
	;JSR MMC_ReadDiscTitle

		\\ *** Read the disc title to read16str% ***
	\\ *** read16sec% contains the address   ***
	\\ *** of the first disc sector          ***

	JSR SetLEDS
	LDA #0
	STA TubeNoTransferIf0

	LDX #2
.loop
	LDA read16sec%, X
	STA sec%, X
	DEX
	BPL loop

	JSR MMC_SetupRead
	JSR MMC_StartRead
	LDA #LO(read16str%)
	STA datptr%
	LDA #HI(read16str%)
	STA datptr%+1
	LDA #8
	STA byteslastsec%
	JSR MMC_ReadBLS
	LDY #256-8
	JSR MMC_Clocks
	LDA #LO(read16str%+8)
	STA datptr%			; assume same page
	\LDA #8
	STA byteslastsec%
	JSR MMC_ReadBLS
	LDY #256-8+2
	JSR MMC_Clocks

	JSR ResetLEDS

	\ copy title to table
	LDY #&0B
.drc_loop4
	LDA read16str%,Y
	STA (gdptr%),Y
	DEY
	BPL drc_loop4

	\ Test if we are at the end of the disk table sector
	LDA gdptr%+1
	CMP #HI(disccataloguebuffer%+&100)
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
dmStr%=tempfilename1		; location of string
dmLen%=tempfilename1+&0D		; length of string
dmAmbig%=tempfilename1+&0E	; string terminated with *

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
	CMP #&20
	BCC pdcdot
	CMP #&7F
	BNE pdcprint
.pdcdot
	LDA #'.'
.pdcprint
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
	ORA #&30
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
	JMP storeDRIVE_INDEX4_ResetCRC7

	\\ *DCAT ((<f.dno>) <t.dno>) (<adsp>)

IF _INCLUDE_CMD_DCAT_
.CMD_DCAT
{
	dcCount%=&A8	; number of disks found
	dcEnd%=&AA	; last disk in range

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
	JSR bcd_inc16_zp_x_A8
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

IF _INCLUDE_CMD_DFREE_
.CMD_DFREE
{
	dfFree%=&A8	; number of unformatted disks
	dfTotal%=&AA	; total number of disks

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
	JSR bcd_inc16_zp_x_A8
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
	STA disccataloguebuffer%+&00, X
	LDA &B9
	STA disccataloguebuffer%+&04, X
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
	CMP #HI(disccataloguebuffer%+&00)
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
IF _BP12K_
	PRINT "    code ends at",~P%," (",(guard_value - P%+extraspace), "bytes free )"
ELSE
	PRINT "    code ends at",~P%," (",(guard_value - P%), "bytes free )"
ENDIF

IF _DEVICE_="G"
SAVE &8000, P%
ELSE
SAVE &8000, &C000
ENDIF
