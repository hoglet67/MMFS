\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

	\ ****** START OF UTILITIES *****

.CMD_DUMP
{
	dumpbuffer = &A8 ; 8 byte buffer ( local use only)

	LDA #&40; Open file for input
	JSR Utils_FilenameAtXY

	LDA #0
	PHA ; low byte
	TAX ; high byte
	BEQ dump_loop

.dump_inc_address
	; C already 0
	ADC #8
	PHA ; low byte
	BCC dump_loop
	INX
.dump_loop

	TXA ; high byte
	JSR PrintHexSPL

	PLA	 ; low byte
	PHA
	JSR PrintHexSPL

	TXA
	PHA ;high byte

	JSR PrintSpaceSPL		; exits with C=0
	LDX #7
.dump_getbytes_loop
	JSR OSBGET
	BCS dump_eof			; If eof
	STA dumpbuffer,X				; save byte
	JSR PrintHexSPL
	JSR PrintSpaceSPL		; exits with C=0
	DEX
	BPL dump_getbytes_loop
.dump_eof
	PHP
	BCC dump_noteof			; If not eof
.dump_padnum_loop
	LDA #&2A				; Pad end of line with "** "
	JSR OSWRCH
	JSR OSWRCH
	JSR PrintSpaceSPL		; exits with C=0
	LDA #&00
	STA dumpbuffer,X
	DEX
	BPL dump_padnum_loop
.dump_noteof
	LDX #7
.dump_chr_loop
	LDA dumpbuffer,X

	; Chr or "."

	AND #&7F			; If A<&20 OR >=&7F return "."
	CMP #&7F			; Ignores bit 7
	BEQ showchrdot
	CMP #&20
	BCS showchrexit
.showchrdot
	LDA #&2E			; "."
.showchrexit

	JSR OSWRCH
	DEX
	BPL dump_chr_loop

	JSR OSNEWL
	PLP
	PLA ; Restore high byte
	TAX
	PLA ; low byte byte
	BIT &FF				; Check escape
	BMI Utils_ESCAPE_CloseFileY

	BCC dump_inc_address

	BCS Utils_CloseFile_Yhandle
}

\ *TYPE and *LIST are very similar
\
	linenumberA8 = &A8 ; &A8 &A9
	printlinenumberAA = &AA
.CMD_TYPE
	LDA #&00
	BEQ type
.CMD_LIST
	LDA #&FF
.type
{
	STA printlinenumberAA
	LDA #&40 ; Open file for input
	JSR Utils_FilenameAtXY

	LDA #&0D
.list_loop_entry
	AND printlinenumberAA
	EOR #&0D			; Carriage return?
	TAX 				; (Always false if CMD_TYPE)
.list_loop
	JSR OSBGET
	BCS list_eof			; EOF exit loop
	CMP #&0A
	BEQ list_loop			; ignore &0A
	PHA
	TXA
	BNE list_skiplineno		; If don't print line number
	JSR Utils_PrintLineNoA8
.list_skiplineno
	PLA
	JSR OSASCI
	BIT &FF
	BMI Utils_ESCAPE_CloseFileY			; Escape?
	BPL list_loop_entry		;Always

.list_eof
	JSR OSNEWL      ;Print newline + exit
}
.Utils_CloseFile_Yhandle
	LDA #&00
	JMP OSFIND

.Utils_ESCAPE_CloseFileY
	JSR Utils_CloseFile_Yhandle	; file Y and report error!
	JMP ReportESCAPE


.CMD_BUILD
{
IF _NON_WS_BUILD_COM

	; linenumberA8 = &A8 NB this is a globally defined
	flength = &AA

	LDA #&80			; Open file for OUTPUT only
	JSR Utils_FilenameAtXY		; XY points to filename
	; Y is file handle

.loop_newline
	JSR Utils_PrintLineNoA8
	LDX #0
	BEQ loop_line ; always

.subpointer
	JSR fpdec
.loop_line
	JSR OSRDCH
	BCS Utils_ESCAPE_CloseFileY
	CMP #21 : BEQ clearline
	CMP #127 : BEQ subpointer
	JSR OSASCI
	JSR OSBPUT
	INX
	CMP #13
	BNE loop_line
	BEQ loop_newline

.clearline
	JSR fpdec
	BNE clearline
	BEQ loop_line

.fpdec
	TXA
	BEQ fpdecrts
	PHA
	LDA #127
	JSR OSWRCH
	LDA #0	; read file ptr length
	LDX #flength+0
	JSR OSARGS
	LDA flength+0
	BNE declsb
	LDA flength+1
	BNE decmsb
	DEC flength+2
.decmsb
	DEC flength+1
.declsb
	DEC flength+0

.fpdecdone
	LDA #3	; set file ext length
	JSR OSARGS
	PLA
	TAX
	DEX
.fpdecrts
	RTS

ELSE
	; line number @ A8 A9
	filehandleAB = &AA
	bufferAC = &AB 		; &AC &AD 16bit pointer
	maxlinelengthAE = &AD
	minasciivalueAF = &AE
	maxasciivalueB0 = &AF
	tempAF = &AF ; This overlaps the last value of the OSWORD buffer
				 ; which is rewritten before next OSWORD use
	LDA #&80			; Open file for OUTPUT only
	JSR Utils_FilenameAtXY		; XY points to filename
	STA filehandleAB	;File handle

IF _SWRAM_
	LDA #UTILSBUF
	STA bufferAC+1
ELSE
	LDX PagedRomSelector_RAMCopy
	LDA PagedROM_PrivWorkspaces,X	; Word AC -> 2nd PWSP Page
	AND #&3F			; Bits 7 & 6 are flags
	STA bufferAC+1
	INC bufferAC+1

ENDIF

	LDA #&20
	STA minasciivalueAF	; min ASCII value accepted

.build_loop1
	JSR Utils_PrintLineNoA8		; Line number prompt:
								; Build Osword control block @ AC

	LDY #&FF
	STY maxlinelengthAE	; Max length = 256
	STY maxasciivalueB0
	INY
	STY bufferAC		; So word AC=&1800 (normally)

	TYA 				; OSwORD 0
	LDX #bufferAC		; Osword ptr YX=&00AC
	JSR OSWORD			; OSWORD 0, YX=&00AC
	PHP 				; Read line from input
	STY tempAF			; Y=line length
	LDY filehandleAB    ; Y=file handle
	LDX #&00
	BEQ build_loop2entry		; always
.build_loop2
	LDA (bufferAC,X)			; Output line to file
	JSR OSBPUT
	INC bufferAC
.build_loop2entry
	LDA bufferAC
	CMP tempAF
	BNE build_loop2
	PLP
	BCS Utils_ESCAPE_CloseFileY	; Escape pressed so exit
	LDA #&0D			; Carriage return
	JSR OSBPUT
	JMP build_loop1
ENDIF
}

.Utils_FilenameAtXY
{
	tempAF = &AF
	STA tempAF
	TSX 				; Return A=0 to OS
	LDA #&00
	STA &0107,X
	STA linenumberA8	; reset line number
	STA linenumberA8+1  ; NB not always needed
	DEY
.utils_skipspcloop
	INY 				; Skip spaces
	LDA (TextPointer),Y
	CMP #&20
	BEQ utils_skipspcloop

	CMP #&0D
	BNE utils_notnullstr		; If not end of line
	JMP errSYNTAX			; Syntax Error!

.utils_notnullstr

	TYA				; YX=TextPtr+Y
	CLC				; Used to pass to OSFIND
	ADC TextPointer			; ie. Filename
	TAX
	LDA TextPointer+1
	ADC #&00
	TAY

	LDA tempAF
	JSR OSFIND
	BEQ utils_filenotfound
	TAY 				; Y=File handle
	RTS

.utils_filenotfound
	JMP err_FILENOTFOUND
}

.Utils_PrintLineNoA8
	JSR bcd_inc16_zp_x_A8		; A = hi byte
	JSR PrintHexSPL
	LDA linenumberA8			; A = lo byte
	JSR PrintHexSPL
	JMP PrintSpaceSPL		; exits with C=0

	\ ********** END OF UTILITIES **********
