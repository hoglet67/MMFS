\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

	\ \\**    *ROMS (<rom>)    **//
.CMD_ROMS
{
	LDA #&00
	STA &A8
	JSR Sub_AAEA_StackAZero		; Change value of A in stack to 0?
	LDA #&0F
	STA &AA
	JSR Sub_AADD_RomTablePtrBA
	SEC
	JSR GSINIT
	STY &AB
	CLC
	BEQ Label_A9FF_notnum		; If null str (no parameter)
.Label_A9E7_loop
	JSR Param_ReadNum
	BCS Label_A9FF_notnum		; If not valid number
IF NOT(_MM32_)
	CMP #0				; Ignore if number >= 16
	BNE Label_A9E7_loop
ENDIF
	CPX #16
	BCS Label_A9E7_loop
	TXA
	STY &AB				; Save Y
	STA &AA				; Rom Nr
	JSR Label_AA53_RomInfo
	LDY &AB
	JSR GSINIT_A
	STY &AB				; Restore Y
	BNE Label_A9E7_loop		; Another rom id?
	RTS

.Label_A9FF_notnum
	ROR &A8				; Loop through roms
.Label_AA01_loop
	BIT &A8
	BPL Label_AA0A
	JSR Sub_AA12_titlecmp		; Match title with parameter
	BCC Label_AA0D_nomatch
.Label_AA0A
	JSR Label_AA53_RomInfo
.Label_AA0D_nomatch
	DEC &AA
	BPL Label_AA01_loop
	RTS

.Sub_AA12_titlecmp
	LDA #&09			; wF6=&8009 = title
	STA &F6
	LDA #&80
	STA &F7
	LDY &AB
.Label_AA1C_loop
	LDA (TextPointer),Y
	CMP #&0D			; If end of str
	BEQ Label_AA44
	CMP #&22
	BEQ Label_AA44			; If ="."
	INY
	CMP #&2A
	BEQ Label_AA51_match		; If ="*"
	JSR UcaseA2
	STA &AE
	JSR Sub_AACF_ReadRom
	BEQ Label_AA42_nomatch
	LDX &AE
	CPX #&23			; "#"
	BEQ Label_AA1C_loop
	JSR UcaseA2
	CMP &AE
	BEQ Label_AA1C_loop
.Label_AA42_nomatch
	CLC
	RTS

.Label_AA44
	JSR Sub_AACF_ReadRom
	BEQ Label_AA51_match
	CMP #&20
	BEQ Label_AA44			; If =" "   skip spaces
	CMP #&0D
	BNE Label_AA42_nomatch		; If <>CR
.Label_AA51_match
	SEC
	RTS

.Label_AA53_RomInfo
	LDY &AA				; Y=Rom nr
	LDA (&B4),Y
	BEQ Label_AA42_nomatch		; If RomTable(Y)=0
	PHA
	JSR PrintString
	EQUS "Rom "
	TYA
	JSR PrintBCD			; Print ROM nr
	JSR PrintString
	EQUS " : "
	LDA #&28			; A="("
	JSR PrintChrA
	PLA
	PHA
	BMI Label_AA78			; Bit 7 set = Service Entry
	LDY #&20			; Y=" "
	BNE Label_AA7A			; always
.Label_AA78
	LDY #&53			; Y="S"
.Label_AA7A
	TYA
	JSR PrintChrA
	PLA
	LDY #&20			; Y=" "
	ASL A
	BPL Label_AA86			; Bit 6 set = Language Entry
	LDY #&4C			; Y="L"
.Label_AA86
	TYA
	JSR PrintChrA
	LDA #&29			; A=")"
	JSR PrintChrA
	JSR PrintSpaceSPL
	JSR Label_AA9A_PrtRomTitle
	JSR PrintNewLine
	SEC
	RTS

.Label_AA9A_PrtRomTitle
	LDA #&07			; Print ROM title
	STA &F6
	LDA #&80
	STA &F7				; wF6=&8007
	JSR Sub_AACF_ReadRom
	STA &AE				; Copyright offset
	INC &F6				; wF6=&8009
	LDY #&1E
	JSR Sub_AAC2_PrintRomStr
	BCS Label_AAB7_rts		; If reached copyright offset
	JSR PrintSpaceSPL
	DEY
	JSR Sub_AAC2_PrintRomStr
.Label_AAB7_rts
	RTS

.Label_AAB8_loop
	CMP #&20
	BCS Label_AABE			; If >=" "
	LDA #&20
.Label_AABE
	JSR PrintChrA
	DEY
.Sub_AAC2_PrintRomStr
	LDA &F6
	CMP &AE
	BCS Label_AACE_rts		; If >=
	JSR Sub_AACF_ReadRom
	BNE Label_AAB8_loop
	CLC 				; C=0=Terminator
.Label_AACE_rts
	RTS

.Sub_AACF_ReadRom
	TYA 				; Read byte from ROM
	PHA
	LDY &AA
	JSR OSRDRM			; Address in wF6
	INC &F6
	TAX
	PLA
	TAY
	TXA
	RTS

.Sub_AADD_RomTablePtrBA
	JSR RememberXYonly
	LDA #&AA			; ROM information table @ XY
	JSR osbyte_X0YFF
	STX &B4
	STY &B5
	RTS

.Sub_AAEA_StackAZero
	TSX 				; Change value of A to 0?
	LDA #&00
	STA &0107,X
	RTS
}	\ End of *ROMS code
