\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

\\ MEMORY MAPPED MMC DEVICE


IF _MASTER_
mmc%=&FEDC
ELSE
mmc%=&FE18
ENDIF

	\\ RESET DEVICE
.MMC_DEVICE_RESET
	RTS


	\\ Read byte (User Port)
	\\ Write FF
.MMC_GetByte
	LDA #&FF
	STA mmc%
	JSR donothing
	LDA mmc%
	RTS


	\\ *** Send &FF to MMC Y times ***
	\\ Y=0=256
.MMC_16Clocks
	LDY #2
.MMC_Clocks
.MMC_SlowClocks
{
	LDA #&FF
.clk1
	STA mmc%
	JSR donothing
	DEY
	BNE clk1
}
.donothing
	\\ JSR+RTS = 12 cycles
	RTS				; A=&FF, Y=0


	\\ *** Send command to MMC ***
	\\ On exit A=result, Z=result=0
.MMC_DoCommand
{
	LDX #0
	LDY #8
.dcmd1
	LDA cmdseq%,X
	STA mmc%			;\ 2 - write
	NOP				;\ 2
	NOP				;\ 2
	INX				;\ 2
	DEY				;\ 2
	BNE dcmd1			;\ 2
	STA mmc%			; assume A=&FF
	\ Wait for response, Y=0
.wR1mm
	JSR donothing			;\ 12
	LDA mmc%
	BPL dcmdex
	DEY
	BNE wR1mm
	CMP #0
.dcmdex
IF _DEBUG_MMC
	PHP
	PHA
	LDY #0
.dcmdu2
	LDA cmdseq%,Y
	JSR PrintHex
	INY
	CPY #7
	BNE dcmdu2
	LDA #':'
	JSR OSWRCH
	PLA
	PHA
	JSR PrintHex
	JSR OSNEWL
	PLA
	PLP
ENDIF
	RTS
}					; A=result, X=X+8, Y=?


	\\ *** Wait for data token ***
.MMC_WaitForData
{
	LDX #&FF
.wl1
	STX mmc%
	JSR donothing			;\ 12
	LDA mmc%
	CMP #&FE			;\ data token
	BNE wl1
	RTS				; A=&FE, X=&FF, Y unchanged
}


	\\ *** Read 256 bytes to datptr ***
.MMC_Read256
{
	LDX #&FF
	STX mmc%
	LDY TubeNoTransferIf0		;\ 4
	BNE rdlT20			;\ 2

	NOP				;\ 2
	NOP				;\ 2
	NOP				;\ 2
.rdl1
	LDA mmc%			;\ 2 - read
	STX mmc%			;\ 2 - write
	STA (datptr%),Y			;\ 6
	INY				;\ 2
	CPY #&FF			;\ 2
	BNE rdl1			;\ 2
	LDA mmc%			;\ 2 - read
	STA (datptr%),Y
	RTS

.rdlT20					;\ 7
	LDY #0				;\ 9
	JMP rdlT2			;\ 12
}


	\\ *** Read "byteslastsector" bytes
	\\ to datptr ***
.MMC_ReadBLS
{
	LDX #&FF
	STX mmc%			;\ write
	LDY TubeNoTransferIf0		;\ 4
	BNE rdlT1			;\ 2
	DEC byteslastsec%		;\ 6
	BEQ rdl3			;\ 2
.rdl2
	LDA mmc%			;\ 2 - read
	STX mmc%			;\ 2 - write
	STA (datptr%),Y			;\ 6
	INY				;\ 2
	DEC byteslastsec%		;\ 6
	BNE rdl2			;\ 2
.rdl3
	LDA mmc%			;\ 2 - read
	STA (datptr%),Y
	RTS
}

	\\ TUBE
	\\ 24us delay=48 cycles
	\ \ (7)
.rdlT1
	LDY byteslastsec%		;\ (9)
.rdlT2
{
	DEY				;(11)
	BEQ rdlT4			;(14)
.rdlT3
	NOP				;\ 2=37
	LDA mmc%			;\ 4=41
	STX mmc%			;\ 4=45
	STA TUBE_R3_DATA		;\ 4=49
	JSR donothing			;\ 12
	JSR donothing			;\ 12=24
	NOP				;\ 2=26
	NOP				;\ 2=28
	NOP				;\ 2=30
	DEY				;\ 2=32
	BNE rdlT3			;\ 3=35

	NOP				;\ 2=36
	NOP				;\ 2=38
	NOP				;\ 2=40
	NOP				;\ 2=42

.rdlT4
	LDA mmc%			;\ (17) 4=46
	STA TUBE_R3_DATA		;\ 4=50
	RTS
}


	\\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
{
	LDA #&FF
	STA CurrentCat

	LDY #0
	LDX #&FF
	STX mmc%
	JSR donothing			;\ 12
.rdl4
	LDA mmc%			;\ 2 - read
	STX mmc%			;\ 2 - write
	STA buf%,Y			;\ \ 5
	INY				;\ 2
	CPY #&FF			;\ 2
	BNE rdl4			;\ 2
	LDA mmc%			;\ 2 - read
	STA buf%,Y
	RTS
}

	\\ **** Send Data Token to card ****
.MMC_SendingData
{
	LDX #&FF
	STX mmc%
	JSR donothing
	STX mmc%
	JSR donothing
	DEX
	STX mmc%
	RTS
}

	\\ **** Complete Write Operation *****
.MMC_EndWrite
{
	JSR MMC_16Clocks
	LDX #&FF
	STX mmc%
	JSR donothing			;\ 12
	LDA mmc%
	TAY
	AND #&1F
	CMP #5
	BNE errWrite2

	LDA #&FF
.ew1
	STX mmc%
	JSR donothing
	CMP mmc%
	BNE ew1
	RTS
}

	\\ **** Write 256 bytes from dataptr% ****
.MMC_Write256
{
	LDY TubeNoTransferIf0
	BNE wrT1
.wr1
	LDA (datptr%),Y			;\ 6
	STA mmc%			;\ 2 - write
	INY				;\ 2
	BNE wr1				;\ 3
	RTS

	\.wrT1	; To tube 24us delay
.wrT1
	LDY #0
.wrT2
	LDA TUBE_R3_DATA		;\ 4
	STA mmc%			;\ 4=8
	JSR donothing			;\ 12=20
	JSR donothing			;\ 12=32
	JSR donothing			;\ 12=44
	INY				;\ 2=46
	BNE wrT2			;\ 3=49
	RTS
}

	\\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
	LDY #0
.wbm1
	NOP				;\ 2
	LDA buf%,Y			;\ 4
	STA mmc%			;\ 2 - write
	INY				;\ 2
	BNE wbm1			;\ 3
	RTS
}
