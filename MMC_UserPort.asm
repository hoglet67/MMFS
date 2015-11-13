\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

\\ mmbeeb USER PORT MMC DEVICE

\ User VIA registers
iorb%=&FE60
ddrb%=&FE62
sr%=&FE6A
acr%=&FE6B
ier%=&FE6E

	\\ Reset the User VIA
.MMC_DEVICE_RESET
	LDA #3
	STA ddrb%
	LDA #0
	STA iorb%
	LDA acr%
	AND #&E3
	STA acr%
	LDA #&1C
	STA ier%
	RTS


	\\ Read byte (User Port)
	\\ Write FF
.MMC_GetByte
	LDX #1
.UP_ReadByteX
	LDA #3
	STX iorb%			;\0
	STA iorb%
.UP_ReadBits7
	STX iorb%			;\1
	STA iorb%
	STX iorb%			;\2
	STA iorb%
	STX iorb%			;\3
	STA iorb%
.UP_ReadBits4
	STX iorb%			;\4
	STA iorb%
	STX iorb%			;\5
	STA iorb%
	STX iorb%			;\6
	STA iorb%
	STX iorb%			;\7
	STA iorb%
	LDA sr%
	RTS

	\\ Write byte (User Port)
	\\ Ignore byte in
.UP_WriteByte
	ASL A
	ROL A				;\0
	STA iorb%
	ORA #2
	STA iorb%
	ROL A				;\1
	AND #&FD
	STA iorb%
	ORA #2
	STA iorb%
	ROL A				;\2
	AND #&FD
	STA iorb%
	ORA #2
	STA iorb%
	ROL A				;\3
	AND #&FD
	STA iorb%
	ORA #2
	STA iorb%
	ROL A				;\4
	AND #&FD
	STA iorb%
	ORA #2
	STA iorb%
	ROL A				;\5
	AND #&FD
	STA iorb%
	ORA #2
	STA iorb%
	ROL A				;\6
	AND #&FD
	STA iorb%
	ORA #2
	STA iorb%
	ROL A				;\7
	AND #&FD
	STA iorb%
	ORA #2
	STA iorb%
	RTS

	\\ *** Send &FF to MMC Y times ***
	\\ Y=0=256
.MMC_16Clocks
	LDY #2
.MMC_Clocks

{
	LDX #1
.clku1
	JSR UP_ReadByteX		; Writes &FF
	DEY
	BNE clku1
	RTS				; A=SR, X=1, Y=0
}


	\\ *** Send command to MMC ***
	\\ On exit A=result, Z=result=0
.MMC_DoCommand
	LDX #0

{
	LDY #7
.dcmdu1
	LDA cmdseq%,X
	JSR UP_WriteByte
	INX
	DEY
	BNE dcmdu1
	JSR waitresp_up
	JMP UP_ReadBits7
}

	\\ wait for response bit
	\\ ie for clear bit (User Port only)
.waitresp_up
{
	LDA #1
	LDX #3
	LDY #0
.wrup
	DEY
	BEQ wrup_timeout
	STA iorb%
	STX iorb%
	LDA sr%
	AND #1
	BNE wrup
	LDX #1
	LDA #3
.wrup_timeout
	RTS
}


	\\ *** Wait for data token ***
.MMC_WaitForData
{

	LDX #1
.wlu1
	JSR UP_ReadByteX
	CMP #&FE
	BNE wlu1
	RTS
}

	\\ *** Read 256 bytes to datptr ***
.MMC_Read256
{

	LDX #1
	LDY TubeNoTransferIf0
	BNE rdub2T20

.rdlu2
	JSR UP_ReadByteX
	STA (datptr%),Y
	INY
	BNE rdlu2
	RTS

.rdub2T20
	LDY #0
	JMP rdub2T2
}

	\\ *** Read "byteslastsector" bytes
	\\ to datptr ***
.MMC_ReadBLS
{
	LDX #1
	LDY TubeNoTransferIf0
	BNE rdub2T1
.rdub2
	JSR UP_ReadByteX
	STA (datptr%),Y
	INY
	DEC byteslastsec%
	BNE rdub2
	RTS
}

.rdub2T1
	LDY byteslastsec%
.rdub2T2
	JSR UP_ReadByteX
	STA TUBE_R3_DATA
	DEY
	BNE rdub2T2
	RTS


	\\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
{
	LDA #&FF
	STA CurrentCat

	LDY #0
	LDX #1
.rdbuf2
	JSR UP_ReadByteX
	STA buf%,Y
	INY
	BNE rdbuf2
	RTS
}

	\\ **** Send Data Token to card ****
.MMC_SendingData
{
	LDY #2
	JSR MMC_Clocks
	LDA #&FE
	JMP UP_WriteByte
}

	\\ **** Complete Write Operation *****
.MMC_EndWrite
{
	LDY #2

	JSR MMC_Clocks
	JSR waitresp_up
	JSR UP_ReadBits4
	TAY
	AND #&1F
	CMP #5
	BNE errWrite2

	LDX #1
.ewu2
	JSR UP_ReadByteX
	CMP #&FF
	BNE ewu2
	RTS
}

	\\ **** Write 256 bytes from dataptr% ****
.MMC_Write256
{
	LDY TubeNoTransferIf0
	BNE wruT1

.wru1
	LDA (datptr%),Y
	JSR UP_WriteByte
	INY
	BNE wru1
	RTS

.wruT1
	LDY #0
.wruT2
	LDA TUBE_R3_DATA
	JSR UP_WriteByte
	INY
	BNE wruT2
	RTS
}

	\\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
	LDY #0
.wbu1
	LDA buf%,Y
	JSR UP_WriteByte
	INY
	BNE wbu1
	RTS
}