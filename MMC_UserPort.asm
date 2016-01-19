\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

\\ mmbeeb USER PORT MMC DEVICE

\ User VIA registers
iorb%=_VIA_BASE
ddrb%=_VIA_BASE + &02
sr%  =_VIA_BASE + &0A
acr% =_VIA_BASE + &0B
ier% =_VIA_BASE + &0E

IF _TURBOMMC
   temp    = &cf
   ddrmask = &1F \\ 0001 1111
   msbits  = &08 \\ 0000 1000
   msmask  = &E9 \\ 1110 1001
ELSE
   ddrmask = &03 \\ 0000 0011
   msbits  = &00 \\ 0000 0000
   msmask  = &FD \\ 1111 1101
ENDIF

	\\ Reset the User VIA
.MMC_DEVICE_RESET
	LDA #(0 + msbits)
	STA iorb%
	LDA #ddrmask
	STA ddrb%
	LDA acr%
	AND #&E3
	STA acr%
	LDA #&1C
	STA ier%
	RTS


	\\ Read byte (User Port)
	\\ Write FF
.MMC_GetByte
	LDX #(1 + msbits)
.UP_ReadByteX
	LDA #(3 + msbits)
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
IF _TURBOMMC
    STA temp
ENDIF

FOR N, 0, 7
IF _TURBOMMC
    ROL temp
    LDA temp
    ORA #msbits
ELSE
    ROL A
ENDIF
	AND #msmask
	STA iorb%
	ORA #(2 + msbits)
	STA iorb%
NEXT        
	RTS

	\\ *** Send &FF to MMC Y times ***
	\\ Y=0=256
.MMC_16Clocks
	LDY #2
.MMC_Clocks

{
	LDX #(1 + msbits)
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
	LDY #0
.wrup
	DEY
	BEQ wrup_timeout
	LDX #(1 + msbits)
	LDA #(3 + msbits)
	STX iorb%
	STA iorb%
	LDA sr%
	AND #1
	BNE wrup
	LDX #(1 + msbits)
	LDA #(3 + msbits)
.wrup_timeout
	RTS
}


	\\ *** Wait for data token ***
.MMC_WaitForData
{

	LDX #(1 + msbits)
.wlu1
	JSR UP_ReadByteX
	CMP #&FE
	BNE wlu1
	RTS
}

	\\ *** Read 256 bytes to datptr ***
.MMC_Read256
{

	LDX #(1 + msbits)
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
	LDX #(1 + msbits)
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
	LDX #(1 + msbits)
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

	LDX #(1 + msbits)
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
