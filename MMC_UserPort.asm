\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

\\ mmbeeb USER PORT MMC DEVICE

\ User VIA registers
iorb%=_VIA_BASE
ddrb%=_VIA_BASE + &02
sr%  =_VIA_BASE + &0A
acr% =_VIA_BASE + &0B
ifr% =_VIA_BASE + &0D
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
	JSR ShiftRegMode0
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

	\\ The read code below now operates in turbo mode on all hardware
	\\ using shift register mode 2.

	\\ *** Read 256 bytes to datptr ***
.MMC_Read256
	LDX #0
	BEQ MMC_ReadX

	\\ *** Read "byteslastsector" bytes
	\\ to datptr ***
.MMC_ReadBLS
	LDX byteslastsec%

.MMC_ReadX
	JSR ShiftRegMode2
	LDA TubeNoTransferIf0
	BNE MMC_ReadToTube

.MMC_ReadToMemory
	JSR WaitForShiftDone
	STA (datptr%),Y
	INY
	DEX
	BNE MMC_ReadToMemory
	RTS

.MMC_ReadToTube
	JSR WaitForShiftDone
	STA TUBE_R3_DATA
	DEX
	BNE MMC_ReadToTube
	RTS


	\\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
	LDX #&FF
	STX CurrentCat
	INX

	JSR ShiftRegMode2

.rdbuf2
	JSR WaitForShiftDone
	STA buf%, Y
	INY
	DEX
	BNE rdbuf2
	RTS

.WaitForShiftDone
{
	LDA #4
.loop
	BIT ifr%	 \\ wait for the SR interrupt flag to be set
	BEQ loop
	CPX #1	   \\ if the last byte, then return to mode zero, so we don't shift an extra byte
	BNE notLastByte
	JSR ShiftRegMode0
.notLastByte		
	LDA sr%	  \\ read the data byte, and clear the SR interrupt flag
	RTS
}		
		
.ShiftRegMode0
	LDA acr%   \\ Set SR Mode to mode 0
	AND #&E3   \\ 11100011 = SR Mode 0  
	STA acr%   \\ CB1 is now an input
	LDA ddrb%  \\ Set PB1 to being an output
	ORA #&02   \\ 00000010
	STA ddrb%
	RTS

.ShiftRegMode2
	LDA ddrb%  \\ Set PB1 to being an input
	AND #&FD   \\ 11111101 
	STA ddrb%
	LDA acr%
	AND #&E3   \\ 11100011
	ORA #&08   \\ 00001000 = SR Mode 2
	STA acr%
	LDA sr%	   \\ Start the first read
	LDY #0	 \\ Set the memory index to zero
	RTS
		
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
