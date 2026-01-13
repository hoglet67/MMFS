\\ Driver for the SPI port on ElkSD-Plus1 Rev2 and ElkVGA P2 hardware,
\\
\\ &FC80 is data port for reading/writing
\\ &FC81 is status and control port, read LSB for SPI controller state (0 = idle, 1 = busy) 
\\ and write for clock speed control (0 = slow clock, 1 = fast clock)
\\
\\ On ElkSD-Plus1 Rev 2 hardware slow clock sets SPI CLK to PHI0/8, fast clock is 2MHz


spi_port%=&FC80
spi_active%=&FC81



\\ This is unused on the Electron, according to the EAUG
sr%=&F8


\\ Read byte (User Port)
\\ Write FF
.MMC_GetByte
.P1_ReadByte

	LDA #&00
	STA spi_active%
	LDA #$FF
	STA spi_port%
.spi_readwaitidle
	LDA spi_active%
	CMP #0
	BNE spi_readwaitidle
	LDA spi_port%
    RTS
	
	
.spiwait
.spi_waitidle
	LDA spi_active%
	CMP #0
	BNE spi_waitidle
	RTS
	



\\ wait for response bit
\\ ie for clear bit
.P1_WaitResp
{
.loop
	DEY
	BEQ timeout
	LDA #&FF
	STA spi_port%
	JSR spiwait
	LDA spi_port%
	STA sr%
	ROL sr%
	BCS loop
.timeout
	LDA spi_port%
	RTS
	
	
}

\\ Write byte (User Port)
\\ Ignore byte in
.P1_WriteByte
{
	STA spi_port%
.spi_waitwriteidle
	LDA spi_active%
	CMP #0
	BNE spi_waitwriteidle
    RTS
}

\\ More generic code below tis point


\\ RESET DEVICE
.MMC_DEVICE_RESET
    RTS

.MMC_SlowClocks
    JMP MMC_Clocks

\\ *** Send &FF to MMC Y times ***
\\ Y=0=256
.MMC_16Clocks
    LDY #2
.MMC_Clocks
{
.loop
    JSR P1_ReadByte  ; Writes &FF
    DEY
    BNE loop
    RTS              ; A=SR, X=one%, Y=0
}



.MMC_DoCommand
{
	LDX #0
	LDY #8
.xdcmd1dcmd1
	LDA cmdseq%,X
	STA spi_port%			;\ 2 - write
IF _DEBUG_MMC
    JSR PrintHex
ENDIF	
	JSR spiwait
	NOP				;\ 2
	NOP				;\ 2
	INX				;\ 2
	DEY				;\ 2
	BNE xdcmd1dcmd1			;\ 2
IF _DEBUG_MMC
    LDA #':'
    JSR OSWRCH
ENDIF	
	LDA #&FF	

	\ Wait for response, Y=0
.xwR1mm

	STA spi_port%			; assume A=&FF
	JSR spiwait			;\ 12
	LDA spi_port%
	BPL xdcmdex
	DEY
	BNE xwR1mm
	CMP #0
.xdcmdex
IF _DEBUG_MMC
	PHA
    JSR PrintHex
    JSR OSNEWL 
	PLA
ENDIF	
	RTS
}



\\ *** Wait for data token ***
.MMC_WaitForData
{
	LDX #&FF
.tloop
	STX spi_port%
    JSR spiwait
	LDA spi_port%
    CMP #&FE
    BNE tloop
    RTS
}

\\ *** Read 256 bytes to datptr ***
.MMC_Read256
    LDX #0
    BEQ MMC_ReadX

    \\ *** Read "byteslastsector" bytes
    \\ to datptr ***
.MMC_ReadBLS
    LDX byteslastsec%

.MMC_ReadX
    LDY #0
    LDA TubeNoTransferIf0
    BNE MMC_ReadToTube

.MMC_ReadToMemory
	LDA #&01
	STA spi_active%
	
.MMC_ReadToMemoryLoop
	LDA #&FF
	STA spi_port%
	JSR spiwait
	NOP
	NOP
	LDA spi_port%	
    STA (datptr%),Y
    INY
    DEX
    BNE MMC_ReadToMemoryLoop
	LDA #&00
	STA spi_active%
    RTS

.MMC_ReadToTube
    TXA
    PHA
    JSR P1_ReadByte
    STA TUBE_R3_DATA
    PLA
    TAX
    INY
    DEX
    BNE MMC_ReadToTube
    RTS


\\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
{
    LDY #&FF
    STY CurrentCat
    INY
.loop
    JSR P1_ReadByte
    STA buf%, Y
    INY
    BNE loop
    RTS
}

\\ **** Send Data Token to card ****
.MMC_SendingData
{
    LDY #2
    JSR MMC_Clocks
    LDA #&FE
    JMP P1_WriteByte
}


.MMC_EndWrite
{
    JSR MMC_16Clocks
.ewu1
    JSR P1_ReadByte
    TAY
    AND #&1F
    CMP #&1F
    BEQ ewu1
    CMP #5
    BNE errWrite2

.ewu2
    JSR P1_ReadByte
    CMP #&FF
    BNE ewu2
    RTS
}



\\ **** Write 256 bytes from dataptr% ****
.MMC_Write256
{
	LDA #&01
	STA spi_active%
    LDY TubeNoTransferIf0
    BNE tube
.loop1
    LDA (datptr%),Y
    JSR P1_WriteByte
    INY
    BNE loop1
    RTS
.tube
    LDY #0
.loop2
    LDA TUBE_R3_DATA
    JSR P1_WriteByte
    INY
    BNE loop2
	LDA #&00
	STA spi_active%
    RTS
}

\\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
    LDY #0
.loop
    LDA buf%,Y
    JSR P1_WriteByte
    INY
    BNE loop
    RTS
}
