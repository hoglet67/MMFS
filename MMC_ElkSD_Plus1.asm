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
{
        LDA #&00
        STA spi_active%
        LDA #$FF
        STA spi_port%
.loop
        LDA spi_active%
        CMP #0
        BNE loop
        LDA spi_port%
        RTS
}

.spiwait
{
.loop
        LDA spi_active%
        CMP #0
        BNE loop
        RTS
}

\\ TODO: This code is currently unused

\\ wait for response bit
\\ ie for clear bit
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
.spi_write_byte
{
        STA spi_port%
.loop
        LDA spi_active%
        CMP #0
        BNE loop
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
        JSR MMC_GetByte  ; Writes &FF
        DEY
        BNE loop
        RTS              ; A=SR, X=one%, Y=0
}



.MMC_DoCommand
{
        LDX #0
        LDY #8
.loop1
        LDA cmdseq%,X
        STA spi_port%                   ;\ 2 - write
IF _DEBUG_MMC
        JSR PrintHex
ENDIF
        JSR spiwait
        NOP                             ;\ 2
        NOP                             ;\ 2
        INX                             ;\ 2
        DEY                             ;\ 2
        BNE loop1                       ;\ 2
IF _DEBUG_MMC
        LDA #':'
        JSR OSWRCH
ENDIF
        LDA #&FF
        \ Wait for response, Y=0
.loop2
        STA spi_port%                   ; assume A=&FF
        JSR spiwait                     ;\ 12
        LDA spi_port%
        BPL done
        DEY
        BNE loop2
        CMP #0
.done
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
.loop
        STX spi_port%
        JSR spiwait
        LDA spi_port%
        CMP #&FE
        BNE loop
        RTS
}

\\ *** Read 256 bytes to datptr ***
.MMC_Read256
        LDX #0
        BEQ mmc_read

    \\ *** Read "byteslastsector" bytes
    \\ to datptr ***
.MMC_ReadBLS
        LDX byteslastsec%

.mmc_read
{
        LDY #0
        LDA TubeNoTransferIf0
        BNE tube_loop


        LDA #&01
        STA spi_active%

.loop
        LDA #&FF
        STA spi_port%
        JSR spiwait
        NOP
        NOP
        LDA spi_port%
        STA (datptr%),Y
        INY
        DEX
        BNE loop
        LDA #&00
        STA spi_active%
        RTS

.tube_loop
        TXA
        PHA
        JSR MMC_GetByte
        STA TUBE_R3_DATA
        PLA
        TAX
        INY
        DEX
        BNE tube_loop
        RTS
}

\\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
{
        LDY #&FF
        STY CurrentCat
        INY
.loop
        JSR MMC_GetByte
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
        JMP spi_write_byte
}


.MMC_EndWrite
{
        JSR MMC_16Clocks
.loop1
        JSR MMC_GetByte
        TAY
        AND #&1F
        CMP #&1F
        BEQ loop1
        CMP #5
        BNE errWrite2

.loop2
        JSR MMC_GetByte
        CMP #&FF
        BNE loop2
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
        JSR spi_write_byte
        INY
        BNE loop1
        RTS
.tube
        LDY #0
.loop2
        LDA TUBE_R3_DATA
        JSR spi_write_byte
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
        JSR spi_write_byte
        INY
        BNE loop
        RTS
}
