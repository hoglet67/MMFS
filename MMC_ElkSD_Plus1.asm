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

IF _ELECTRON_

MACRO DO_JSR_MAP
ENDMACRO

MACRO DO_JSR_UNMAP
ENDMACRO

MACRO DO_INLINE_MAP
ENDMACRO

MACRO DO_INLINE_UNMAP
ENDMACRO

ELSE

acccon% = &FE34

MACRO DO_JSR_MAP
        JSR map_internal_io
ENDMACRO

MACRO DO_JSR_UNMAP
        JSR unmap_internal_io
ENDMACRO

MACRO DO_INLINE_MAP
        PHA
        LDA acccon%
        STA &90            ;; TODO: Use of &90 is dodgy
        ORA #&20
        STA acccon%
        PLA
ENDMACRO

MACRO DO_INLINE_UNMAP
        LDA &90            ;; TODO: Use of &90 is dodgy
        STA acccon%
ENDMACRO

.map_internal_io
{
        DO_INLINE_MAP
        RTS
}

.unmap_internal_io
{
        PHA
        DO_INLINE_UNMAP
        PLA
        RTS
}

ENDIF

\\ Read byte (User Port)
\\ Write FF
.MMC_GetByte
{
        DO_JSR_MAP
        LDA #&00
        STA spi_active%
        LDA #$FF
        STA spi_port%
.loop
        LDA spi_active%
        CMP #0
        BNE loop
        LDA spi_port%
        DO_JSR_UNMAP
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
        DO_JSR_MAP
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
        DO_JSR_UNMAP
        RTS
}

\\ Write byte in A to SPI data port
IF NOT(_ELECTRON_)
.spi_write_byte_remap
{
        DO_INLINE_MAP
        STA spi_port%
.loop
        LDA spi_active%
        CMP #&00
        BNE loop
        DO_INLINE_UNMAP
        LDA #&00
        RTS
}
ENDIF

\\ Write byte (User Port)
\\ Ignore byte in
.spi_write_byte
{
        STA spi_port%
.loop
        LDA spi_active%
        CMP #0
        BNE loop
IF NOT(_ELECTRON_)
        LDA #&00     ; This is unnecessary
ENDIF
        RTS
}

\\ More generic code below tis point


\\ RESET DEVICE
.MMC_DEVICE_RESET
{
        RTS          ; This could use an existing RTS
}

IF _ELECTRON_
.MMC_SlowClocks
        JMP MMC_Clocks
ENDIF

\\ *** Send &FF to MMC two times ***
.MMC_16Clocks
        LDY #2
        \\ fall through to

\\ *** Send &FF to MMC Y times ***
\\ Y=0=256
IF NOT(_ELECTRON_)
.MMC_SlowClocks
        \\ fall through to
ENDIF

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
        DO_JSR_MAP
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
        DO_JSR_UNMAP
        RTS
}


\\ *** Wait for data token ***
.MMC_WaitForData
{
        DO_JSR_MAP
        LDX #&FF
.loop
        STX spi_port%
        JSR spiwait
        LDA spi_port%
        CMP #&FE
        BNE loop
        DO_JSR_UNMAP
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

        DO_JSR_MAP
        LDA #&01
        STA spi_active%

.loop
        LDA #&FF
        STA spi_port%
        JSR spiwait
        NOP
IF _ELECTRON_
        NOP
ENDIF
        LDA spi_port%
        STA (datptr%),Y
        INY
        DEX
        BNE loop
        LDA #&00
        STA spi_active%
        DO_JSR_UNMAP
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
IF _ELECTRON_
        JMP spi_write_byte
ELSE
        JMP spi_write_byte_remap
ENDIF
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
        DO_JSR_MAP
        LDA #&01
        STA spi_active%
        LDY TubeNoTransferIf0
        BNE tube
.loop1
        LDA (datptr%),Y
        JSR spi_write_byte
        INY
        BNE loop1
        DO_JSR_UNMAP
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
        DO_JSR_UNMAP
        RTS
}

\\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
        LDY #0
.loop
        LDA buf%,Y
IF _ELECTRON_
        JSR spi_write_byte
ELSE
        JSR spi_write_byte_remap
ENDIF
        INY
        BNE loop
        RTS
}
