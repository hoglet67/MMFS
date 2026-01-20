\\ Driver for the SPI port on ElkSD-Plus1 Rev2 and ElkVGA P2 hardware,
\\
\\ &FC80 is data port for reading/writing
\\ &FC81 is status and control port, read LSB for SPI controller state (0 = idle, 1 = busy)
\\ and write for clock speed control (0 = slow clock, 1 = fast clock)
\\
\\ On ElkSD-Plus1 Rev 2 hardware slow clock sets SPI CLK to PHI0/8, fast clock is 2MHz


spi_port%=&FC80
spi_active%=&FC81

_MASTERSD_ = not(_ELECTRON_)

IF _MASTERSD_

acccon% = &FE34

MACRO DO_INLINE_MAP
        LDA acccon%
        STA &90                  ; TODO: Use of &90 is dodgy
        ORA #&20
        STA acccon%
ENDMACRO

MACRO DO_INLINE_UNMAP
        LDA &90                  ; TODO: Use of &90 is dodgy
        STA acccon%
ENDMACRO

.map_internal_io
{
        DO_INLINE_MAP            ; corrupts A
        RTS
}

.unmap_internal_io
{
        PHA
        DO_INLINE_UNMAP          ; corrupts A
        PLA
        RTS
}

ENDIF

\\ Read byte from SPI data port (simultaneously writing &FF)
.MMC_GetByte
{
IF _MASTERSD_
        JSR map_internal_io      ; corrupts A
ENDIF
        LDA #&00
        STA spi_active%
        LDA #$FF
        STA spi_port%
.loop
        LDA spi_active%
        BNE loop
        LDA spi_port%
IF _MASTERSD_
        JMP unmap_internal_io
ELSE
        RTS
ENDIF
}

\\ Write byte in A to SPI data port
IF _MASTERSD_
.spi_write_byte_remap
{
        PHA
        DO_INLINE_MAP            ; corrupts A
        PLA
        STA spi_port%
.loop
        LDA spi_active%
        BNE loop
        DO_INLINE_UNMAP          ; corrupts A
        LDA #&00
        RTS
}
ENDIF

\\ Write byte to SPI data port (returns A=0)
.spi_write_byte
{
        STA spi_port%
        \\ fall through to
}

.spiwait
{
.loop
        LDA spi_active%
        BNE loop
        RTS                      ; exit with A=0
}

\\ More generic code below tis point

\\ RESET DEVICE
.MMC_DEVICE_RESET
{
        RTS                      ; This could use an existing RTS
}

\\ *** Send &FF to MMC two times ***
.MMC_16Clocks
        LDY #2
        \\ fall through to

\\ *** Send &FF to MMC Y times ***
\\ Y=0=256
.MMC_SlowClocks
        \\ fall through to

.MMC_Clocks
{
.loop
        JSR MMC_GetByte          ; Writes &FF
        DEY
        BNE loop
        RTS                      ; A=SR, X=one%, Y=0
}



.MMC_DoCommand
{
IF _MASTERSD_
        JSR map_internal_io      ; corrupts A
ENDIF
        LDX #0
        LDY #8
.loop1
        LDA cmdseq%,X
        STA spi_port%
IF _DEBUG_MMC
        JSR PrintHex
ENDIF
        JSR spiwait
        INX
        DEY
        BNE loop1
IF _DEBUG_MMC
        LDA #':'
        JSR OSWRCH
ENDIF
        LDA #&FF
        \ Wait for response, Y=0
.loop2
        STA spi_port%            ; assume A=&FF
        JSR spiwait
        LDA spi_port%
        BPL done
        DEY
        BNE loop2
.done
IF _DEBUG_MMC
        PHA
        JSR PrintHex
        JSR OSNEWL
        PLA
ENDIF
IF _MASTERSD_
        JMP unmap_internal_io    ; includes PHA/PLA which sets flags on exit
ELSE
        CMP #0                   ; set flags on exit
        RTS
ENDIF
}


\\ *** Wait for data token ***
.MMC_WaitForData
{
IF _MASTERSD_
        JSR map_internal_io      ; corrupts A
ENDIF
        LDX #&FF
.loop
        STX spi_port%
        JSR spiwait
        LDA spi_port%
        CMP #&FE
        BNE loop
IF _MASTERSD_
        JMP unmap_internal_io
ELSE
        RTS
ENDIF
}

\\ *** Read 256 bytes to datptr ***
.MMC_Read256
        LDX #0
        BEQ mmc_read

\\ *** Read "byteslastsector" bytes to datptr ***
.MMC_ReadBLS
        LDX byteslastsec%

.mmc_read
{
IF _MASTERSD_
        JSR map_internal_io      ; corrupts A
ENDIF
        LDA #&01
        STA spi_active%
        LDY TubeNoTransferIf0
        BNE tube
.loop1
        LDA #&FF
        STA spi_port%
        JSR spiwait
        LDA spi_port%
        STA (datptr%),Y
        INY
        DEX
        BNE loop1
        BEQ done                 ; branch always
.tube
        LDY #0
.loop2
        LDA #&FF
        STA spi_port%
        JSR spiwait
        LDA spi_port%
        STA TUBE_R3_DATA
        INY
        DEX
        BNE loop2
.done
        LDA #&00
        STA spi_active%
IF _MASTERSD_
        JMP unmap_internal_io
ELSE
        RTS
ENDIF
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
IF _MASTERSD_
        JMP spi_write_byte_remap
ELSE
        JMP spi_write_byte
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
IF _MASTERSD_
        JSR map_internal_io      ; corrupts A
ENDIF
        LDA #&01
        STA spi_active%
        LDY TubeNoTransferIf0
        BNE tube
.loop1
        LDA (datptr%),Y
        JSR spi_write_byte
        INY
        BNE loop1
        BEQ done                 ; branch always
.tube
        LDY #0
.loop2
        LDA TUBE_R3_DATA
        JSR spi_write_byte
        INY
        BNE loop2
.done
        LDA #&00
        STA spi_active%
IF _MASTERSD_
        JMP unmap_internal_io
ELSE
        RTS
ENDIF
}

\\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
        LDY #0
.loop
        LDA buf%,Y
IF _MASTERSD_
        JSR spi_write_byte_remap
ELSE
        JSR spi_write_byte
ENDIF
        INY
        BNE loop
        RTS
}
