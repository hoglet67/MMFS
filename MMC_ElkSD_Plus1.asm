\\ Driver for the SPI port on ElkSD-Plus1 Rev2 and ElkVGA P2 hardware,
\\
\\ &FC80 is data port for reading/writing
\\ &FC81 is status and control port, read LSB for SPI controller state (0 = idle, 1 = busy)
\\ and write for clock speed control (0 = slow clock, 1 = fast clock)
\\
\\ The only timimg sensitive code is the Tube Transfer code, which
\\ needs to transfer no faster than 24us per byte. See cycle counts
\\ inline.
\\
\\ ElkSD-Plus1 Rev 2 timings:
\\
\\    Slow clock:
\\        250KHz max, min period 4us
\\        8 PHI0 cycles, timings depend on CPU cycle stretching
\\        SPI byte transfer: 32us
\\
\\    Fast clock:
\\        2MHz, period 0.5us (250ns high, 250ns low)
\\        SPI byte transfer: 4us
\\
\\    Tube Transfer timimgs (measured on scope)
\\        (these need to be >= 24us)
\\        LOAD: SD Card -> Tube (e.g.  Read256): 25.0us
\\        SAVE: Tube -> SD Card (e.g. Write256): 25.0us
\\        (these now both use the Fast clock)
\\
\\ MasterSD Rev 2 timings:
\\
\\    Slow clock:
\\        333KHz, period 3.0us (1us high, 2us low)
\\        SPI byte transfer: 24us
\\
\\    Fast clock:
\\        666KHz, period 1.5us (0.5us high, 1us low)
\\        SPI byte transfer: 12us
\\
\\    Tube Transfer timimgs (measured on scope)
\\        (these need to be >= 24us)
\\        LOAD: SD Card -> Tube (e.g.  Read256): 30.0us
\\        SAVE: Tube -> SD Card (e.g. Write256): 29.5us
\\        (these now both use the Fast clock)

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
        JMP unmap_internal_io    ; preserves A
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
        JMP unmap_internal_io    ; preserves A
ELSE
        RTS
ENDIF
}

\\ *** Read 256 bytes to datptr ***
.MMC_Read256
        LDX #0
        BEQ mmc_read

\\ *** Read "byteslastsector" bytes to datptr ***
\\ On exit: Y=number of bytes transferred (0=256)
\\
\\ Tube Timing notes:
\\
\\ All numbers are 2MHz cycles
\\
\\ ElkSDP1:
\\    Tube and Internal FCxx slow down to 1MHz
\\    RAM slow down to 1MHz
\\    JSR/RTS take 9 cycles as they includes 3 RAM accesses
\\    LDA/STA to IO takes 5 or 6 cycles depending on phase
\\    SPI transfer takes 4us, so 1 iteration of spiwait loop
\\
\\ MasterSD:
\\    Everything (including Tube and internal FCxx) accessed at 2MHz
\\    JSR takes 6 cycles
\\    RTS takes 6 cycles
\\    LDA/STA to IO takes 4 cycles
\\    SPI transfer takes 12us, so 4 iteration of spiwait loop

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
.loop2                           ; ElkSDP1            MasterSD
        LDA #&FF                 ; 2                  2
        STA spi_port%            ; 5                  4
        JSR spiwait              ; 9+5+2+9            6+(4+3)*3+4+2+6
        LDA spi_port%            ; 5                  4
        STA TUBE_R3_DATA         ; 6                  4
        INY                      ; 2                  2
        DEX                      ; 2                  2
        BNE loop2                ; 3                  3
.done                            ; ---                ---
        LDA #&00                 ; 50 = 25us          60 = 30us
        STA spi_active%          ; ---                ---
IF _MASTERSD_
        JMP unmap_internal_io    ; preserves A
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
\\
\\ Tube Timing notes:
\\
\\ All numbers are 2MHz cycles
\\
\\ ElkSDP1:
\\    Tube and Internal FCxx slow down to 1MHz
\\    RAM slow down to 1MHz
\\    JSR/RTS take 9 cycles as they includes 3 RAM accesses
\\    LDA/STA to IO takes 5 or 6 cycles depending on phase
\\    SPI transfer takes 4us, so 2 iterations of spiwait loop
\\
\\ MasterSD:
\\    Everything (including Tube and internal FCxx) accessed at 2MHz
\\    JSR takes 6 cycles
\\    RTS takes 6 cycles
\\    LDA/STA to IO takes 4 cycles
\\    SPI transfer takes 12us, so 5 iterations of spiwait loop

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
.loop2                           ; ElkSDP1            MasterSD
        LDA TUBE_R3_DATA         ; 6                  4
        JSR spi_write_byte       ; 9+6+(5+3)*1+5+2+9  6+4+(4+3)*4+4+2+6
        INY                      ; 2                  2
        BNE loop2                ; 3                  3
.done                            ; ---                ---
        LDA #&00                 ; 50 = 25us          59 = 29.5us
        STA spi_active%          ; ---                ---
IF _MASTERSD_
        JMP unmap_internal_io    ; preserves A
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
