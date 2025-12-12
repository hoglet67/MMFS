\\ Open source driver that is compatible with Ramtop Retro's
\\ ElkSD128/MasterSD cartridges.

\\ Note: the _ELECTRON_ flag is used to determine Electron vs Master

\\ Note: there is some scope to reduce the code size. For example,
\\ removing CMP #&00 and using JMP instead of JSR+RTS


_PLUS1_     = TRUE

spi_base    = &FC80

spi_data    = spi_base
spi_control = spi_base+1
spi_status  = spi_base+1

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

MACRO DO_JSR_MAP
        JSR map_internal_io
ENDMACRO

MACRO DO_JSR_UNMAP
        JSR unmap_internal_io
ENDMACRO

MACRO DO_INLINE_MAP
        PHA
        LDA &FE34
        STA &90            ;; TODO: Use of &90 is dodgy
        ORA #&20
        STA &FE34
        PLA
ENDMACRO

MACRO DO_INLINE_UNMAP
        LDA &90            ;; TODO: Use of &90 is dodgy
        STA &FE34
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


\\ Read byte from SPI data port and return in A
.MMC_GetByte
{
        DO_JSR_MAP
        LDA #&00
        STA spi_control ;; slow clock
        LDA #&FF
        STA spi_data
.loop
        LDA spi_status
        CMP #&00
        BNE loop
        LDA spi_data
        DO_JSR_UNMAP
        RTS
}

\\ Wait for SPI to be ready for more data
.wait_spi_ready
{
.loop
        LDA spi_status
        CMP #&00
        BNE loop
        RTS
}

\\ TODO: This code is currently unused
{
        DO_JSR_MAP
.loop
        DEY
        BEQ done
        LDA #&FF
        STA spi_data
        JSR wait_spi_ready
        LDA spi_data
        STA &F8   ;; &F8 is unused on the Elk, but not on the Master
        ROL &F8
        BCS loop
.done
        LDA spi_data
        DO_JSR_UNMAP
        RTS
}

\\ Write byte in A to SPI data port
IF NOT(_ELECTRON_)
.spi_write_byte_remap
{
        DO_INLINE_MAP
        STA spi_data
.loop
        LDA spi_status
        CMP #&00
        BNE loop
        DO_INLINE_UNMAP
        LDA #&00
        RTS
}
ENDIF

.spi_write_byte
{
        STA spi_data
.loop
        LDA spi_status
        CMP #&00
        BNE loop
IF NOT(_ELECTRON_)
        LDA #&00     ; This is unnecessary
ENDIF
        RTS
}

.MMC_DEVICE_RESET
{
        RTS          ; This could use an existing RTS
}

IF _ELECTRON_ AND NOT(_PLUS1_)
.MMC_SlowClocks
        JMP MMC_Clocks
ENDIF

\\ *** Send &FF to MMC two times ***
.MMC_16Clocks
        LDY #2
        \\ fall through to

IF NOT(_ELECTRON_)
.MMC_SlowClocks
        \\ fall through to
ENDIF

\\ *** Send &FF to MMC Y times ***
\\ Y=0=256
.MMC_Clocks
{
.loop
        JSR MMC_GetByte
        DEY
        BNE loop
        RTS
}

\\ *** Send command to MMC ***
\\ On exit A=result, Z=result=0
.MMC_DoCommand
{
        DO_JSR_MAP
        LDX #0
        LDY #8
.loop1
        LDA cmdseq%,X
        STA spi_data
        JSR wait_spi_ready
        NOP
        NOP
        INX
        DEY
        BNE loop1
        LDA #&FF
        \ Wait for response, Y=0
.loop2
        STA spi_data
        JSR wait_spi_ready
        LDA spi_data
        BPL done
        DEY
        BNE loop2
        CMP #0
.done
IF _DEBUG_MMC
        PHP
        PHA
        LDY #0
.loop3
        LDA cmdseq%,Y
        JSR PrintHex
        INY
        CPY #7
        BNE loop3
        LDA #':'
        JSR OSWRCH
        PLA
        PHA
        JSR PrintHex
        JSR OSNEWL
        PLA
        PLP
ENDIF
        DO_JSR_UNMAP
        RTS
}                                       ; A=result, X=X+8, Y=?

\\ *** Wait for data token ***
.MMC_WaitForData
{
        DO_JSR_MAP
        LDX #&FF
.loop
        STX spi_data
        JSR wait_spi_ready
        LDA spi_data
        CMP #&FE                        ;\ data token
        BNE loop
        DO_JSR_UNMAP
        RTS                             ; A=&FE, X=&FF, Y unchanged
}

\\ *** Read 256 bytes to datptr ***
.MMC_Read256
        LDX #&00
        BEQ mmc_read

\\ *** Read "byteslastsector" byte to datptr ***
.MMC_ReadBLS
        LDX byteslastsec%

.mmc_read
{
        LDY #&00
        LDA TubeNoTransferIf0
        BNE tube

        DO_JSR_MAP
        LDA #&01
        STA spi_control ;; fast clock
.loop1
        LDA #&FF
        STA spi_data
        JSR wait_spi_ready
        NOP
IF _ELECTRON_
        NOP
ENDIF
        LDA spi_data
        STA (datptr%),Y
        INY
        DEX
        BNE loop1
        LDA #&00
        STA spi_control ;; slow clock
        DO_JSR_UNMAP
        RTS

.tube
        TXA
        PHA
        JSR MMC_GetByte
        STA TUBE_R3_DATA
        PLA
        TAX
        INY
        DEX
        BNE tube
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
        STA buf%,Y
        INY
        BNE loop
        RTS
}

\\ **** Send Data Token to card ****
.MMC_SendingData
{
        LDY #$02
        JSR MMC_Clocks
        LDA #&FE
IF _ELECTRON_
        JMP spi_write_byte
ELSE
        JMP spi_write_byte_remap
ENDIF
}

\\ **** Complete Write Operation *****
.MMC_EndWrite
{
        JSR MMC_16Clocks
.loop1
        JSR MMC_GetByte
        TAY
        AND #&1F
        CMP #&1F
        BEQ loop1
        CMP #&05
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
        STA spi_control ;; fast clock
        LDY TubeNoTransferIf0
        BNE tube
.loop
        LDA (datptr%),Y
        JSR spi_write_byte
        INY
        BNE loop
        DO_JSR_UNMAP
        RTS

.tube
        LDY #0
.tube_loop
        LDA TUBE_R3_DATA
        JSR spi_write_byte
        INY
        BNE tube_loop
        LDA #$00
        STA spi_control ;; slow clock
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
