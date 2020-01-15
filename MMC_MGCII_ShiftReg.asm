\\ EXPERIMENTAL DRIVER FOR MGCII in SHIFT REGISTER MODE

data%=&FCD8     \\ output: MOSI = bit 0, SCK = bit 1
                \\ input:  MISO = bit 7

shifter%=&FCD4  \\ input/output: MOSI/SCK shift register

chipsel%=&FCD9  \\ output: bit 0 = SD_CS1, bit 1 = SD_CS2


        \\ RESET DEVICE
.MMC_DEVICE_RESET
        LDA #&FE
        STA chipsel%
        RTS

        \\ Read byte (User Port)
        \\ Write FF
.MMC_GetByte
        LDA #&FF
        STA shifter%
        JSR donothing
        LDA shifter%
        RTS


        \\ *** Send &FF to MMC Y times ***
        \\ Y=0=256
.MMC_16Clocks
        LDY #2
.MMC_Clocks
{
        LDA #&FF
.clk1
        STA shifter%
        JSR donothing
        DEY
        BNE clk1
}
.donothing
        \\ JSR+RTS = 12 cycles
        RTS                             ; A=&FF, Y=0


        \\ *** Send command to MMC ***
        \\ On exit A=result, Z=result=0
.MMC_DoCommand
{
        LDX #0
        LDY #8
.dcmd1
        LDA cmdseq%,X
        STA shifter%                    ;\ 2 - write
        NOP                             ;\ 2
        NOP                             ;\ 2
        INX                             ;\ 2
        DEY                             ;\ 2
        BNE dcmd1                       ;\ 2
        \ Wait for response, Y=0
.wR1mm
        JSR donothing
        LDA shifter%
        BPL dcmdex
        LDA #&FF
        STA shifter%
        DEY
        BNE wR1mm
        CMP #0
.dcmdex
IF _DEBUG_MMC
{
        PHP
        PHA
        LDY #0
.loop
        LDA cmdseq%,Y
        JSR PrintHex
        INY
        CPY #7
        BNE loop
        LDA #':'
        JSR OSWRCH
        PLA
        PHA
        JSR PrintHex
        JSR OSNEWL
        PLA
        PLP
}
ENDIF
        RTS
}                                       ; A=result, X=X+8, Y=?


        \\ *** Wait for data token ***
.MMC_WaitForData
{
        LDX #&FF
.wl1
        STX shifter%
        JSR donothing                   ;\ 12
        LDA shifter%
        CMP #&FE                        ;\ data token
        BNE wl1
        RTS                             ; A=&FE, X=&FF, Y unchanged
}


        \\ *** Read 256 bytes to datptr ***
.MMC_Read256
{
        LDX #&FF
        STX shifter%
        LDY TubeNoTransferIf0           ;\ 4
        BNE rdlT20                      ;\ 2

        NOP                             ;\ 2
        NOP                             ;\ 2
        NOP                             ;\ 2
.rdl1
        LDA shifter%                    ;\ 2 - read
        STX shifter%                    ;\ 2 - write
        STA (datptr%),Y                 ;\ 6
        INY                             ;\ 2
        CPY #&FF                        ;\ 2
        BNE rdl1                        ;\ 2
        LDA shifter%                    ;\ 2 - read
        STA (datptr%),Y
        RTS

.rdlT20                                 ;\ 7
        LDY #0                          ;\ 9
        JMP rdlT2                       ;\ 12
}


        \\ *** Read "byteslastsector" bytes
        \\ to datptr ***
.MMC_ReadBLS
{
        LDX #&FF
        STX shifter%                    ;\ write
        LDY TubeNoTransferIf0           ;\ 4
        BNE rdlT1                       ;\ 2
        DEC byteslastsec%               ;\ 6
        BEQ rdl3                        ;\ 2
.rdl2
        LDA shifter%                    ;\ 2 - read
        STX shifter%                    ;\ 2 - write
        STA (datptr%),Y                 ;\ 6
        INY                             ;\ 2
        DEC byteslastsec%               ;\ 6
        BNE rdl2                        ;\ 2
.rdl3
        LDA shifter%                    ;\ 2 - read
        STA (datptr%),Y
        RTS
}

        \\ TUBE
        \\ 24us delay=48 cycles
        \ \ (7)
.rdlT1
        LDY byteslastsec%               ;\ (9)
.rdlT2
{
        DEY                             ;(11)
        BEQ rdlT4                       ;(14)
.rdlT3
        NOP                             ;\ 2=37
        LDA shifter%                    ;\ 4=41
        STX shifter%                    ;\ 4=45
        STA TUBE_R3_DATA                ;\ 4=49
        JSR donothing                   ;\ 12
        JSR donothing                   ;\ 12=24
        NOP                             ;\ 2=26
        NOP                             ;\ 2=28
        NOP                             ;\ 2=30
        DEY                             ;\ 2=32
        BNE rdlT3                       ;\ 3=35

        NOP                             ;\ 2=36
        NOP                             ;\ 2=38
        NOP                             ;\ 2=40
        NOP                             ;\ 2=42

.rdlT4
        LDA shifter%                    ;\ (17) 4=46
        STA TUBE_R3_DATA                ;\ 4=50
        RTS
}


        \\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
{
        LDA #&FF
        STA CurrentCat

        LDY #0
        LDX #&FF
        STX shifter%
        JSR donothing                   ;\ 12
.rdl4
        LDA shifter%                    ;\ 2 - read
        STX shifter%                    ;\ 2 - write
        STA buf%,Y                      ;\ \ 5
        INY                             ;\ 2
        CPY #&FF                        ;\ 2
        BNE rdl4                        ;\ 2
        LDA shifter%                    ;\ 2 - read
        STA buf%,Y
        RTS
}

        \\ **** Send Data Token to card ****
.MMC_SendingData
{
        LDX #&FF
        STX shifter%
        JSR donothing
        STX shifter%
        JSR donothing
        DEX
        STX shifter%
        RTS
}

        \\ **** Complete Write Operation *****
.MMC_EndWrite
{
        JSR MMC_16Clocks
        LDX #&FF
        STX shifter%
        JSR donothing                   ;\ 12
        LDA shifter%
        TAY
        AND #&1F
        CMP #5
        BNE errWrite2

        LDA #&FF
.ew1
        STX shifter%
        JSR donothing
        CMP shifter%
        BNE ew1
        RTS
}

        \\ **** Write 256 bytes from dataptr% ****
.MMC_Write256
{
        LDY TubeNoTransferIf0
        BNE wrT1
.wr1
        LDA (datptr%),Y                 ;\ 6
        STA shifter%                    ;\ 4
        JSR donothing                   ;\ 12
        INY                             ;\ 2
        BNE wr1                         ;\ 3
        RTS

        \.wrT1  ; To tube 24us delay
.wrT1
        LDY #0
.wrT2
        LDA TUBE_R3_DATA                ;\ 4
        STA shifter%                    ;\ 4
        JSR donothing                   ;\ 12
        INY                             ;\ 2
        BNE wrT2                        ;\ 3
        RTS
}

        \\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
        LDY #0
.wbm1
        LDA buf%,Y                      ;\ 4
        STA shifter%                    ;\ 4
        JSR donothing                   ;\ 12
        INY                             ;\ 2
        BNE wbm1                        ;\ 3
        RTS
}

\\ NOTE: When SR running of Phi2, then it takes 16
\\ Phi2 cycles to shift out the data, so we need to ensure
\\ the above loops don't execute any faster than this
