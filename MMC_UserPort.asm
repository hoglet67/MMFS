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
    LDA #(3 + msbits)
    STA iorb%
    LDA ddrb%
    ORA #ddrmask
    STA ddrb%
    JSR ShiftRegMode0
    LDA #&1C
    STA ier%
    RTS

    \\ Read byte (User Port)
    \\ Write FF
.MMC_GetByte
.UP_ReadByteX
{
    LDA #(3 + msbits)   \\ Ensure CLK and MOSI both high
    STA iorb%
    JSR ShiftRegMode2
    LDA #4
.wait
    BIT ifr%            \\ Bit 2 of IFR is the Shift Reg Interrupt flag
    BEQ wait
    JSR ShiftRegMode0
    LDA sr%
    RTS
}
        
    \\ This is always entered with X and A with the correct values
.UP_ReadBits7
    STX iorb%           ;\1
    STA iorb%
    STX iorb%           ;\2
    STA iorb%
    STX iorb%           ;\3
    STA iorb%
    STX iorb%           ;\4
    STA iorb%
    STX iorb%           ;\5
    STA iorb%
    STX iorb%           ;\6
    STA iorb%
    STX iorb%           ;\7
    STA iorb%
    LDA sr%
    RTS

    \\ Write byte (User Port)
    \\ Ignore byte in
.UP_WriteByte
{
IF _TURBOMMC
    PHA
    JSR ShiftRegMode6
    PLA
    STA sr%
    LDA #4
.wait
    BIT ifr%
    BEQ wait
    JMP ShiftRegMode6Exit
ELSE
    ASL A
    FOR N, 0, 7
        ROL A
        AND #msmask
        STA iorb%
        ORA #2
        STA iorb%
    NEXT
ENDIF
    RTS
}

    \\ *** Send &FF to MMC Y times ***
    \\ Y=0=256
.MMC_16Clocks
    LDY #2
.MMC_Clocks

{
.clku1
    JSR UP_ReadByteX        ; Writes &FF
    DEY
    BNE clku1
    RTS             ; A=SR, X=1, Y=0
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
IF _DEBUG_MMC
    JSR UP_ReadBits7        
    PHP
    PHA
    LDY #0
.dcmdu2
    LDA cmdseq%,Y
    JSR PrintHex
    INY
    CPY #7
    BNE dcmdu2
    LDA #':'
    JSR OSWRCH
    PLA
    PHA
    JSR PrintHex
    JSR OSNEWL       
    PLA
    PLP
    RTS
ELSE
    JMP UP_ReadBits7
ENDIF
}

    \\ wait for response bit
    \\ ie for clear bit (User Port only)
.waitresp_up
{
    LDX #(1 + msbits)
    LDY #0
.wrup
    DEY
    BEQ wrup_timeout
    LDA #(3 + msbits)
    STX iorb%
    STA iorb%
    LDA sr%
    AND #1
    BNE wrup
.wrup_timeout
    LDA #(3 + msbits)
    RTS
}


    \\ *** Wait for data token ***
.MMC_WaitForData
{

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
    LDY #0
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
{        
    JSR WaitForShiftDone
    STA TUBE_R3_DATA
    NOP
    NOP
    NOP
    INY
    DEX
    BNE MMC_ReadToTube
.tube_delay
    RTS
}

    \\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
    LDX #&FF
    STX CurrentCat
    INX

    JSR ShiftRegMode2

    LDY #0
.rdbuf2
    JSR WaitForShiftDone
    STA buf%, Y
    INY
    DEX
    BNE rdbuf2
    RTS

\\ Wait for the shift reg to complete shifing, and return the value in A.
\\
\\ If this is the last byte, return to mode 0 before reading the shift reg.
\\
\\ This could be coded in fewer instructions, but it's done this way to minimise
\\ the time between testing for the interrupt flag, and reading the shift reg
\\ After reading the shift reg, the next byte will be ready 16us later
\\ Which gives us ~32 "free" instruction cycles
\\
\\  Here's the common path, starting with the read of the shift reg
\\  LDA sr%             (A)    0
\\  RTS                     6  6
\\  STA (datptr%),Y         6 12
\\  INY                     2 14
\\  DEX                     2 16
\\  BNE MMC_ReadToMemory    3 19
\\.MMC_ReadToMemory
\\  JSR WaitForShiftDone    6 25
\\  LDA #4                  2 27
\\  CPX #1                  2 29
\\  BEQ lastByte            2 31
\\.notLastByte
\\  BIT ifr%            (B) 4 35 IFR should set again just in time
\\  BEQ notLastByte         2 37
\\  LDA sr%             (A) 4 41
\\
\\  Note: the above does not account for 1MHz slow down on (A,B)
\\  because it's hard to tell empirically these add 1 or 2 cycles.
\\
\\  It turns out the (A) adds 1 cycle, and (B) adds 2 cycles, giving
\\  total of 44 cycles, or 22us per byte.
\\
\\  I tried optmising this further, by replacing:
\\    BIT ifr%
\\    BEQ notLastByte
\\  with NOPs.
\\
\\  with 3 NOPs, data transfer was reliable, and the code took 21us/byte:
\\
\\  LDA sr%             (A)    0
\\  RTS                     6  6
\\  STA (datptr%),Y         6 12
\\  INY                     2 14
\\  DEX                     2 16
\\  BNE MMC_ReadToMemory    3 19
\\.MMC_ReadToMemory
\\  JSR WaitForShiftDone    6 25
\\  LDA #4                  2 27
\\  CPX #1                  2 29
\\  BEQ lastByte            2 31
\\.notLastByte
\\  NOP                     2 33
\\  NOP                     2 35
\\  NOP                     2 37
\\  LDA sr%             (A) 4 41
\\
\\ In this situation (A) is stretched for 1 cycle, giving 42 cycles total.
\\
\\  with 2 NOPs, and data transfer still reliable
\\
\\  LDA sr%             (A)    0
\\  RTS                     6  6
\\  STA (datptr%),Y         6 12
\\  INY                     2 14
\\  DEX                     2 16
\\  BNE MMC_ReadToMemory    3 19
\\.MMC_ReadToMemory
\\  JSR WaitForShiftDone    6 25
\\  LDA #4                  2 27
\\  CPX #1                  2 29
\\  BEQ lastByte            2 31
\\.notLastByte
\\  NOP                     2 33
\\  NOP                     2 35
\\  LDA sr%             (A) 4 39
\\
\\ In this situation (A) is stretched for 1 cycle, giving 40 cycles total.
\\
\\  with 1 NOPs, and data transfer still reliable
\\
\\  LDA sr%             (A)    0
\\  RTS                     6  6
\\  STA (datptr%),Y         6 12
\\  INY                     2 14
\\  DEX                     2 16
\\  BNE MMC_ReadToMemory    3 19
\\.MMC_ReadToMemory
\\  JSR WaitForShiftDone    6 25
\\  LDA #4                  2 27
\\  CPX #1                  2 29
\\  BEQ lastByte            2 31
\\.notLastByte
\\  NOP                     2 33
\\  LDA sr%             (A) 4 37
\\
\\ In this situation (A) is stretched for 1 cycle, giving 38 cycles total.
\\
\\  with 0 NOPs, data transfer is unreliable, as every other byte is skipped
\\
\\  LDA sr%             (A)    0
\\  RTS                     6  6
\\  STA (datptr%),Y         6 12
\\  INY                     2 14
\\  DEX                     2 16
\\  BNE MMC_ReadToMemory    3 19
\\.MMC_ReadToMemory
\\  JSR WaitForShiftDone    6 25
\\  LDA #4                  2 27
\\  CPX #1                  2 29
\\  BEQ lastByte            2 31
\\.notLastByte
\\  LDA sr%             (A) 4 35
\\
\\ In this situation (A) is stretched for 1 cycle, giving 36 cycles total.
\\
\\ I tried adding just one cycle back into the main loop:
\\
\\  LDA sr%             (A)    0
\\  RTS                     6  6
\\  STA (datptr%),Y         6 12
\\  INY                     2 14
\\  DEX                     2 16
\\  BNE MMC_ReadToMemory    3 19
\\.MMC_ReadToMemory
\\  JSR WaitForShiftDone    6 25
\\  LDA #4                  2 27
\\  CPX #1                  2 29
\\  BNE notLastByte         3 32
\\.notLastByte
\\  LDA sr%             (A) 4 36
\\
\\ In this situation (A) is stretched for 2 cycle, again giving 38 cycles total, which is the same as one NOP.
\\
\\ So, the limit of the 6522 in the Beeb in SR Mode 2 is 19us/byte.
\\
\\ There is other overhead, between blocks, and interrupts.
\\
\\ 19us/byte for &7000 bytes actually took 680ms. This is excactly the value SWEH measured with the TurboMMC ROM.
\\
\\ I'm goting to return to the code as the top of this thread, as I don't like doing things by dead reconning.
\\
\\ 22us/byte for &7000 bytes actually took 770ms.

.WaitForShiftDone
{
    LDA #4            \\ Bit 2 of IFR is the Shift Reg Interrupt flag
    CPX #1            \\ test if the last byte
    BEQ lastByte      \\ so we can return to mode zero before reading it
.notLastByte
    BIT ifr%          \\ wait for the SR interrupt flag to be set
    BEQ notLastByte
    LDA sr%           \\ read the data byte, and clear the SR interrupt flag
    RTS
.lastByte
    BIT ifr%          \\ wait for the SR interrupt flag to be set
    BEQ lastByte
    JSR ShiftRegMode0 \\ returning to mode 0 here avoids an addional byte read
    LDA sr%           \\ read the data byte, and clear the SR interrupt flag
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
    LDA sr%    \\ Start the first read
    RTS

IF _TURBOMMC
.ShiftRegMode6        \\ Sequence here is important to avoid brief bus conflicts
    LDA #&17          \\ 00010111
                      \\ PB0=1 sets MOSI to 1 (not very important)
                      \\ PB1=1 sets SCLK to 1 (important to avoid glitches)
                      \\ PB2=1 disables buffer connecting MISO to CB2
                      \\ PB3=0 enables  buffer connecting CB2 to MOSI
                      \\ PB4=1 disables buffer connecting PB0 to MOSI
    STA iorb%         \\ Flip the direction of the data bus
    LDA ddrb%         \\ Set PB1 to being an input
    AND #&FD          \\ 11111101
    STA ddrb%         \\ Briefly the clock will float
    LDA acr%          \\ Change the SR mode last, to avoid conflicts
    AND #&E3          \\ 11100011
    ORA #&18          \\ 00011000 = SR Mode 6
    STA acr%          \\ CB1, CB2 are both outputs
    RTS

.ShiftRegMode6Exit    \\ Sequence here is important to avoid brief bus conflicts
    JSR ShiftRegMode0 \\ CB1,2 are both inputs
                      \\ Briefly the clock will float
                      \\ PB1 is set as an output again
    LDA #&0B          \\ 00001011
                      \\ PB0=1 sets MOSI to 1 (not very important)
                      \\ PB1=1 sets SCLK to 1 (important to avoid glitches)
                      \\ PB2=0 enables  buffer connecting MISO to CB2
                      \\ PB3=1 disables buffer connecting CB2 to MOSI
                      \\ PB4=0 enables  buffer connecting PB0 to MOSI
    STA iorb%         \\ Flip the direction of the data bus
    RTS
ENDIF

    \\ **** Send Data Token to card ****
.MMC_SendingData
{
    JSR MMC_16Clocks
    LDA #&FE
    JMP UP_WriteByte
}

    \\ **** Complete Write Operation *****
.MMC_EndWrite
{
    JSR MMC_16Clocks
.ewu1
    JSR UP_ReadByteX
    TAY
    AND #&1F
    CMP #&1F
    BEQ ewu1
    CMP #5
    BNE errWrite2

.ewu2
    JSR UP_ReadByteX
    CMP #&FF
    BNE ewu2
    RTS
}

    \\ **** Write 256 bytes from dataptr% ****
.MMC_Write256
{
IF _TURBOMMC
    JSR ShiftRegMode6
ENDIF
    LDY TubeNoTransferIf0
    BNE wruT1

.wru1
    LDA (datptr%),Y
IF _TURBOMMC
    STA sr%
    LDA #4
.wait
    BIT ifr%
    BEQ wait
ELSE
    JSR UP_WriteByte
ENDIF
    INY
    BNE wru1
IF _TURBOMMC
    BEQ ShiftRegMode6Exit
ELSE
    RTS
ENDIF
}

.wruT1
{
    LDY #0
.wruT2
    LDA TUBE_R3_DATA
IF _TURBOMMC
    STA sr%
    LDA #4
.wait
    BIT ifr%
    BEQ wait
ELSE
    JSR UP_WriteByte
ENDIF
    INY
    BNE wruT2
IF _TURBOMMC
    BEQ ShiftRegMode6Exit
ELSE
    RTS
ENDIF
}

    \\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
    LDY #0
IF _TURBOMMC
    JSR ShiftRegMode6
ENDIF
.wbu1
    LDA buf%,Y
IF _TURBOMMC
    STA sr%
    LDA #4
.wait
    BIT ifr%
    BEQ wait
ELSE
    JSR UP_WriteByte
ENDIF
    INY
    BNE wbu1
IF _TURBOMMC
    BEQ ShiftRegMode6Exit
ELSE
    RTS
ENDIF
}
