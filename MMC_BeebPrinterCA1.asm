\\ EXPERIMENTAL DRIVER FOR BEEB
\\ WITH MMC CONNECTED TO PRINTER PORT

iora%=_VIA_BASE + &01
ddra%=_VIA_BASE + &03
pcr%= _VIA_BASE + &0C
ifr%= _VIA_BASE + &0d
iorx%=_VIA_BASE + &0f

\\ MOSI is connected to D0
\\ SCK  is connected to D1
\\ MISO is connected to CA1

\\ This is unused on the Beeb, according to JGH
sr%=&F8

clockbit%=&02

one_clockhigh%=&FF
one_clocklow%=&FF-clockbit%

MACRO READ_BIT
    \\ clock low, MISO changes on this edge
    LDA #one_clocklow%  \\ (2)
    STA iorx%           \\ (4) Don't clear CA1 int!
    \\ Update active edge
    LDA ifr%            \\ (4) IFR bit 1 set if active edge seen
    ROR A               \\ (2) Move to bit 0
    AND #&01            \\ (2) Mask off all other bits
    EOR pcr%            \\ (4) toggle active edge if necessary
    STA pcr%            \\ (4) bit 0 of A is now not(CA1)
    \\ Accumulate value of not(CA1) in zero page sr%
    ROR A               \\ (2) C=not(CA1)
    ROL sr%             \\ (5) Accumulate result (bit 7 sent first)
    \\ clock high, clear CA1 int
    LDA #one_clockhigh% \\ (2)
    STA iora%           \\ (4)
ENDMACRO

\\ Start of Beeb Printer Port Specific Code

\\ Read byte (User Port)
\\ Write FF
.MMC_GetByte
.P1_ReadByte
    \\ Read first bit
    READ_BIT
    \\ Fall through to...

\\ This is always entered with A and X with the correct values
.P1_ReadBits7
    READ_BIT
    READ_BIT
    READ_BIT
    READ_BIT
    READ_BIT
    READ_BIT
    READ_BIT
    LDA sr%
    EOR #&FF
    RTS

\\ wait for response bit
\\ ie for clear bit
.P1_WaitResp
{
    LDA pcr%
    AND #&FE
    STA pcr%             \\ Set CA1 active edge to negative
    LDA #one_clockhigh%
    STA iora%            \\ Reset CA1 int flag
    LDY #0
.loop
    DEY
    BEQ timeout
    LDX #one_clocklow%    \\ Clock low
    STX iorx%             \\ Don't clear CA1 int flag!
    LDA ifr%              \\ Sample CA1 int flag
    LDX #one_clockhigh%   \\ Clock high
    STX iora%             \\ Clear CA1 int flag
    AND #&02              \\ Was the initial falling edge seen?
    BEQ loop              \\ No, so loop back
.timeout
    LDA pcr%
    ORA #&01
    STA pcr%              \\ Set CA1 active edge to positive
    LDA #&01
    STA sr%               \\ Initialize the zero page sr to &FE (inverted)
    RTS
}

\\ Write byte (User Port)
\\ Ignore byte in
.P1_WriteByte
{
    ASL A
FOR N, 0, 7
    ROL A
    AND #&FD
    STA iora%
    ORA #clockbit%
    STA iora%
NEXT
    RTS
}

\\ RESET DEVICE
.MMC_DEVICE_RESET
    LDA ddra%
    AND #&7F
    ORA #&03
    STA ddra%
    RTS

\\ INITIALIZE DEVICE IN SPI MODE
.MMC_SlowClocks
    JMP MMC_Clocks

INCLUDE "MMC_PrinterCommon.asm"
