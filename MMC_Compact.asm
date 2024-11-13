\\ EXPERIMENTAL DRIVER FOR BEEB
\\ WITH MMC CONNECTED TO PRINTER PORT

iorb%=_VIA_BASE + &00
ddrb%=_VIA_BASE + &02

\\ MOSI is connected to PB5
\\ SCK  is connected to PB6
\\ MISO is connected to PB7

clockbit%=&40

one_clockhigh%=&FF
one_clocklow%=&FF-clockbit%

MACRO READ_BIT
    STX iorb%    \\ Take clock (PB6) low
    ROL iorb%    \\ Sample D7 (MISO) into C, and take clock (PB6) high
    ROL A        \\ C=1 after this, because A starts off as FF
ENDMACRO

\\ Start of Beeb Printer Port Specific Code

\\ Read byte (User Port)
\\ Write FF
.MMC_GetByte
.P1_ReadByte
    LDA #one_clockhigh%
    LDX #one_clocklow%
    \\ Set carry so D0 (MOSI) remains high after ROL
    SEC
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
    RTS

\\ wait for response bit
\\ ie for clear bit
.P1_WaitResp
{
    LDA #&FF
    LDX #one_clocklow%
    LDY #0
.loop
    DEY
    BEQ timeout
    STX iorb%
    ROL iorb%
    BCS loop
.timeout
    ROL A
    RTS
}

\\ Write byte (User Port)
\\ Ignore byte in
.P1_WriteByte
{
    ROR A
    ROR A
FOR N, 0, 7
    AND #&FF-clockbit%
    STA iorb%
    ORA #clockbit%
    STA iorb%
    ROL A
NEXT
    RTS
}

\\ RESET DEVICE
.MMC_DEVICE_RESET
    LDA ddrb%
    AND #&7F   \\ PB7 is an input
    ORA #&60   \\ PB6/5 are outputs
    STA ddrb%
    RTS

\\ INITIALIZE DEVICE IN SPI MODE
.MMC_SlowClocks
    JMP MMC_Clocks

INCLUDE "MMC_PrinterCommon.asm"
