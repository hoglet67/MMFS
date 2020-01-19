\\ EXPERIMENTAL DRIVER FOR BEEB
\\ WITH MMC CONNECTED TO PRINTER PORT

iora%=_VIA_BASE + &01
ddra%=_VIA_BASE + &03

\\ MOSI is connected to D0
\\ SCK  is connected to D1
\\ MISO is connected to D7

clockbit%=&02

one_clockhigh%=&FF
one_clocklow%=&FF-clockbit%

MACRO READ_BIT
    STX iora%    \\ Take clock (D1) low
    ROL iora%    \\ Sample D7 (MISO) into C, and take clock (D1) high
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
    STX iora%
    ROL iora%
    BCS loop
.timeout
    ROL A
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
