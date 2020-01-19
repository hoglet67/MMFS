\\ EXPERIMENTAL DRIVER FOR ELK PLUS 1
\\ WITH MMC CONNECTED TO PRINTER PORT

data%=&FC71
status%=&FC72

\\ MOSI is connected to D0
\\ SCK is connected to D1
\\ MISO is connected to Ack

clockbit%=&02

one_clockhigh%=&FF
one_clocklow%=&FF-clockbit%

\\ This is unused on the Electron, according to the EAUG
sr%=&F8

\\ Start of Plus One specific code

MACRO READ_BIT
    STX data%
    STA data%
    ROL status%
    ROL sr%
ENDMACRO

\\ Read byte (User Port)
\\ Write FF
.MMC_GetByte
.P1_ReadByte
    LDA #one_clockhigh%
    LDX #one_clocklow%
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
    RTS

\\ wait for response bit
\\ ie for clear bit
.P1_WaitResp
{
    LDA #one_clockhigh%
    LDX #one_clocklow%
    LDY #0
.loop
    DEY
    BEQ timeout
    STX data%
    STA data%
    ROL status%
    BCS loop
.timeout
    ROL sr%
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
    STA data%
    ORA #clockbit%
    STA data%
NEXT
    RTS
}

\\ RESET DEVICE
.MMC_DEVICE_RESET
    RTS

\\ INITIALIZE DEVICE IN SPI MODE
.MMC_SlowClocks
    JMP MMC_Clocks


INCLUDE "MMC_PrinterCommon.asm"
