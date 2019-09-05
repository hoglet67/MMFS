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

\\ Read byte (User Port)
\\ Write FF
.MMC_GetByte
.P1_ReadByte
    LDA #one_clockhigh%
    LDX #one_clocklow%
FOR n, 0, 7
    STX data%
    STA data%
    ROL status%
    ROL sr%
NEXT
    LDA sr%
    RTS

\\ This is always entered with A and X with the correct values
.P1_ReadBits7
FOR n, 0, 2
    STX data%
    STA data%
    ROL status%
    ROL sr%
NEXT

 \\ This is always entered with A and X with the correct values
.P1_ReadBits4
FOR n, 0, 3
    STX data%
    STA data%
    ROL status%
    ROL sr%
NEXT
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

INCLUDE "MMC_PrinterCommon.asm"
