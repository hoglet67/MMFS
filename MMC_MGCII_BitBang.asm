\\ EXPERIMENTAL DRIVER FOR MGCII in BIT-BANG MODE

data%=&FCD8     \\ output: MOSI = bit 0, SCK = bit 1
                \\ input:  MISO = bit 7

shifter%=&FCD4  \\ input/output: MOSI/SCK shift register

chipsel%=&FCD9  \\ output: bit 0 = SD_CS1, bit 1 = SD_CS2

\\ MOSI is connected to D0
\\ SCK  is connected to D1
\\ MISO is connected to D7

clockbit%=&02

one_clockhigh%=&FF
one_clocklow%=&FF-clockbit%

MACRO READ_BIT
    STX data%    \\ Take clock (D1) low
    ROL data%    \\ Sample D7 (MISO) into C, and take clock (D1) high
    ROL A        \\ C=1 after this, because A starts off as FF
ENDMACRO

\\ Start of MGCII Specific Code

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
    STX data%
    ROL data%
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
    STA data%
    ORA #clockbit%
    STA data%
NEXT
    RTS
}

\\ RESET DEVICE
.MMC_DEVICE_RESET
    LDA #&FE
    STA chipsel%
    RTS

\\ MMC_SlowClocks is used for SPI Initialization
\\ Where the speed has to be between 100KHz-400KHz
\\   A = corrupted
\\   X = corrupted
\\   Y = number of clocks / 8
.MMC_SlowClocks
{
    TYA
    PHA
    LDA #&00
    LDX #&01
    JSR &FFF4
    CPX #&03
    BCC not_master
    LDA &FE34               ; On the Master, map FCxx to the cartridge port
    ORA #&20
    STA &FE34
.not_master                     ; Y * 8 is the number of clocks
    PLA
    ASL A
    ASL A
    ASL A
    TAY
    LDA #&FF
    STA chipsel%            ; CS1=1
    LDA #&01                ; %00000001
    LDX #&03                ; %00000011
.loop
    STA data%               ; MOSI=1 SCK=0
    STX data%               ; MOSI=1 SCK=1
    DEY
    BNE loop
    STA data%               ; MOSI=1 SCK=0
    LDA #&FE
    STA chipsel%            ; CS1=0
    RTS
}

INCLUDE "MMC_PrinterCommon.asm"
