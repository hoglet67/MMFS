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

\\ More generic code below tis point


\\ RESET DEVICE
.MMC_DEVICE_RESET
    RTS

\\ *** Send &FF to MMC Y times ***
\\ Y=0=256
.MMC_16Clocks
    LDY #2
.MMC_Clocks
{
.loop
    JSR P1_ReadByte  ; Writes &FF
    DEY
    BNE loop
    RTS              ; A=SR, X=one%, Y=0
}


\\ *** Send command to MMC ***
\\ On exit A=result, Z=result=0
.MMC_DoCommand
    LDX #0

{
    LDY #7
.loop1
    LDA cmdseq%,X
    JSR P1_WriteByte
    INX
    DEY
    BNE loop1
    JSR P1_WaitResp
IF _DEBUG_MMC
    JSR P1_ReadBits7
    PHP
    PHA
    LDY #0
.loop2
    LDA cmdseq%,Y
    JSR PrintHex
    INY
    CPY #7
    BNE loop2
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
    JMP P1_ReadBits7
ENDIF
}

\\ *** Wait for data token ***
.MMC_WaitForData
{
.loop
    JSR P1_ReadByte
    CMP #&FE
    BNE loop
    RTS
}

\\ *** Read 256 bytes to datptr ***
.MMC_Read256
    LDX #0
    BEQ MMC_ReadX

    \\ *** Read "byteslastsector" bytes
    \\ to datptr ***
.MMC_ReadBLS
    LDX byteslastsec%

.MMC_ReadX
    LDY #0
    LDA TubeNoTransferIf0
    BNE MMC_ReadToTube

.MMC_ReadToMemory
    TXA
    PHA
    JSR P1_ReadByte
    STA (datptr%),Y
    PLA
    TAX
    INY
    DEX
    BNE MMC_ReadToMemory
    RTS

.MMC_ReadToTube
    TXA
    PHA
    JSR P1_ReadByte
    STA TUBE_R3_DATA
    PLA
    TAX
    INY
    DEX
    BNE MMC_ReadToTube
    RTS


\\ **** Read 256 bytes to buffer ****
.MMC_ReadBuffer
{
    LDY #&FF
    STY CurrentCat
    INY
.loop
    JSR P1_ReadByte
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
    JMP P1_WriteByte
}

\\ **** Complete Write Operation *****
.MMC_EndWrite
{
    LDY #2

    JSR MMC_Clocks
    JSR P1_WaitResp
    JSR P1_ReadBits4
    TAY
    AND #&1F
    CMP #5
    BNE errWrite2

.loop
    JSR P1_ReadByte
    CMP #&FF
    BNE loop
    RTS
}

\\ **** Write 256 bytes from dataptr% ****
.MMC_Write256
{
    LDY TubeNoTransferIf0
    BNE tube
.loop1
    LDA (datptr%),Y
    JSR P1_WriteByte
    INY
    BNE loop1
    RTS
.tube
    LDY #0
.loop2
    LDA TUBE_R3_DATA
    JSR P1_WriteByte
    INY
    BNE loop2
    RTS
}

\\ **** Write 256 bytes from buffer ****
.MMC_WriteBuffer
{
    LDY #0
.loop
    LDA buf%,Y
    JSR P1_WriteByte
    INY
    BNE loop
    RTS
}
