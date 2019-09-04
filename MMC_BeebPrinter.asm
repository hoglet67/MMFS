\\ EXPERIMENTAL DRIVER FOR BEEB
\\ WITH MMC CONNECTED TO PRINTER PORT

iora%=_VIA_BASE + &01
ddra%=_VIA_BASE + &03

\\ MOSI is connected to D0
\\ SCK  is connected to D1
\\ MISO is connected to D7

clockbit%=&02
        
one_clocklow%=&FF-clockbit%

\\ Start of Beeb Printer Port Specific Code

\\ Read byte (User Port)
\\ Write FF
.MMC_GetByte
.P1_ReadByte
    LDA #&FF    
    LDX #one_clocklow%
    SEC          \\ Set carry so D0 (MOSI) remains high after ROL
FOR n, 0, 7
    STX iora%    \\ Take clock (D1) low
    ROL iora%    \\ Sample D7 (MISO) into C, and take clock (D1) high
    ROL A        \\ C=1 after this, because A starts off as FF
NEXT
    RTS

\\ This is always entered with A and X with the correct values
.P1_ReadBits7
FOR n, 0, 2
    STX iora%
    ROL iora%
    ROL A
NEXT

 \\ This is always entered with A and X with the correct values
.P1_ReadBits4
FOR n, 0, 3
    STX iora%
    ROL iora%
    ROL A
NEXT
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

\\ More generic code below this point
\\
\\ TODO: This is unchanged from the ElkPlus1 version, so source could be shared


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
