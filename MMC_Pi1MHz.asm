\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

\\ FRED RAM MMC DEVICE

discaccess = &FCA6

; not used
;byteCounter% = MA+&10FF
;lowByteStore% = MA+&10FE

	\\ RESET DEVICE
.MMC_DEVICE_RESET
    ; setup discaccess buffer in FRED.
    ; X = 255 on entry

    ; setup pointer to the command buffer
	LDA #0   : STA discaccess
	LDY #&F0 : STY discaccess+1
    LDX #&FF : STX discaccess+2

    ; setup read
    STA discaccess+3
    STA discaccess+3
    STA discaccess+3
    STA discaccess+3

    ; setup buffer address we only need 512 bytes
    		   STA discaccess+3
    LDY #&E0 : STY discaccess+3
    		 : STX discaccess+3
             : STA discaccess+3

    ; setup start LBA
    STA discaccess+3
    STA discaccess+3
    STA discaccess+3
    STA discaccess+3

    ; setup number of sectors to read or write ( 1 )
    LDY #1
    STY discaccess+3
    STA discaccess+3
    STA discaccess+3
    STA discaccess+3
	; return A=0
	RTS

    \\ only used during init sequence
    \\ mainly for sdhc2 cards and for GetIDCRC so we can just return a constant
.MMC_GetByte
    LDA #&40 ; this fakes sdhc return type
	\\ 16 Clocks is used to skip CRC usually, we don't have CRC so we can ignore
.MMC_16Clocks
	\\ Slow clocks is only used during INIT so we can safely ignore this call as well
.MMC_SlowClocks
	RTS

	\\ MMC Clocks is used to skip bytes in sector reads so we need to handle this correctly
    \\ most of the time Y=0 ie 256 bytes
	\\ entry Z flag is setup correctly based on Y
.MMC_Clocks
{
    BEQ skip_sector
.count
    LDA discaccess+3
	DEY
	BNE count
	RTS			; Don't need to do anything timing wise

.skip_sector
}
	\\ **** Read 256 bytes to buffer ****
	\\ Don't bother, we can simply skip the first page
.MMC_ReadBuffer
	\\ **** Write 256 bytes from buffer ****
	\\ Like reading we can simply ignore the 256 bytes we would need to buffer in JIM RAM
.MMC_WriteBuffer
    LDA #&E1 : STA discaccess+1
    RTS



	\\ *** Send command to MMC ***
	\\ On exit A=result, Z=result=0
.MMC_DoCommand
{
	lda cmdseq%+1
    CMP #read_single_block ; Read command
	BNE notareadcommand

.setup_read_buffer
	; C is set
	LDA #0 ; set up read sector A= 0 ( read sector command)
.setup_buffer
    LDX #0   : STX discaccess
	LDY #&F0 : STY discaccess+1
    LDX #&FF : STX discaccess+2

	STA discaccess+3 ; setup sector commmand
    ; setup pointer to LBA in command buffer
    LDX #8   : STX discaccess

	; Set sector address
	LDX cmdseq%+5 : STX discaccess+3  ; Low byte of command sector address
	LDX cmdseq%+4 : STX discaccess+3
	LDX cmdseq%+3 : STX discaccess+3
	LDX cmdseq%+2 : STX discaccess+3  ; High Byte of sector
	BCC skip
    STY discaccess+4 ; start read sector
.skip

    ; setup pointer to buffer
	LDX #&E0 : STX discaccess+1
    LDA #&0  : STA discaccess
.MMC_send_cid
	; Pretend we are an SD card
.MMC_send_op
.MMC_set_blk_size
.MMC_return
	LDA #0
    ; Potentially do debug output
	; Debug the command sent to the pretend MMC
.dbgMmc
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
	; Done
	RTS

.notareadcommand
    CMP #write_block	; Write command
    BNE setup_commands
	LDA #1
	CLC
    BCC setup_buffer

.setup_commands
    ; These are only used during setup
	CMP #go_idle_state	: BEQ MMC_go_idle 		; Go to Idle command
	CMP #&48			: BEQ MMC_checksdhc		; Check card type
	CMP #send_op_cond	: BEQ MMC_send_op		; Initialise command
	CMP #set_blklen		: BEQ MMC_set_blk_size	; Set block length
	CMP #send_cid		: BEQ MMC_send_cid		; Send CID ; Treat as NOP
	; These command are used to check for sdhc
	CMP #&77 : BEQ MMC_return
	CMP #&69 : BEQ MMC_return
	CMP #&7A : BEQ MMC_return

	LDA #4	; Invalid command response
	BNE dbgMmc

	; Pretend that 95 command returned 1
.MMC_go_idle
	LDA #1
	BNE dbgMmc  ; Potentially do debug output

.MMC_checksdhc
    LDA #0   : STA discaccess
    LDY #&F0 : STY discaccess+1
	LDA #20  : STA discaccess+3 ; SDCARD type command
			 : STY discaccess+4 ; trigger command
	LDA discaccess+4			; get data
	RTS

}

	\\ **** Complete Write Operation *****

.MMC_EndWrite
    LDA #&F0 : STA discaccess+4 ; start write sector
    ; wait for write to complete
	NOP
	\\ Wait for sector to be complete
    \\ for CIDCRC as the command is dropped above it is already complete
.MMC_WaitForData
{
.loop
    LDA discaccess+4
    BMI loop
	    ; setup pointer to buffer
	LDA #&E0 : STA discaccess+1
    LDA #0   : STA discaccess
	RTS
}

	\\ *** Read 256 bytes to datptr ***
.MMC_Read256
{
	LDY TubeNoTransferIf0
	BNE rdlT20

.rdl1
	LDA discaccess+3
	STA (datptr%),Y
	INY
	BNE rdl1
	RTS

.rdlT20
	LDX #0
	BEQ rdlT2
}


	\\ *** Read "byteslastsector" bytes
	\\ to datptr ***
.MMC_ReadBLS
{
    LDX byteslastsec%
    LDY TubeNoTransferIf0
	BNE rdlT1

.rdl2
	LDA discaccess+3
	STA (datptr%),Y
	INY
	DEX
	BNE rdl2

	RTS
}

	\\ TUBE
	\\ 24us delay=48 cycles
	\ \ (7)
.rdlT1
.rdlT2
{
.rdlT3
	LDA discaccess+3 ; 5
	STA TUBE_R3_DATA ; 5
	JSR donothing16cycles ; 6+6+4
	JSR donothing16cycles ; 6+6+4
	NOP           ; 2
	DEX           ; 2
	BNE rdlT3     ; 3
	RTS
}

.donothing16cycles
	NOP
.donothing14cycles
	NOP
.MMC_SendingData
.donothing
{
	RTS
}

	\\ **** Write 256 bytes from dataptr% ****
.MMC_Write256
{
	LDY TubeNoTransferIf0
	BNE wrT1
.wr1
	LDA (datptr%),Y
	STA discaccess+3
	INY
	BNE wr1
    RTS

	\.wrT1	; To tube 24us delay
.wrT1
	LDY #0
.wrT2
	LDA TUBE_R3_DATA
	STA discaccess+3
	JSR donothing16cycles
	JSR donothing16cycles
	NOP
	INY
	BNE wrT2
	RTS
}
