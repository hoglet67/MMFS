\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** August 2011

\\ ******* HIGH LEVEL MMC CODE ********

go_idle_state=&40
send_op_cond=&41
send_cid=&4A
set_blklen=&50
read_single_block=&51
write_block=&58

	\\ **** Reset MMC Command Sequence ****
	\\ A=cmd, token=&FF
.MMC_SetCommand
	STA cmdseq%+1
	LDA #0
	STA cmdseq%+2
	STA cmdseq%+3
	STA cmdseq%+4
	STA cmdseq%+5
	LDA #&FF
	STA cmdseq%
	STA cmdseq%+6			;\ token
	STA cmdseq%+7
	RTS

	\\ ***** Initialise MMC card *****
	\\ Carry=0 if ok
	\\ Carry=1 if card doesn't repsond at all!
.MMC_INIT
{
	trys%=&32
	attempts%=skipsec%

	JSR SetLEDS
	LDA #0
	STA MMC_STATE

	LDA #trys%
	STA attempts%

	\\ 80 Clocks
.iloop
	LDY #10
	JSR MMC_SlowClocks

	\\ CMD0
	LDA #go_idle_state
	JSR MMC_SetCommand
	LDA #&95
	STA cmdseq%+6			; token (crc7)
	JSR MMC_DoCommand
	AND #&81			; ignore errors
	CMP #1
	BEQ il0
	JMP ifail
.il0
	LDA #&01
	STA CardSort
	LDA #&48
	JSR MMC_SetCommand
	LDA #&01
	STA cmdseq%+4
	LDA #&AA
	STA cmdseq%+5
	LDA #&87
	STA cmdseq%+6
	JSR MMC_DoCommand
	CMP #1
	BEQ isdhc

	LDA #&02
	STA CardSort
.il1
	\\ CMD1
	LDA #send_op_cond
	JSR MMC_SetCommand
	JSR MMC_DoCommand
	CMP #2
	BCC il11
	JMP ifail
.il11
	BIT EscapeFlag			; may hang
	BMI ifail
	CMP #0
	BNE il1
	LDA #&02
	STA CardSort
	JMP iok

.isdhc
	JSR MMC_GetByte
	JSR MMC_GetByte
	JSR MMC_GetByte
	JSR MMC_GetByte
.isdhc2
	LDA #&77
	JSR MMC_SetCommand
	JSR MMC_DoCommand
	LDA #&69
	JSR MMC_SetCommand
	LDA #&40
	STA cmdseq%+2
	JSR MMC_DoCommand
	CMP #&00
	BNE isdhc2
	LDA #&7A
	JSR MMC_SetCommand
	JSR MMC_DoCommand
	CMP #&00
	BNE ifail
	JSR MMC_GetByte
	AND #&40
	PHA
	JSR MMC_GetByte
	JSR MMC_GetByte
	JSR MMC_GetByte
	PLA
	BNE iok
	LDA #2
	STA CardSort

	\\ Set blklen=512
.iok
	LDA #set_blklen
	JSR MMC_SetCommand
	LDA #2
	STA par%+2
	JSR MMC_DoCommand
	BNE blkerr

	\\ All OK!
	LDA #&40
	STA MMC_STATE
	JSR ResetLEDS
	CLC
	RTS

.ifail
	\\ Try again?
	DEC attempts%
	BEQ ifaildone
	JMP iloop

.ifaildone
	\\ Give up!
	JSR ResetLEDS
	SEC
	RTS

	\\ Failed to set block length
.blkerr
IF _MM32_
	JSR mm32_MMC_error
ELSE
	JSR ReportMMCErrS
	EQUB &FF
ENDIF
	EQUS "Set block len error ",0
}

	\\ Read CID and return CRC16 in YA
.MMC_GetCIDCRC
	LDA #send_cid
	JSR MMC_SetCommand
	JSR MMC_StartRead
	LDY #16
	JSR MMC_Clocks
	JSR MMC_GetByte
	TAY
	JMP MMC_GetByte

	\ **** Set-up MMC command sequence ****
.MMC_SetupWrite
	LDA #write_block
	BNE setuprw

.MMC_SetupRead
	LDA #read_single_block
.setuprw
	JSR MMC_SetCommand
	JMP setCommandAddress

	\ **** Begin Read Transaction ****
.MMC_StartRead
	JSR MMC_DoCommand
	BNE errRead
	JMP MMC_WaitForData

.errRead
IF _MM32_
	JSR mm32_MMC_error
	EQUS "Read fault ",0
ELSE
	JSR ReportMMCErrS
	EQUB &C5
	EQUS "MMC Read fault ",0
ENDIF

	\ **** Begin Write Transaction ****
.MMC_StartWrite
	JSR MMC_DoCommand
	BNE errWrite
	JMP MMC_SendingData

.errWrite
IF _MM32_
	JSR mm32_MMC_error
	EQUS "Write fault ",0
ELSE
	JSR ReportMMCErrS
	EQUB &C5
	EQUS "MMC Write fault ",0
ENDIF

	\\ **** Read 2 sectors to "Catalogue" ****
	\\ i.e. pages &E and &F
	\\ (Start sector must be even)
.SetupCatRW
	JSR SetLEDS
	LDA #0
	STA TubeNoTransferIf0
	STA datptr%
	LDA #MP+&0E
	STA datptr%+1
	RTS

	\\ **** Read the Catalogue ****
.MMC_ReadCatalogue
	JSR SetupCatRW
	JSR MMC_SetupRead
	JSR MMC_StartRead
	JSR MMC_Read256
	INC datptr%+1
	JSR MMC_Read256
	JSR MMC_16Clocks		; ignore CRC
	JMP ResetLEDS

	\\ **** Write the Catalgoue ****
.MMC_WriteCatalogue
	JSR SetupCatRW
	JSR MMC_SetupWrite
	JSR MMC_StartWrite
	JSR MMC_Write256
	INC datptr%+1
	JSR MMC_Write256
	JSR MMC_EndWrite
	JMP ResetLEDS


	\\ **** Check if data to/from Tube ****
	\\ Set transfer up if yes.  Exit C=0=Tube Xfr
.MMC_RWBlock_CheckIfToTube
{
	PHA				; 0=read / 1=write

IF _MM32_
	LDA mm32_taddr%+2
	AND mm32_taddr%+3
ELSE
	\ Copy load address to 1072
	LDA MA+&1090
	STA MA+&1072
	LDA MA+&1091
	STA MA+&1073

	LDA MA+&1074
	AND MA+&1075
ENDIF

	ORA TubePresentIf0
	EOR #&FF
	STA TubeNoTransferIf0

	SEC
	BEQ notTube

	JSR TUBE_CLAIM

IF _MM32_
	LDX #LO(mm32_taddr%)
	LDY #HI(mm32_taddr%)
ELSE
	LDX #&72
	LDY #MP+&10
ENDIF

	PLA
	PHA
	JSR TubeCode			; YX=addr,A=0:initrd,A=1:initwr,A=4:strexe
	CLC

.notTube
	PLA
	RTS
}


	\\ **** Read data block to memory ****
	\\ at loc. datptr%
	\\ sec%, seccount% & byteslastsec%
	\\ define block
.MMC_ReadBlock
{
IF _MM32_
	LSR mm32_error%			; Clear error flag
ENDIF
	JSR SetLEDS
	JSR rdblk
	JMP ResetLEDS

IF _MM32_
.rb_err	;C=1
	ROR mm32_error%			; Set bit7
ENDIF
.rb1_exit
	RTS

.rdblk
IF _LARGEFILES
	LDX seccount%
	BNE rb1
	LDA seccount%+1
	BEQ rb1_exit			; nothing to do

.rb1
ELSE
	LDX seccount%
	BEQ rb1_exit			; nothing to do
ENDIF
	LDA #1
	JSR MMC_RWBlock_CheckIfToTube

	LDX seccount%
	ROR sec%
	ROR skipsec%
	BPL rb2
IF _LARGEFILES
	INX
	STX seccount%
	BNE rb2
	INC seccount%+1
.rb2
ELSE
	INX
.rb2
	STX seccount%
ENDIF
	ASL sec%			; sec always even
	JSR MMC_SetupRead

IF _LARGEFILES
	LDA seccount%+1
	BNE rb3
ENDIF
	LDX seccount%
	CPX #3
	BCS rb3				; X>2 = more than 2 sectors

.rb4_loop
	LDA byteslastsec%
	BNE rb5				; don't read whole sector
	CPX #1
	BEQ rb9				; one sector left

.rb3
	BIT skipsec%
	BPL rb6_loop

	\\ read odd sector
	JSR MMC_StartRead
	LDY #0
	STY skipsec%
	JSR MMC_Clocks
	JMP rb7

	\\ read even sectors
.rb6_loop
	JSR MMC_StartRead
	JSR MMC_Read256
	INC datptr%+1

.rb7
	JSR MMC_Read256
	INC datptr%+1
	JSR MMC_16Clocks		; ignore CRC

	\\ increment MMC sector
IF _MM32_
    LDA #&FE
    JSR dec_seccount
    BEQ rb1_exit

	JSR mm32_disk_next_sector
	BCS rb_err				; Error, so exit

	JSR MMC_SetupRead
ELSE
	JSR incCommandAddress
ENDIF

IF _LARGEFILES

IF _MM32_
    LDX seccount%
ELSE
	LDA #&FE
	JSR dec_seccount
	BEQ rb1_exit
ENDIF

	LDA seccount%+1
	BNE rb6_loop
ELSE
	LDX seccount%			; X>=2
	DEX
	DEX
	BEQ rb1_exit
	STX seccount%
ENDIF
	CPX #3
	BCS rb6_loop
	JMP rb4_loop

.rb9
	JSR MMC_StartRead
	JSR MMC_Read256
	JMP rbx4


	\\ A=byteslastsec>0
.rb5
	JSR MMC_StartRead

	BIT skipsec%
	BPL rbx1

	LDY #0				; Skip first MMC sector
	JSR MMC_Clocks
	JMP rbx2

.rbx1
	DEC seccount%			; =1 or =2
	BEQ rbx2

	JSR MMC_Read256
	INC datptr%+1

.rbx2
	JSR MMC_ReadBLS
	TYA				; BLS
	EOR #&FF
	TAY
	INY
	JSR MMC_Clocks

	LDA seccount%
	BNE rbx3

.rbx4
	LDY #0
	JSR MMC_Clocks

.rbx3
	JMP MMC_16Clocks
}

	\\ **** Write data block from memory ****
.wb1_exit
	RTS

.MMC_WriteBlock
{
IF _MM32_
	LSR mm32_error%			; Clear error flag
ENDIF
	JSR SetLEDS
	JSR wrblk
	JMP ResetLEDS

.wrblk
IF _LARGEFILES
	LDX seccount%
	BNE wb1
	LDA seccount%+1
	BEQ wb1_exit			; nothing to do
.wb1
ELSE
	LDX seccount%
	BEQ wb1_exit			; nothing to do!
ENDIF

	LDA #0
	JSR MMC_RWBlock_CheckIfToTube

	LDX seccount%
	ROR sec%
	ROR A
	ASL sec%
	PHA

	JSR MMC_SetupWrite

	PLA
	BPL wb2				; sec even!

	\\ start is odd!
	\\ read mmc sector bytes 0-255
	\\ to buffer, then rewrite it
	\\ with page 1 of the data

	LDA #read_single_block
	STA cmdseq%+1
	JSR MMC_StartRead
	JSR MMC_ReadBuffer
	LDY #0
	JSR MMC_Clocks
	LDY #2
	JSR MMC_Clocks

	LDA #write_block
	STA cmdseq%+1
	JSR MMC_StartWrite
	JSR MMC_WriteBuffer
	JSR MMC_Write256
	JSR MMC_EndWrite
IF _LARGEFILES
	LDA #&FF
	JSR dec_seccount
ELSE
	DEC seccount%
ENDIF
	BEQ wb1_exit			; finished
	INC datptr%+1

	\\ sector+=2
.wb4
IF _MM32_
	JSR mm32_disk_next_sector
	BCS wb_err

	JSR MMC_SetupWrite
ELSE
	JSR incCommandAddress
ENDIF

.wb2
IF _LARGEFILES
	LDA seccount%+1
	BNE wb3
ENDIF
	LDX seccount%
	BEQ wb5				; finished
	DEX
	BNE wb3				; seccount>=2

	\\ 1 sector left
	\\ read mmc sector bytes 256-511
	\\ to buffer, then write last
	\\ page of data, followed by the
	\\ data in the buffer

	LDA #read_single_block
	STA cmdseq%+1
	JSR MMC_StartRead
	LDY #0
	JSR MMC_Clocks
	JSR MMC_ReadBuffer
	LDY #2
	JSR MMC_Clocks

	LDA #write_block
	STA cmdseq%+1
	JSR MMC_StartWrite
	JSR MMC_Write256
	JSR MMC_WriteBuffer
	JMP MMC_EndWrite		; finished

	\\ write whole sectors
	\\ i.e. 2 pages (512 bytes)

.wb3
	JSR MMC_StartWrite
	JSR MMC_Write256
	INC datptr%+1
	JSR MMC_Write256
	INC datptr%+1
	JSR MMC_EndWrite
IF _LARGEFILES
	LDA #&FE
	JSR dec_seccount
ELSE
	DEC seccount%
	DEC seccount%
ENDIF
	BNE wb4

.wb5
	RTS

IF _MM32_
.wb_err	;C=1
	ROR mm32_error%			; Set bit7
	RTS
ENDIF
}

IF _LARGEFILES
\\ Decrement a 16-bit sector count
\\ Call with A=-1 (&FF) to decrement by 1
\\ Call with A=-2 (&FE) to decrement by 2
\\ On exit:
\\ X = value of seccount%
\\ Z flag if seccount%,seccount%+1 zero
.dec_seccount
{
	CLC
	ADC seccount%
	STA seccount%
	TAX
	LDA #&FF
	ADC seccount%+1
	STA seccount%+1
	ORA seccount%
	RTS
}
ENDIF

IF NOT(_MM32_)
	\\ *** Read the disc title to read16str% ***
	\\ *** read16sec% contains the address   ***
	\\ *** of the first disc sector          ***
read16sec%=&B3	; 3 byte sector value
read16str%=MA+&1000

.MMC_ReadDiscTitle
{
	JSR SetLEDS
	LDA #0
	STA TubeNoTransferIf0

	LDX #2
.loop
	LDA read16sec%, X
	STA sec%, X
	DEX
	BPL loop

	JSR MMC_SetupRead
	JSR MMC_StartRead
	LDA #&00			; LO(read16str%)
	STA datptr%
	LDA #MP+&10			; HI(read16str%)
	STA datptr%+1
	LDA #8
	STA byteslastsec%
	JSR MMC_ReadBLS
	LDY #256-8
	JSR MMC_Clocks
	LDA #&08			; LO(read16str%+8)
	STA datptr%			; assume same page
	\LDA #8
	STA byteslastsec%
	JSR MMC_ReadBLS
	LDY #256-8+2
	JSR MMC_Clocks

	JMP ResetLEDS
}
ENDIF

	\\ **** CHECK MMC STATUS ****
	\\ Preserves AXY, and values in BC-C5
.MMC_BEGIN2
	JSR RememberAXY
	JSR MMC_BEGIN1
	JMP MMC_END

	\\ **** BEGIN MMC TRANSACTION ****
	\\ Save values in BC-C5 at 1090-1099
.MMC_BEGIN1
{
IF _MM32_
	LDX #15
ELSE
	LDX #9
ENDIF
.begloop1
	LDA &BC,X
	STA MA+&1090,X
	DEX
	BPL begloop1

	\\ Reset device
	JSR MMC_DEVICE_RESET

	\\ Check if MMC initialised
	\\ If not intialise the card
	BIT MMC_STATE
IF _MM32_
	BVC begX

	RTS

.begX
ELSE
	BVS beg2
ENDIF

	JSR MMC_INIT
	BCS carderr

	JSR MMC_CheckCardID

IF _MM32_
	JMP mm32_init_dos
ELSE

	\\ Check MMC_SECTOR & DRIVE_INDEX initialised
.beg2
	JSR CheckCRC7
	LDA MMC_SECTOR_VALID
	BEQ beg3

	RTS

.beg3
	JSR MMC_Sector_Reset
	JMP MMC_LoadDisks
ENDIF

	\\ Failed to initialise card!
.carderr
	JSR ReportError
	EQUB &FF
	EQUS "Card?",0
}

IF NOT(_MM32_)
	\\ Reset Discs in Drives
.MMC_LoadDisks
{
	LDA #0
	STA &B9
	LDX #3
.loop
	STX &B8
	JSR LoadDriveX
	DEX
	BPL loop
	RTS
}
ENDIF

	\\ If sector 0 set, check it's the same card
	\\ If ok Z=1
.MMC_CheckCardID
{
	JSR CheckCRC7
IF _MM32_
	LDA CLUST_SIZE
ELSE
	LDA MMC_SECTOR_VALID
ENDIF
	BEQ cid_x
	JSR MMC_GetCIDCRC		; YA=CRC16
	CMP MMC_CIDCRC+1
	BNE errCardChanged
	CPY MMC_CIDCRC
	BNE errCardChanged
.cid_x
	RTS

.errCardChanged
	LDA #0
	STA MMC_STATE
	JSR ReportError
	EQUB &FF
	EQUS "Wrong card!",0
}

	\\ **** END MMC TRANSACTION ****
.MMC_END
{
IF _MM32_
	LDX #15
ELSE
	LDX #9
ENDIF
.eloop0
	LDA MA+&1090,X
	STA &BC,X
	DEX
	BPL eloop0
	RTS
}


\\ Translate the sector number into a SPI Command Address
\\ Sector number is in 256 bytes sectors
\\ For SDHC cards this is in blocks (which are also sectors)
\\ For SD cards this needs converting to bytes by multiplying by 512

.setCommandAddress
{
\\ Skip multiply for SDHC cards (cardsort = 01)
	LDA CardSort
	CMP #2
	BNE setCommandAddressSDHC
\\ Convert to bytes by multiplying by 256
	LDA sec%+2
	STA cmdseq%+2
	LDA sec%+1
	STA cmdseq%+3
	LDA sec%
	STA cmdseq%+4
	LDA #0
	STA cmdseq%+5
	RTS


.setCommandAddressSDHC
\\ Convert to 512b sectors by dividing by
	LDA #0
	STA cmdseq%+2
	LDA sec%+2
	LSR A
	STA cmdseq%+3
	LDA sec%+1
	ROR A
	STA cmdseq%+4
	LDA sec%
	ROR A
	STA cmdseq%+5
	RTS
}


IF NOT(_MM32_)
.incCommandAddress
{
	LDA CardSort
	CMP #2
	BNE incCommandAddressSDHC
\\ Add 512 to address (Sector always even)
	INC cmdseq%+4
.incMS
	INC cmdseq%+4
	BNE incDone
	INC cmdseq%+3
	BNE incDone
	INC cmdseq%+2
.incDone
	RTS

\\ Add one to address
.incCommandAddressSDHC
	INC cmdseq%+5
	BEQ incMS
	RTS
}
ENDIF
