\\** MM32.asm by Martin Mather 11th May 2019

LBA_FAT=VID2		;16bit
LBA_DATA=VID2+2		;16bit
CLUST_SIZE=VID2+4	;8bit
CHAIN_INDEX=VID2+5	;3*24bit

ATTRIB_X=&C2	;8bit
FRAG_X=&CE		;8bit
CLUST_X=&C8		;24bit
SECT_X=&CB		;8bit

CLUST_Y=MA+&10A0	;32bit

mm32_sector%=MA+&10A4	;8bit
mm32_track%=MA+&10A5	;8bit
mm32_error%=MA+&10A6	;8bit
mm32_taddr%=MA+&10AA	;32bit

mm32_star = 1		;String markers
mm32_hash = 2

\B0-B2
mm32_zptr%=&B0		; 16 bit
mm32_flags%=&B2		; 8 bit : bit 7 = directories only

\B5-B9
mm32_logging%=&B7	; 8 bit : bit 7 = cataloguing
mm32_cluster%=&B7	; 24 bit
mm32_attrib%=&B9	; 8 bit

mm32_str%=MA+&1000		; 1000 - 103F (64 bytes)
mm32_drvtbl%=MA+&11C0	; 11C0 - 11DF (32 bytes)

;; FOR INFO: sec%=&BE

IF _MM32_DEBUG
\ Temp command
.mm32_cmd_dbug
{
	;JSR mm32_param_count
	;bcc l1
	;JSR mm32_param_drive
	;ror a
	;lda #0
	;ror a
	;sta _XX32FLAG	;temp flag
	;jsr PrintHex
	;jmp OSNEWL

.l1	jsr PrintString
	EQUS "FAT-LBA  "
	nop
	ldy #LBA_FAT-VID2
	jsr phex2
	jsr PrintString
	EQUS "DATA-LBA "
	nop
	ldy #LBA_DATA-VID2
	jsr phex2
	jsr PrintString
	EQUS "CLUST SZ "
	nop
	ldy #CLUST_SIZE-VID2
	jsr phex1

	ldx #0
.loop
	txa
	jsr PrintHex
	lda #':'
	jsr OSWRCH
	jsr mm32_pclust
	jsr OSNEWL
	inx
	cpx #3
	bne loop
	rts

.phex2
	lda VID2+1,Y
	jsr PrintHex
.phex1
	lda VID2,Y
	jsr PrintHex
	jmp OSNEWL
}

\ DEBUG: Print CHAIN_INDEX entry.
.mm32_pclust
{
	lda CHAIN_INDEX+6,X
	jsr PrintHex
	lda CHAIN_INDEX+3,X
	jsr PrintHex
	lda CHAIN_INDEX,X
	jsr PrintHex
	RTS
}
ENDIF


\\ Convert block size to 256 bytes.
\\ Exit: C=1 if overflow
.mm32_lba_to_256
	ASL sec%	; x 2
	ROL sec%+1
	ROL sec%+2
	RTS


\\ Read 512 byte sector into catalogue area
.mm32_readblock
	LDA #&FF
	STA CurrentCat
	JMP MMC_ReadCatalogue


.mm32_init_dos
{
	\ Byte order must match VID
	lba_fat% = &C7	;16 bit
	lba_data% = &C9	;16 bit
	clust_size% = &CB	;8 bit

	JSR CheckCRC7
	LDA CLUST_SIZE		; If >0 already done
	BEQ init

	RTS

.init
{
	JSR read_VBR
	BCC faterr		; Couldn't read VBR

	\\ BIOS Parameter Block (BPB)

	LDA cat%+&0C
	CMP #2
	BNE faterr		; Sector size <> 512 bytes

	; A=2
	EOR cat%+&2C
	ORA cat%+&26
	BNE faterr		; Root Dir Cluster <> 2 or FAT size > 65535

	LDA cat%+&25
	LSR A
	BEQ faterr		; FAT size < 512 so can't be FAT32

	\\ If no error then:
	\\ 1) Sector Size = 512 bytes
	\\ 2) Size of FAT >= 512 and < 65536
	\\ 3) Root Directory starts in cluster 2
	\\ (We've assumed some bytes are zero.)

	\\ Remember the cluster size (in sectors)
	LDA cat%+&0D
	STA clust_size%

	\\ Now calc LBA of FAT area

	LDA lba_fat%
	LDY lba_fat%+1

	CLC
	ADC cat%+&0E	; LBA of VBR + Reserved sectors
	TAX
	TYA
	ADC cat%+&0F
	BCS faterr		; Overflow

	STX lba_fat%
	STA lba_fat%+1

	\\ Now add size of FAT area to get LBA of data area

	;C=0
.loop1
	TAY
	TXA
	ADC cat%+&24
	TAX
	TYA
	ADC cat%+&25
	BCS faterr		; Overflow

	DEC cat%+&10
	BNE loop1

	TAY

	\\ Subtract 2 * Cluster size

	ASL cat%+&0D
	TXA
	SEC
	SBC cat%+&0D
	STA lba_data%
	TYA
	SBC #0
	STA lba_data%+1

	\\ Now we have populated:
	\\ clust_size%, lba_fat% and lba_data%
	\\ and we know the root directory is at cluster 2

	\ No errors so copy to VID
	JSR CheckCRC7

	JSR MMC_GetCIDCRC	; Remember card id
	STA MMC_CIDCRC+1
	STY MMC_CIDCRC

	LDY #5

.loop2
	LDA lba_fat%-1,Y
	STA LBA_FAT-1,Y
	DEY
	BNE loop2

	TYA ; A=0
	LDY #8

	\ Reset CHAIN_INDEX
.loop3
	STA CHAIN_INDEX,Y
	DEY
	BPL loop3

	JMP ResetCRC7

.faterr
	JSR ReportError
	EQUB &FF
	EQUS "Card format?",0
}

\\ Read VBR, on exit C=1 if successful,
\\ and lba_fat% contains LBA of FAT.
.read_VBR
{
	\\ Read sector 0 (MBR)
	LDA #0
	STA lba_fat%
	STA lba_fat%+1

	STA sec%
	STA sec%+1
	STA sec%+2

	JSR mm32_readblock	; MBR?
	JSR isfat
	BCC ifx			; FAT not recognised

\\ Boot sector signature 0x55 0xAA found

\\ Test for presence of FAT Partition Boot Sector
\\ 0x000 = 0xEB xx 0x90 is a good indicator
\\ If this is not found, then assume sector 0 is an MBR

    LDA cat%
	CMP #&EB
	BNE mbr

	LDA cat%+2
	CMP #&90
	BNE mbr

	LDA cat%+&C
	CMP #&02
	BEQ ifx

.mbr
	\\ VBR sec = cat!&1C6 * 2
	\\ Must be less than 2^16
	LDA cat%+&1C6
	STA lba_fat%
	STA sec%
	LDA cat%+&1C7
	STA lba_fat%+1
	STA sec%+1

	LDA cat%+&1C8
	ORA cat%+&1C9
	BNE bad

	; A=0
	STA sec%+2

	JSR mm32_lba_to_256
	JSR mm32_readblock	; VBR

	\\ FAT signature word?
.isfat
	LDA cat%+&1FE
	CMP #&55
	BNE bad

	LDA cat%+&1FF
	CMP #&AA
	BNE bad

.ifx
	RTS

.bad
	CLC
	RTS
}
}


\\ Initialise CLUST_X, SECT_X and ATTRIB_X
\\ Copies cluster from chain index.
\\ On entry Y=index (0=Drive 0, 1=Drive 1 or 2=Current Dir)
\\ If Y<2 then V=1 if drive empty
.mm32_SECT_X_First
{
	LDA CHAIN_INDEX,Y
	STA CLUST_X
	LDA CHAIN_INDEX+3,Y
	STA CLUST_X+1
	LDA CHAIN_INDEX+6,Y
	STA ATTRIB_X		; Save flags
	AND #&3F
	STA CLUST_X+2
	ORA CLUST_X
	ORA CLUST_X+1
	BNE l2

	CPY #2
	BEQ l1				; If current directory

	\ If 'root' in drive, should be read only.
	BIT ATTRIB_X
	BPL l3				; If read only flag clear

	\ If zero, assume root directory at cluster 2
.l1	LDA #2
	STA CLUST_X

.l2	LDA CLUST_SIZE		; A > 0
	STA SECT_X			; Reset sector counter

	LDA #0
	STA FRAG_X

	CLV
	RTS

.l3	BIT l4				; Set V

.l4	RTS
}


\\ Get next sector in cluster chain
\\ On exit:
\\ If C=1 then EOC or overflow error
\\ else CLUST_X + SECT_X point to sector.
\\ X & Y preserved
.mm32_SECT_X_Next
{
{
	DEC SECT_X
	BEQ l1			; If zero go to next cluster

	CLC
	RTS

.l1	JSR next_cluster
	LDA CLUST_SIZE
	STA SECT_X		; Reset sector counter
	RTS
}

\ Current cluster in CLUST_X (22 bit)
\ On exit, if C=1 then End of Chain (EOC) else CLUST_X is next cluster
\ X & Y preserved.
\ Uses one byte in zero page.
.next_cluster
{
	JSR RememberXYonly

	DEC FRAG_X
	BPL l1

	LDA TubeNoTransferIf0
	PHA
	LDA datptr%
	PHA
	LDA datptr%+1
	PHA
	LDA byteslastsec%
	PHA

	JSR read_fat

	PLA
	STA byteslastsec%
	PLA
	STA datptr%+1
	PLA
	STA datptr%
	PLA
	STA TubeNoTransferIf0

.l1	CLC

	LDA FRAG_X
	BEQ l4

	INC CLUST_X
	BNE l3

	INC CLUST_X+1
	BNE l3

	INC CLUST_X+2

.l2	LDA CLUST_X+2
	AND #&C0
	BEQ l3

	SEC

.l3	RTS

.l4	LDX #2

.loop1
	LDA CLUST_Y,X
	STA CLUST_X,X
	DEX
	BPL loop1
	BMI l2
}

\ Read FAT table
\ On exit: FRAG_X is the size of the fragment (max = 127)
\ + CLUST_Y (first cluster of next fragment)
.read_fat
{
	n = SECT_X ; 8bit

	; Calc FAT sector address

	LDA CLUST_X
	ASL A
	STA n

	LDA CLUST_X+1
	ROL A
	TAY
	LDA CLUST_X+2
	ROL A
	TAX
	;CLC
	TYA
	ADC LBA_FAT
	STA sec%
	TXA
	ADC LBA_FAT+1
	STA sec%+1
	LDA #0
	STA TubeNoTransferIf0
	STA sec%+2
	JSR mm32_lba_to_256

	; Read FAT sector

	JSR SetLEDS
	JSR MMC_SetupRead
	JSR MMC_StartRead

	LDA #LO(CLUST_Y)
	STA datptr%
	LDA #HI(CLUST_Y)
	STA datptr%+1

	LDA n
	JSR clocks

.loop1
	INC FRAG_X

	LDA #4
	STA byteslastsec%
	JSR MMC_ReadBLS

	INC n
	INC n
	BEQ l4		; If end of sector

	SEC

	LDA CLUST_Y+2
	AND #&C0
	ORA CLUST_Y+3
	BNE l2		; If CLUST_Y >= &400000, then EOC

	;SEC
	LDA CLUST_Y
	SBC CLUST_X
	TAY
	LDA CLUST_Y+1
	SBC CLUST_X+1
	BNE l3

	LDA CLUST_Y+2
	SBC CLUST_X+2
	BNE l3

	DEY
	;Y=CLUST_Y-CLUST_X-1
	CPY FRAG_X
	BEQ loop1
	BNE l3

	; EOC , set bit 23
.l2	ROR CLUST_Y+2

.l3	SEC
	LDA #0
	SBC n
	JSR clocks

.l4	JSR MMC_16Clocks
	JMP ResetLEDS
}

\ Do A*2 clocks (note A must be even)
.clocks
{
	ASL A
	PHA
	BCC l1

	LDY #0
	JSR MMC_Clocks

.l1	PLA
	BEQ l2

	TAY
	JMP MMC_Clocks

.l2 RTS
}
}

\\ Calculate address of sector SECT_X in cluster CLUST_X.
\\ Exit: C=1 if error (overflow)
.mm32_SECT_X_Address
{
	LDX #2

.loop1
	LDA CLUST_X,X	; sec% = CLUST_X
	STA sec%,X
	DEX
	BPL loop1

	LDA CLUST_SIZE	; sec% *= CLUST_SIZE

.loop2
	LSR A
	BCS l3

	ASL sec%
	ROL sec%+1
	ROL sec%+2
	BCC loop2

	RTS				; Exit with C=1

.l3	CLC				; sec% += LBA_DATA
	LDA sec%
	ADC LBA_DATA
	STA sec%
	LDA sec%+1
	ADC LBA_DATA+1
	STA sec%+1
	BCC l4

	INC sec%+2

.l4	SEC				; sec% += (CLUST_SIZE - SECT_X)
	LDA CLUST_SIZE
	SBC SECT_X
	CLC
	ADC sec%
	STA sec%
	BCC l5

	INC sec%+1
	BNE l5

	INC sec%+2

.l5	JMP mm32_lba_to_256
}


\\ Scan the current directory
\\ On exit: C=0 if file/dir found
.mm32_Scan_Dir
{
	is_dir%=&B8

{
	LDY #2
	JSR mm32_SECT_X_First

.loop1
	JSR mm32_SECT_X_Address
	BCS exit	; If overflow error (C=1)

	JSR mm32_readblock

	JSR scan_block
	BCC exit	; File/Dir found
	BEQ exit	; C=1, End of dir

	JSR mm32_SECT_X_Next
	BCC loop1	; If not EOC and no error

	;C=1

.exit
	RTS
}

\\ Scan sector of directory for valid entries.
\\ On exit A=0 & Z=1 if end of dir found
.scan_block
{
	z = mm32_zptr%

	LDA #0
	STA &AB		; Use &AB as a writeback flag
	STA z
	LDA #HI(cat%)

.loop1
	STA z+1

.loop2
	LDY #0
	LDA (z),Y	; First chr of name
	BEQ exit	; Z=1, End of directory
	BMI skip	; If A>=128

	CMP #&21
	BCC skip	; If A<=32

	\Is cluster number too large? (i.e. >20 bits)
	LDY #21
	LDA (z),Y
	AND #&0F
	BNE skip

	DEY
	LDA (z),Y
	AND #&F0
	BNE skip

	LDY #11
	LDA (z),Y	; A = Attributes
	AND #&0E
	BNE skip	; If long filename/hidden/system/volume name

	LDA (z),Y
	LSR A		; C=Read only
	AND #&08
	STA is_dir%
	BEQ l1		; If not dir

	LDA #&80	; If directory, always read only
	;LDA (z),Y
	;LSR A
	;ROR A
	BNE l2		; Always

.l1	BIT mm32_flags%
	BMI skip	; Only looking at directories

	; A=0
	ROR A		; Bit 7 = Read only
	STA mm32_attrib%

	JSR is_XSD
	ORA mm32_attrib%

.l2	STA mm32_attrib%
	;jsr PrintHex

	JSR copy_name
	JSR dir_match
	BCS skip	; Didn't match

	LDA &AA		; Command: 0 NOP, 1 Unlock, 2 lock
	BEQ s1		; Cmd 0 -> Do nothing
	CMP #2		; Cmd 2 -> Lock file
	BEQ lock
	JSR unlock_fat_file
	JMP s1
.lock
	JSR lock_fat_file
.s1
	BIT mm32_logging%
	BMI logging

	;jsr print_dir_entry
	;lda #&20
	;jsr PrintChrA

	CLC	;C=0 = FOUND
	RTS

.logging
	JSR print_dir_entry

.skip
	JSR CheckESCAPE

	CLC
	LDA z
	ADC #32
	STA z
	BCC loop2

	LDA z+1
	ADC #0
	CMP #HI(cat%)+1
	BEQ loop1

	;Z=0

.exit
	PHP			; Preserve flags for return
	PHA			; Preserve A for return
	LDA &AB		; If &AB is &FF then writeback
	CMP #&FF
	BNE nowrite
	JSR MMC_WriteCatalogue
.nowrite
    LDY #&00	; Clear writeback flag
    STY &AB
	PLA			; Get A back
	PLP			; Get flags back!
    SEC
    RTS
}

\\ Set read only bit
.lock_fat_file
{
	z = mm32_zptr%
	LDA #&80	; Set 'L' flag in logging printout
	STA mm32_attrib%
	LDY #11
	LDA (z),Y	; Attribs
	ORA #&01	; Set r/o bit
	STA (z),Y
	LDA #&FF	; Writeback flag
	STA &AB
	RTS
}

\\ Clear read only bit
.unlock_fat_file
{
	z = mm32_zptr%
	LDA #&00	; Clear 'L' flag in logging printout
	STA mm32_attrib%
	LDY #11
	LDA (z),Y	; Attribs
	AND #&FE	; Clear r/o bit
	STA (z),Y
	LDA #&FF	; Writeback flag
	STA &AB
	RTS
}

\\ Copy name from directory
.copy_name
{
	z = mm32_zptr%
	str = mm32_str%

	LDX #1
	LDY #0

.loop1
	LDA (z),Y
	CMP #&21
	BCC l1

	STA str,X
	INX

.l1	INY
	CPY #8
	BNE l2

.l11
	LDA #'.'
	STA str,X
	INX

.l2	CPY #11
	BNE loop1

.l3	LDA #mm32_hash
	STA str
	STA str,X
	LDA #0
	STA str+1,X
	RTS
}

\\ Print directory entry
\\ Note: Cannot be spooled
.print_dir_entry
{
	;z = mm32_zptr%
	str = mm32_str%

	LDX #3
	JSR lspc

	LDX #&20
	LDA is_dir%
	BEQ	l99				; If not directory

	LDX #'/'

.l99
	TXA

	LDY #1
	LDX #14

.loop1
	JSR PrintChrA		; Print '/' or space

	LDA str,Y
	CMP #&20
	BCC lx

	CMP #'.'
	BEQ l1

.l0	DEX
	INY
	BNE loop1			; Always

.l1	LDA str+1,Y			; Skip trailing dot
	CMP #&20

	LDA #'.'
	BCS l0

.lx	JSR lspc

	LDA #'L'
	BIT mm32_attrib%	; Flags
	BMI l00				; If read only
	LDA #' '

.l00
	JSR PrintChrA		; Print 'L' or space
	JMP PrintSpace

	; Pad with X spaces
.lspc
	JSR PrintSpace
	DEX
	BNE lspc

	RTS		; A=&20
}

\\ Is it a SSD or DSD file?
\\ Result retured in A
\\ On exit: Bit 7 set if not disk image, bit 6 set if double sided image
.is_XSD
{
	z = mm32_zptr%

	ssd=&00
	dsd=&20

	LDY #11
	LDA (z),Y
	AND #&10
	BNE l2				; It's a directory

IF TRUE
	LDX #&FF

.loop1
	LDY #8
	CLC

.loop2
	INX
	LDA ext_table,X
	BMI x2				; end of record
	BEQ l2				; end of table (no matches found)

	EOR (z),Y
	BEQ x1

	SEC

.x1	INY
	BNE loop2			; always

.x2	BCS loop1			; not a match

	ASL A
	RTS
ELSE
	DEY					; Last chr of extension
	LDA (z),Y
	CMP #'D'
	BNE l2

	DEY
	LDA (z),Y
	CMP #'S'
	BNE l2

	DEY
	LDA (z),Y
	CMP #'S'
	CLC
	BEQ l1

	CMP #'D'
	BNE l2

.l1	ROR A
	LSR A				; Bit 7 clear if SSD or DSD, Bit 6 set if DSD
	AND #&C0
	RTS
ENDIF

.l2	LDA #&80			; Bit 7 set
	RTS

IF TRUE
.ext_table
	EQUB "SSD", &80+ssd
	EQUB "DSD", &80+dsd
	BRK	;End of table
ENDIF
}

\\ Match name with command line parameter
.dir_match
{
	str = mm32_str%
	ix = &BA
	iy = &BB

	; str offsets
	strX = 16
	strY = 0

	LDX #strX-1
	LDY #strY

.l00
	INX
	STX ix

.l0
	STY iy

.l1	LDA str,X
	BEQ ex1				; If successful

	CMP #mm32_star
	BEQ l00				; So far so good

	;SEC				; A must be >= star, so C=1
	LDA str,Y
	BEQ ex2				; If failed

	CMP str,X
	BNE l2				; Try again

	INY
	INX
	BNE l1				; Always

.l2	LDX ix
	LDY iy
	INY
	BNE l0				; Always

.ex1
	CLC
.ex2
	RTS
}
}


\\ Count the number of parameters entered by user.
\\ On entry: A contains two flags:
\\ If flag7=0 then the permissable range is 0 to 1, else it's 1 to 2.
\\ If flag0=0 then the lower limit is allowed.
\\ On exit: Y preserved, C=result
.mm32_param_count
	LDA #&00

.mm32_param_count_a
{
	PHA
	TYA
	PHA

	LDX #0

.loop1
	JSR GSINIT_A
	BEQ l1

	INX

.loop2
	JSR GSREAD_A
	BCC loop2
	BCS loop1

.l1	PLA
	TAY

	PLA
	BPL l2		; If flag7=0 : range 0 to 1

	; range 1 to 2
	DEX
	BMI err		; If count=0

.l2	CPX #1
	BEQ l3		; C=1 ; If (flag7=0 and count=1) or (flag7=1 and count=2)
	BCS err		; If (flag7=0 and count>1) or (flag7=1 and count>2)

	; If (flag7=0 and count=0) or (flag7=1 and count=1)
	ROR A
	BCS err		; Not allowed

	;C=0

.l3	RTS

.err
	JMP errSYNTAX
}


\\ Read the drive parameter or, if not given, set to default
\\ On entry: C=0 if default to be used
\\ On exit : A=Physical drive number
.mm32_param_drive
{
	BCC l1				; No drive parameter, use default.

	JSR GSINIT_A
	JSR Param_DriveNo_BadDrive
	JSR GSREAD_A
	LDA CurrentDrv
	BCS l2

	; Should have been end of gs string.
	JMP errBADDRIVE

.l1	LDA DEFAULT_DRIVE

.l2	AND #&01			; Only interest in "physical drive" (0 or 1).
	STA CurrentDrv
	RTS
}


\\ Read filename parameter
\\ On entry: C=1 if cataloguing (i.e. parameter is a filter)
\\ On exit : C=1 if error (e.g. string too long), else Z=1 if zero length string.
.mm32_param_filename
{
	str = mm32_str%+16

	MaxLen=16

	\ Flags
	dir%=mm32_flags%
	star%=&B3
	dot%=&B4
	notdot%=&B5

	ROR mm32_logging%	; Cataloguing flag (bit 7) = C.

	LDX #1
	STX dir%
	STX star%
	STX dot%
	STX notdot%

	;CLC
	JSR GSINIT_A
	BEQ exloop			; Null string

.loop1
	JSR GSREAD_A
	BCS exloop			; End of string

	CMP #'/'
	BNE l0

	;C=1
	ROR dir%			; Set Dir flag
	BNE loop1			; Always

.loop2
	JSR GSREAD_A
	BCS exloop			; End of string

.l0	BIT mm32_logging%
	BPL l1				; If not catloguing no wildcards

	CMP #'*'
	BNE l1

	BIT star%
	BMI loop2			; Last chr was also '*'

	;C=1
	ROR star%

	LDA #mm32_star
	BNE l3				; Always

.l1	LSR star%

	CMP #'.'
	BNE l2				; If not dot

	BIT notdot%			; No other chrs yet!
	BPL l3

	;C=1
	ROR dot%			; Extension dot found
	BMI l3				; Always

.l2	SEC
	ROR notdot%

	\ UCASE
	CMP #'a'
	BCC l3

	CMP #'z'+1
	BCS l3

	EOR #&20

.l3	STA str,X
	INX
	CPX #MaxLen
	BNE loop2			; Else string too long

	;SEC				; C=1=Error
	RTS

.exloop
	DEX
	BEQ lx2				; If null string

	BIT mm32_logging%
	BMI lx1				; If cataloguing

	BIT dot%
	BMI lx1				; If extension dot found

	LDA #'.'			; Add extension dot
	STA str+1,X
	INX

.lx1
	LDA #mm32_hash		; Add end markers
	STA str
	INX
	STA str,X
	INX

.lx2
	LDA #0
	STA str,X

	LDA str				; Z=null string
	CLC					; C=0=OK
	RTS
}


IF FALSE
\ DEBUG: Print the string created by mm32_param_filename.
.mm32_prtstr16

{
	lda #'"'
	jsr OSWRCH
	ldx #0
.loop
	lda mm32_str%+16,x
	beq yy
	cmp #&20
	bcc xx
	jsr OSWRCH
.xx	inx
	bne loop
.yy	lda #'"'
	jsr OSWRCH
	lda #' '
	jmp OSWRCH
}
ENDIF


\\ *DCAT (<filter>)
.mm32_cmd_dcat
{

	JSR mm32_param_count

	SEC					;We are cataloguing.
	JSR mm32_param_filename
	LDA #0
	STA &AA				; Scan_Dir 'normal' mode
	JSR mm32_Scan_Dir

	LDA #&86
	JSR OSBYTE			; get cursor pos
	CPX #0
	BEQ dcEven

	JSR PrintNewLine

.dcEven
	RTS
}


\\ *DBOOT (<dosname>)
\\ If dos name omitted, boot 'BOOT.'
.mm32_cmd_dboot
{
	LDA #0
	STA CurrentDrv

	JSR mm32_param_count
	BCS l2				; If dos name given

	LDX #0
.loop
	LDA bootdisk,X
	STA mm32_str%+16,X
	BEQ l1

	INX
	BNE loop

.l1	STA mm32_logging%
	STA mm32_flags%
	;CLC

.l2	LDA #$00	; Looking for a file
	JSR mm32_chain_open2
	BCC l3		; If file not found C=1, just return
	RTS

.l3	LDA #0
	JMP initMMFS

.bootdisk
	EQUS mm32_hash, "BOOT.", mm32_hash, 0
}


\\ *DIN (<drive>) <dosname>
.mm32_cmd_din
{
	LDA #&80
	JSR mm32_param_count_a
	JSR mm32_param_drive

	;jsr mm32_prtcurdrv

	LDA #$00	; Looking for a file
	JMP mm32_chain_open
}

\\ Automatically load BOOT.{SSD,DSD} into drive 0 at boot
.mm32_cmd_autoload
{
	LDX #0
	STX CurrentDrv
.loop
	LDA bootdisk,X
	STA mm32_str%+16,X
	BEQ l1
	INX
	BNE loop

.l1	STA mm32_logging%
	STA mm32_flags%

	CLC
	LDA #$02	; Looking for a file, autoload mode
	JMP mm32_chain_open2
	RTS
.bootdisk
	EQUS mm32_hash, "BOOT.", mm32_hash, 0
}

;.mm32_prtcurdrv
;	lda CurrentDrv
;	jmp PrintHex

\\ Add extension .SSD to filename at mm32_str+16
.mm32_add_ssd_ext
{
	str = mm32_str%+16
	LDY #1			; Skip initial mm32_hash
.l0	LDA str,Y
	CMP #'.'		; Filenames guaranteed to have '.'
	BEQ s1
	INY
	JMP l0
.s1	INY
	LDA #'S'
	STA str,Y
	STA str+1,Y
	LDA #'D'
	STA str+2,Y
	LDA #mm32_hash
	STA str+3,Y
	LDA #0
	STA str+4,Y
	RTS
}

\\ Change extension .SSD to .DSD for filename at mm32_str+16
.mm32_change_ext_dsd
{
	str = mm32_str%+16
	LDY #1			; Skip initial mm32_hash
.l0	LDA str,Y
	CMP #'.'
	BEQ s1
	INY
	JMP l0
.s1	LDA #'D'		; Change 'S' after period to 'D'
	STA str+1,Y
	RTS
}

\\ Update mm32_dsktbl when a file is mounted
.mm32_upd_dsktbl
{
	str = mm32_str%+16
	tbl = mm32_drvtbl%
	MaxLen = 16
	LDX #0
	LDA CurrentDrv
	ASL A
	ASL A
	ASL A
	ASL A
	TAY
.l0 LDA str,X
	STA tbl,Y
	INX
    INY
	CPX #MaxLen
	BNE l0
	RTS
}

\\ *DDIR (<dosname>)
\\ Change current directory.
\\ If directory="", go to root directory.
.mm32_cmd_ddir
{
	JSR mm32_param_count

	LDA #2
	STA CurrentDrv

	LDA #$01	; Looking for a directory
	\JMP mm32_chain_open
}


\\ Search current directory
\\ If found, set CHAIN_INDEX(X) to first cluster of chain,
\\ else report FILE NOT FOUND.
\\ On entry: CurrentDrv = chain index, If C=0, skip reading filename
\\           Flags in A:
\\           Bit 0 (LSB): 0 looking for file, 1 looking for dir
\\           Bit 1: 0 normal, 1 'autoload mode' - don't report errors!
\\ On exit: C=1 if file was not found
.mm32_chain_open
	SEC

.mm32_chain_open2
{
	z = mm32_zptr%
	str = mm32_str%+16
	is_dir%=&B8

	PHA						; Store the flags for later
	BCC l0

	CLC						; We are not cataloguing.
	JSR mm32_param_filename	; Read filename parameter.
	BCS notfound			; If error when reading parameter.
	BEQ	zerolen				; If string zero length.

.l0	LDA CurrentDrv
	CMP #2					; If C=1, only scan for directories.
	ROR mm32_flags%

	LDA #0
	STA &AA					; Scan_Dir 'normal' mode
	JSR mm32_Scan_Dir
	BCC found
	PLA						; Recover flags
	PHA						; Stash them for l8r
	AND #$01				; File or directory
	BNE notfound			; If directory don't try appending suffixes
	JSR mm32_add_ssd_ext
	JSR mm32_Scan_Dir
	BCC found
	JSR mm32_change_ext_dsd
	JSR mm32_Scan_Dir
	BCC found

.notfound
	PLA						; Recover flags
	AND #$02				; See if we are in autoload mode
	BEQ notautoload
	SEC
	RTS 					; On cold start, simply return
.notautoload
	JMP err_FILENOTFOUND

.found
	PLA						; Recover flags
	PHA						; Stash them for l8r
	AND #$01				; File or directory?
	BEQ file
	PLA						; Fix up stack
	LDA is_dir%
	BNE okay
	RTS
.file
	PLA						; Recover flags
	LDX is_dir%
	BEQ okay
	AND #$02				; Autoload mode?
	BEQ notautoload2
	SEC
	RTS
.notautoload2
	JSR ReportError
	EQUB &D6
	EQUB "Is directory",0
	NOP
	RTS

.zerolen
	\ No parameter given by user
	PLA						; Fix up stack before exit
	LDA mm32_flags%
	AND #&80				; If directory marker found in parameter, set read only flag,
	TAY						; else drive will be empty.
	JMP mm32_clear_cluster_index

.okay
	\ File/Directory Found
	JSR mm32_upd_dsktbl
	LDY #26					; Copy cluster number from directory
	LDA (z),Y
	STA mm32_cluster%
	INY
	LDA (z),Y
	STA mm32_cluster%+1
	LDY #20
	LDA (z),Y
	ORA mm32_attrib%
	STA mm32_cluster%+2

	LDA CurrentDrv
	CMP #2
	BEQ l1

	\ Check disk not already loaded in other drive.
	\ If it is, mark other drive as empty.

	EOR #1
	TAX

	LDA mm32_cluster%
	CMP CHAIN_INDEX,X
	BNE l1

	LDA mm32_cluster%+1
	CMP CHAIN_INDEX+3,X
	BNE l1

	LDA mm32_cluster%+2
	CMP CHAIN_INDEX+6,X		; Flags should be same too.
	BNE l1

	\ Unload other drive

	LDY #0
	JSR mm32_clear_cluster_index_x	; Unload other drive

	\ Copy cluster number from mm32_cluster% to CHAIN_INDEX.
.l1	JSR CheckCRC7
	LDX CurrentDrv
	LDA mm32_cluster%
	STA CHAIN_INDEX,X
	LDA mm32_cluster%+1
	STA CHAIN_INDEX+3,X
	LDA mm32_cluster%+2
	STA CHAIN_INDEX+6,X
	JMP ResetCRC7

}


\\ *DOUT (<drive>)
.mm32_cmd_dout
{
	JSR mm32_param_count
	JSR mm32_param_drive

;	jsr mm32_prtcurdrv
;	jsr PrintNewLine

	tbl = mm32_drvtbl%
	MaxLen = 16

	LDX #0
	LDA CurrentDrv
	ASL A
	ASL A
	ASL A
	ASL A
	TAY
	LDA #' '
.s0 STA tbl,Y
	INX
    INY
	CPX #MaxLen
	BNE s0

	LDY #0
	;JMP mm32_clear_cluster_index
}


\\ Reset cluster number in CHAIN_INDEX
\\ On entry: CurrentDrv=index, Y=attributes
.mm32_clear_cluster_index
	LDX CurrentDrv
	;JMP mm32_clear_cluster_index_x


\\ Reset cluster number in CHAIN_INDEX
\\ On entry: X=index, Y=attributes
\\ Note, for directory 0 = root, for drives it means they are "empty".
.mm32_clear_cluster_index_x
{
	JSR CheckCRC7		; X preserved
	LDA #0
	STA CHAIN_INDEX,X
	STA CHAIN_INDEX+3,X
	TYA
	STA CHAIN_INDEX+6,X
	JMP ResetCRC7
}

\\ *DDRIVE
\\ Show file mapping for both drives (0 and 1)
\\ Bobbi 2020
.mm32_cmd_ddrive
{
	tbl = mm32_drvtbl%
	MaxLen = 16

	LDY #0
.l1
	TYA
	PHA
	LDA #':'
	JSR OSWRCH
	PLA
	PHA
	CLC
	ADC #&30
	JSR OSWRCH
	LDA #' '
	JSR OSWRCH
	PLA
	PHA
	ASL A
	ASL A
	ASL A
	ASL A
	TAX
	LDY #0
	LDA tbl,X
	CMP #mm32_hash
	BNE empty
	INX
.l2	LDA tbl,X
	CMP #mm32_hash
	BEQ s1
	JSR OSWRCH
	INX
	INY
	CPY #MaxLen
	BNE l2
.s1	JSR PrintNewLine
	PLA
	TAY
	INY
	CPY #2
	BNE l1
	RTS
.empty
	JSR PrintString
	EQUB "<Empty>"
	NOP
	JMP s1
}

\\ *DACCESS <dos name> (L)
\\ Set or clear FAT read-only attribute
.mm32_cmd_daccess
{
	LDA #&80			; 1 or 2 parms
	JSR mm32_param_count_a

	SEC					;We are cataloguing.
	JSR mm32_param_filename

	LDX #&01			; Unlock command (mm32_Scan_Dir)
	JSR GSINIT_A
	BNE getparm
.gotflag
	STX &AA				; Same location used by *ACCESS
	
	JSR mm32_Scan_Dir

	LDA #&86
	JSR OSBYTE			; get cursor pos
	CPX #0
	BEQ dcEven

	JSR PrintNewLine

.dcEven
	RTS

.parmloop
	LDX #&02			; Lock command (mm32_Scan_Dir)
.getparm
	JSR GSREAD_A
	BCS gotflag			; If end of string
	AND #&5F
	CMP #&4C			; "L"?
	BEQ parmloop
.errbadparm
	JSR errBAD			; Bad attribute
	EQUB &CF
	EQUS "attribute",0
}

IF _MM32_DDUMP
\\ *DDUMP (<drive>)
\\ Dump contents of image
\\ Output cannot be spooled.
.mm32_cmd_ddump
{
	z = mm32_zptr%	;ptr
	n = &B4			;24bit
	t = mm32_track%
	s = mm32_sector%
	buf%=cat%		;512byte buffer

{
	JSR Param_OptionalDriveNo

	LDA #0
	STA z
	STA n
	STA n+1
	STA n+2

	JSR PrintString
	EQUS "Drive: "
	LDA CurrentDrv
	JSR PrintHex
	JSR PrintNewLine

	LDA #0
	TAY
	JSR mm32_disk_seek_sector
	BVS empty	; If drive empty
	BCS nosec	; If overflow

.loop1
	;jsr prt_trksec

	JSR mm32_readblock
	JSR dump

	JSR mm32_disk_next_sector
	BCC loop1 	; If not EOC/overflow

.nosec
	LDA #Rnosector
	BNE report

.empty
	LDA #Rdrvnotrdy

.report
	JMP ReportIfDiskFault
}

IF FALSE
.prt_trksec
{
	lda t
	jsr PrintHex
	lda #':'
	jsr PrintChrA
	lda t
	BIT ATTRIB_X
	BVC l1				; If SSD
	lsr a
.l1	jsr PrintHex
	lda #'/'
	jsr PrintChrA
	lda s
	jsr PrintHex
	jmp PrintNewLine
}
ENDIF

.dump
{
	LDA #HI(buf%)
	STA z+1

	JSR dumpz
	INC z+1

.dumpz
	LDY #0

.loop1
	LDX #2

.loopA
	LDA n,X
	JSR PrintHex
	DEX
	BPL loopA

	JSR PrintSpace
	TYA
	PHA
	LDX #8

.loop2
	LDA (z),Y
	JSR PrintHex
	JSR PrintSpace
	INY
	DEX
	BNE loop2

	PLA
	TAY
	LDX #8

.loop3
	LDA (z),Y
	CMP #&7F
	BCS l1

	CMP #&20
	BCS l2

.l1	LDA #'.'

.l2	JSR PrintChrA

	INC n
	BNE l3

	INC n+1
	BNE l3

	INC n+2

.l3	INY
	DEX
	BNE loop3

	JSR PrintNewLine
	JSR CheckESCAPE

	TYA
	BNE loop1	;If Y<>0

	RTS
}
}
ENDIF


\\ Get LBA of disk sector for CurrentDrv
\\ Entry: Y=Track, A=Sector
\\ Exit :
\\ If V=1, drive emtpy, else
\\ if C=1, EOC of overflow, else
\\ sec% contains address.
.mm32_disk_seek_sector
{
	STY mm32_track%
	STA mm32_sector%

	LDA CurrentDrv
	AND #1
	TAY
	JSR mm32_SECT_X_First
	BVS l2				; V = 1, Drive empty

	BIT ATTRIB_X		; V = Double sided disk in drive
	LDA CurrentDrv
	ROR A
	ROR A				; C = Side
	BCC l3				; If side 0

	\ It's side 1
	BVS	l3				; If double sided disk

.l1	CLV
	; C=1 = Sector not found

.l2	RTS

.l3	BVC l4				; If single sided disk

	ROL mm32_track%		; Track = Track * 2 + Side
	BCS l1				; Overflow

.l4	LDY mm32_track%
	BEQ l5

.loop1
	LDX #4
	BNE loop2			; always

	\ Target track

.l5	LDA mm32_sector%
	LSR A
	BEQ l6

	TAX
	DEX

.loop2
	JSR mm32_SECT_X_Next
	BCS l1				; If end of cluster chain or error (C=1)

	DEX
	BPL loop2

	DEY
	BEQ l5

	CPY #&FF
	BNE loop1

.l6	JSR mm32_SECT_X_Address
	BCS l1				; If overflow

	ROR mm32_sector%
	BCC l7

	INC sec%

.l7	ASL mm32_sector%
	CLV
	CLC
	RTS
}


\\ Move two 256 byte sectors further
\\ On entry: all variables initialised by mm32_disk_seek_sector
\\ On exit : If C=1, Sector not found, else sec% contains new address.
\\ Also, X&Y preserved.
.mm32_disk_next_sector
{
	JSR RememberXYonly

	LDY #1
	LDX mm32_sector%
	INX
	INX

	CPX #10
	BNE l1

	LDX #0
	INC mm32_track%

	BIT ATTRIB_X
	BVC l1				; If SSD

	LDY #6
	INC mm32_track%

.l1	STX mm32_sector%

.loop1
	JSR mm32_SECT_X_Next
	BCS l2				; If end of cluster chain or error (C=1)

	DEY
	BNE loop1

	JMP mm32_SECT_X_Address

.l2	RTS
}


\\ ********************************************
\\ Report MMC error
\\ On entry: A=result from MMC
.mm32_MMC_error
{
	errno%=&B0		; 8bit
	errptr%=&B8		; 16bit

	STA errno%

	JSR ReportErrorCB
	EQUB &C5			; Drive Fault
	EQUS "MMC "
	NOP

	PLA
	STA errptr%
	PLA
	STA errptr%+1

	LDY #0
	STY MMC_STATE

	\ Print error string
.loop1
	INY
	LDA (errptr%),Y
	BEQ l1

	JSR mm32_PrintChr100
	BNE loop1			; always

.l1	TAY					; Y=0

	\ Print result
	LDA errno%
	JSR PrintHex100

	LDA #'/'
	JSR mm32_PrintChr100

	\ Print parameter
.loop2
	LDA par%,Y
	JSR PrintHex100
	INY
	CPY #4
	BNE loop2

	LDA #0
	STA &100,X
	JMP &100
}


\ Add chr to error string
.mm32_PrintChr100
	STA &100,X
	INX
	RTS


\\ ********************************************
\ 8271 error codes
Rdrvnotrdy=&10
Rwritepro=&12
Rnottrack0=&14
Rnosector=&1E
Rfault=&FF

.Osword7F_8271_Emulation
{
	\ cb already set up as pointer to control block
	cb = &B0 ;16bit pointer
	flags = &C7 ;8bit

{
	;jsr dumpcb

	LDY #6
	LDA (cb),Y		; A=FDC command

	LDX #owtableZ

.loop1
	CMP owtable1,X
	BEQ l2

	DEX
	BPL loop1
	BMI l3			; Unrecognised commands are ignored.

.l2	;Y=6
	JSR ow7f_1

	PHA				; Write result to control block
	LDY #5
	LDA (cb),Y
	CLC
	ADC #7
	TAY
	PLA
	STA (cb),Y

.l3
	LDA #0
	RTS
}

\ Read Special Register
.spec_reg
{
	LDY #7
	LDA (cb),Y		; A = register number
	CMP #6
	BNE l1

	LDA mm32_sector%
	RTS

.l1	CMP #&12		; Side 0 current track
	BNE l4

.l2	LDA mm32_track%
	BIT ATTRIB_X
	BVC l3			; If single sided

	LSR A

.l3	RTS

.l4	CMP #&1A		; Side 1 current track
	BEQ l2

	LDA #0
	RTS
}

.ow7f_1
{
	LDA owtable2,X
	BEQ spec_reg

	PHA
	DEY	;Y=5
	AND #&0F
	CMP (cb),Y		; (Note error in AUG page 61.)
	BNE owfault		; Wrong number of parameters

	JSR MMC_BEGIN1 	; Save ZP &BC-&CB, check init done
	PLA
	STA flags

	\ Copy control block (excluding first byte) to &BC-&C6
	\ Copy buffer address to mm32_taddr% (Tube 32bit address)
	LDY #11

.loop1
	LDA (cb),Y
	STA &BC-1,Y
	CPY #5
	BCS l0

	STA mm32_taddr%-1,Y
.l0	DEY
	BNE loop1

	;LDY #0
	LDA (cb),Y
	BMI l1			; If bit 7 set use the default drive

	JSR SetCurrentDrive_Adrive	; Use drive in control block

.l1	JSR ow7f_2

	PHA
	JSR MMC_END		; Restore ZP &BC-&CB
	PLA
	RTS

.owfault
	PLA
	LDA #Rfault
	RTS
}

.owdrvnotrdy
	LDA #Rdrvnotrdy
	RTS

.ownosector
	LDA #Rnosector
	RTS

.ow7f_2
{
	z_track=&C2		; Track
	z_sector=&C3	; Start Sector
	z_count=&C4		; (Sector Size +) Sector Count

	BIT flags		; N=Block, V=verify|format
	BPL l0			; If not block command

	\ Block commands data length is in bytes.

	LDA byteslastsec%
	BEQ l3

	\ If byteslastsec%>0 increment sector count.
	INC seccount%
	BNE l3

IF _LARGEFILES
	INC seccount%+1
	BNE l3
ENDIF
	BEQ ownosector	; Overflow

.l0	LDA z_count
	BVC l1

	LDA #10			; For Verify & Format, sector count =10

.l1	AND #&1F		; Strip Sector Size
	STA seccount%
	BVS l2			; If verifying or formatting there is no start sector.

	\ Check start sector + sector count
	CLC
	ADC z_sector
	CMP #11
	BCS ownosector	; If start + count > 10

.l2	LDA #0
IF _LARGEFILES
	STA seccount%+1
ENDIF
	STA byteslastsec%
	BVC l3			; If not verify|format

	;LDA #0			; start sector = 0
	STA z_sector

.l3	LDA z_sector
	CMP #10
	BCS ownosector	; If start sector >= 10

.l4	LDY z_track

	\ Y=Track, A=Sector
	JSR mm32_disk_seek_sector ; Corrupts z_sector & z_track
	BVS	owdrvnotrdy	; Drive empty
	BCS ownosector	; No sector

	ROL flags
	BIT flags 		; N=verify|format, V=write
	BVC l5

	\ Check write protect
	BIT ATTRIB_X
	BPL l5

	LDA #Rwritepro	; READ ONLY!
	RTS

.l5	BIT flags
	BMI vform		; If verifying or formatting

	\ Are we reading/writing the catalogue buffer?
	\ e.g. buffer @ &0E00 in the host and sector count = 2
	\ and start sector must be even.

	ROR z_sector
	BCS l6			; If start sector odd

	LDA seccount%
	CMP #2
	BNE l6

	LDA datptr%+1
	CMP #HI(cat%)
	BNE l6

	LDA mm32_taddr%+2
	AND mm32_taddr%+3
	ORA TubePresentIf0
	EOR #&FF
	ORA datptr%
IF _LARGEFILES
	ORA seccount%+1
ENDIF
	ORA byteslastsec%
	BEQ cat

.l6	BIT flags
	BVS l9			; If writing

	JSR MMC_ReadBlock

.l7	LDA TubeNoTransferIf0
	BEQ l8

	JSR TUBE_RELEASE_NoCheck

.l8	BIT mm32_error%		; Check if error occured
	BMI ownosector2

.OK	LDA #0
	RTS

.l9	JSR MMC_WriteBlock
	JMP l7

	\ This bit's used for verify/format, or reading/writing the catalogue.
	\ Sector count = 2 or 10

	\ Verify/Format Track
.vform
	LDY #&FF
	STY CurrentCat		; Invalidate catalogue

	BVC cat				; If verifying

	\ If formatting clear the catalogue buffer
	INY		;Y=0
	TYA		;A=0

.floop
	STA MA+&0E00,Y
	STA MA+&0F00,Y
	INY
	BNE floop

	\ Read\write catalogue buffer seccount/2 times
.cat
{
.loop
	BIT flags
	BVS k1			; If writing

	JSR MMC_ReadCatalogue
	JMP k2

.k1	JSR MMC_WriteCatalogue

.k2	DEC seccount%
	DEC seccount%
	BEQ OK

	JSR mm32_disk_next_sector
	BCC loop
	;BCS ownosector
}

.ownosector2
	LDA #Rnosector
	RTS
}

	\ Recognised 8271 commands
.owtable1
	EQUB &7D, &4A, &4B, &4E, &4F, &52, &53, &56, &57, &5E, &5F, &63, &A5, &85
	\ Flags: Bit 7 = Block, bit 6 = Verify/Format, bit 5=Write
	\ Lower nibble = number of parameters
.owtable2
	EQUB &00, &23, &23, &23, &23, &03, &03, &03, &03, &45, &43, &65, &A5, &85

	owtableZ=owtable2-owtable1

IF FALSE
	; dump control block
.dumpcb
{
	ldy #5
	lda (cb),Y
	clc
	adc #6
	tax
	ldy #0
.xx11
	lda (cb),y
	jsr PrintHex
	jsr PrintSpace
	iny
	dex
	bpl xx11
	jmp PrintNewLine
}
ENDIF
}


\\ MM32.asm
\\ END OF FILE
