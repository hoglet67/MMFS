\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

\\ ******* FAT CODE ********

	\\ ******* LOAD FAT ROOT DIRECTORY ******

fatdirsize%=&C1		; word
fatclustsize%=&C3	; byte

.FATLoadRootDirectory
{
	\\ Read sector 0 (MBR)
	LDA #&DD
	STA CurrentCat
	LDA #0
	STA sec%
	STA sec%+1
	STA sec%+2
	JSR MMC_ReadCatalogue		; MBR?
	JSR isfat
	BEQ fat
	BNE faterr1			; Not FAT16

	\\ FAT signature word?
.isfat
	LDA cat%+&1FE
	CMP #&55
	BNE ifx
	LDA cat%+&1FF
	CMP #&AA
.ifx
	RTS


	\\ FAT found - assume FAT16
	\\ with MBR (partitioned disk)

.fat
	\\ VBR=MBR?
	LDA cat%+&1C6
	ORA cat%+&1C7
	ORA cat%+&1C8
	ORA cat%+&1C9
	BEQ nombr

	\\ sec = cat!&1C6 * 2
	LDA cat%+&1C6
	ASL A
	STA sec%
	LDA cat%+&1C7
	ROL A
	STA sec%+1
	LDA cat%+&1C8
	ROL A
	BCS faterr1
	STA sec%+2
	LDA cat%+&1C9
	BNE faterr1
	JSR MMC_ReadCatalogue		; VBR

	JSR isfat
	BEQ nombr

.faterr1
	JSR ReportError
	EQUB &FF
	EQUS "Card format?",0


	\\ Sec size = 512?
.nombr	LDA cat%+&B
	BNE faterr1
	LDA cat%+&C
	CMP #2
	BNE faterr1

	\\ Save vars

	\\ fatdirsize = (max dir entries / 16) * 2
	LDA cat%+&11
	STA fatdirsize%
	LDA cat%+&12
	LSR A
	ROR fatdirsize%
	LSR A
	ROR fatdirsize%
	LSR A
	ROR fatdirsize%
	STA fatdirsize%+1

	\\ cluster size
	LDA cat%+&D
	STA fatclustsize%

	\\ Calc Root Dir sector

	\\ sec = sec + Reserved sectors * 2
	LDA cat%+&E
	ASL A
	ROL cat%+&F
	BCS faterr1

	ADC sec%
	STA sec%
	LDA cat%+&F
	ADC sec%+1
	STA sec%+1
	BCC skipinc1
	INC sec%+2
.skipinc1

	\\ fat size = fat sectors * 2
	ASL cat%+&16
	ROL cat%+&17
	BCS faterr1

	\\ sec = sec + fat copies * fat size
	LDX cat%+&10			; fat copies
.loop
	CLC
	LDA sec%
	ADC cat%+&16
	STA sec%
	LDA sec%+1
	ADC cat%+&17
	STA sec%+1
	BCC skipinc2
	INC sec%+2
.skipinc2
	DEX
	BNE loop
	JMP MMC_ReadCatalogue		; Root Dir
}
	

	\\ **** SEARCH FOR FILE ****
	\\ Entry: XY Point to filename
	\\ Exit: C=0 = File found

.FATSearchRootDirectory
fatfilename%=MA+&10F0
{
fatptr%=&C4		; word
fatclust%=&C4		; word

	\\ Search dir (first 16 entries only)
	
	LDX #0
	STX fatptr%
	LDA #HI(cat%)
	STA fatptr%+1

	INX
.dirloop
	LDY #&B				; is file deleted?
	LDA (fatptr%),Y
	AND #&F
	BNE nextfile

	DEY				; compare filenames
.comploop
	LDA fatfilename%,Y
	CMP (fatptr%),Y
	BNE nextfile			; no match
	DEY
	BPL comploop
	BMI filefound			; file found!

.nextfile
	CLC				; next file?
	LDA fatptr%
	ADC #32
	STA fatptr%
	BNE dirloop
	INC fatptr%+1
	DEX
	BPL dirloop

	SEC
	RTS

	\\ file found
.filefound
	\\ sec = sec + max dir entries
	CLC
	LDA sec%
	ADC fatdirsize%
	STA sec%
	LDA sec%+1
	ADC fatdirsize%+1
	STA sec%+1
	BCC skipinc1
	INC sec%+2
.skipinc1

	\\ cluster = file cluster - 2
	LDY #&1B
	LDA (fatptr%),Y			; file cluster
	PHA
	DEY
	LDA (fatptr%),Y
	SEC
	SBC #2
	STA fatclust%
	PLA
	SBC #0
	STA fatclust%+1

	ORA fatclust%
	BEQ exit			; if cluster = 0

	\\ cluster = cluster * 2
	ASL fatclust%
	ROL fatclust%+1

	\\ sec = sec + cluster * size
	LDX fatclustsize%

.clustloop
	CLC
	LDA sec%
	ADC fatclust%
	STA sec%
	LDA sec%+1
	ADC fatclust%+1
	STA sec%+1
	BCC skipinc2
	INC sec%+2
.skipinc2

	DEX
	BNE clustloop

.exit
	CLC
	RTS
}
	\\ End of FAT routine