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
	BEQ fat				; FAT not recognised Assume sector 0 is start of image
	CLC				; return C=0 to indicate no FAT
	RTS

	\\ FAT signature word?
.isfat
	LDA cat%+&1FE
	CMP #&55
	BNE ifx
	LDA cat%+&1FF
	CMP #&AA
.ifx
	RTS

\\ Boot sector signature 0x55 0xAA found

\\ Test for presence of FAT Partition Boot Sector
\\ 0x000 = 0xEB xx 0x90 is a good indicator
\\ If this is not found, then assume sector 0 is an MBR

.fat
   LDA cat%
   CMP #&EB
   BNE mbr
   LDA cat%+2
   CMP #&90
   BNE mbr
   LDA cat%+&C
   CMP #&02
   BEQ nombr

.mbr
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
	LSR A : ROR fatdirsize%
	LSR A : ROR fatdirsize%
	LSR A : ROR fatdirsize%
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

	\\ &11-&12  Number of root directory entries (224)
	\\		  0 for FAT32. 512 is recommended for FAT16.
	LDA cat%+&11
	ORA cat%+&12
	BEQ fat32
	\\ Make FAT16 look like FAT32
	\\ &16-&17 Sectors per FAT for FAT16
	\\ &24-&27 Sectors per FAT for FAT32
	LDA cat%+&16
	STA cat%+&24
	LDA cat%+&17
	STA cat%+&25
	LDA #0
	STA cat%+&26
	STA cat%+&27
.fat32

	\\ fat size = fat sectors * 2

	ASL cat%+&24
	ROL cat%+&25
	ROL cat%+&26
	ROL cat%+&27
	BCS faterr1

	\\ sec = sec + fat copies * fat size
	LDX cat%+&10			; fat copies
.loop
	CLC
	LDA sec%
	ADC cat%+&24
	STA sec%
	LDA sec%+1
	ADC cat%+&25
	STA sec%+1
	LDA sec%+2
	ADC cat%+&26
	STA sec%+2
	DEX
	BNE loop
	JSR MMC_ReadCatalogue		; Root Dir
	SEC				; return C=1 to indicate FAT
	RTS
}


	\\ **** SEARCH FOR FILE ****
	\\ Exit: C=0 = File found

.FATSearchRootDirectory
fatfilename%=MA+&10F0
{
fatptr%=&C4		; word
fatclust%=&C3		; 24-bits (3 bytes)

	\\ Search dir (first 16 entries only)
	LDA sec%+2 :PHA
	LDA sec%+1 :PHA
	LDA sec%   :PHA
.FATSearchRootDirectoryloop
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
	LDA (fatptr%),Y
	BEQ filenotfound ;  end of FAT
	CMP fatfilename%,Y
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

.incfatsector

	LDA sec%
	CLC
	ADC #2
	STA sec%
	bcc readnextfatsector
	inc sec%+1
	BNE readnextfatsector
	inc sec%+2
.readnextfatsector
	JSR MMC_ReadCatalogue
	JMP FATSearchRootDirectoryloop

.filenotfound
	PLA
	PLA
	PLA
	SEC
	RTS

	\\ file found
.filefound

	\\ sec = sec + max dir entries
	CLC
	PLA
	ADC fatdirsize%
	STA sec%
	PLA
	ADC fatdirsize%+1
	STA sec%+1
	PLA
	ADC #0
	STA sec%+2

	LDX fatclustsize%

	\\ cluster = file cluster - 2

	\\ In FAT32 the file start cluster is 32 bits and is stored the directory entry
	\\ offsets &15, &14, &1B, &1A (MSB .. LSB).
	\\
	\\ In FAT16, offset &15 and &14 are reserved and should be zero
	\\
	\\ MMFS only deals with 24-bit sector addresses so we ignore bits 31..24
	\\ A 24-bit sector address allows for the file system to be upto 8GB
	\\
	\\ Note: Originally MMFS treated the file start cluster as a 16 bit value.
	\\ With a cluster size of 8 (4KB) that cause problems if the file start
	\\ was more that 128MB into the file system. This was fixed in MMFS 1.51.

	LDY #&14
	LDA (fatptr%),Y		; bits 23..16 of file start cluster in &14
	PHA
	LDY #&1B
	LDA (fatptr%),Y		; bits 15..8 of file start cluster in &1B
	PHA
	DEY
	LDA (fatptr%),Y		; bits 0..7 of file start cluster in &1A
	SEC
	SBC #2
	STA fatclust%
	PLA
	SBC #0
	STA fatclust%+1
	PLA
	SBC #0
	STA fatclust%+2
	ORA fatclust%+1
	ORA fatclust%
	BEQ exit			; if cluster = 0

	\\ cluster = cluster * 2
	ASL fatclust%
	ROL fatclust%+1
	ROL fatclust%+2

	\\ sec = sec + cluster * size
.clustloop
	CLC
	LDA sec%
	ADC fatclust%
	STA sec%
	LDA sec%+1
	ADC fatclust%+1
	STA sec%+1
	LDA sec%+2
	ADC fatclust%+2
	STA sec%+2
	DEX
	BNE clustloop

.exit
	CLC
	RTS
}
	\\ End of FAT routine
