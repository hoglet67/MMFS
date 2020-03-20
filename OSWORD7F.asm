\** MMFS ROM by Martin Mather
\** Compiled using BeebAsm V1.04
\** June/July 2011

\\ **** OSWORD &7F EMULATION ****

owbptr%=&B0	; Pointer to parameter block
owresult%=&B2	; Offset to result byte
owfdcop%=&B3	; FDC operation
owsec%=&B4

	\ 8271 error codes
Rdrvnotrdy=&10
Rwritepro=&12
Rnottrack0=&14
Rnosector=&1E
Rfault=&FF

.Osword7F_8271_Emulation
{
	\ Copy buffer address to &BC-&BD;&1074-&1075
	\ This needs to be called before MMC_BEGIN2
	LDY #1
	LDX #2
	JSR CopyVarsB0BA

	LDY #0
	STY byteslastsec%
IF _LARGEFILES
	STY seccount%+1
ENDIF
	\ Set drive?
	LDA (owbptr%),Y
	BMI ownoreset
	JSR SetCurrentDrive_Adrive
.ownoreset

	\ We are relying on this to copy &BC,&BD to &1090,&1091
	\ which is the LS bytes of the tube transfer address.
	\ so this must happen, even if the current drive is not set.
	JSR MMC_BEGIN2

	LDY #5
	LDA (owbptr%),Y			; no. of parameters
	CLC
	ADC #7
	STA owresult%

	JSR ow7F			; returns result in A
	LDY owresult%
	STA (owbptr%),Y

    \ Unrecognised FDC commands are ignored
.owuknown
	LDA #0				; Service done!
	RTS

\.owuknown
	\pha
	\jsr PrintString
	\equs "OW="
	\nop
	\pla
	\jsr PrintHex
	\LDA #0
	\RTS

.ow7F
	\\ Check drive ready
	LDX CurrentDrv
	LDA DRIVE_INDEX4,X
	BPL owdrvnotrdy			; drive not loaded
	AND #&08
	BNE ownosector			; disc not formatted

	INY
	LDA (owbptr%),Y			; FDC command
	STA owfdcop%
	CMP #&53			; read
	BEQ owrw
	CMP #&57			; read deleted data
	BEQ owrw
	CMP #&4B			; write
	BNE owuknown

	\\ Check Write protect
	LDA DRIVE_INDEX4,X		; Bit 6 set = protected
	ASL A
	BPL owrw
	LDA #Rwritepro
	RTS

.ownosector
	LDA #Rnosector
	RTS

.owdrvnotrdy
	LDA #Rdrvnotrdy
	RTS

.owfault
	LDA #Rfault
	RTS

	\\ Check 3 params (sec/trk/len)
.owrw
	LDA owresult%
	CMP #7+3
	BNE owfault

	\\ Calc 1st disc sector = trk*10+sec
	LDA #0
	STA owsec%+1
	INY
	LDA (owbptr%),Y			; TRACK
	CMP #80				; Check track no<80
	BCS ownosector
	ASL A
	STA owsec%
	ASL A
	ROL owsec%+1
	ASL A
	ROL owsec%+1
	ADC owsec%
	STA owsec%
	BCC owsk1
	INC owsec%+1

.owsk1
	INY
	LDA (owbptr%),Y			; SECTOR
	CMP #10				; Check sector no.<10
	BCS ownosector
	; CLC ; C=0 at this point
	ADC owsec%
	STA owsec%
	BCC owsk2
	INC owsec%+1

.owsk2
	INY
	LDA (owbptr%),Y			; LENGTH
	AND #&1F
	BEQ owretok
	STA seccount%

	\ Check last sector no. to read < 11
	CLC
	DEY
	ADC (owbptr%),Y			; LENGTH + SECTOR
	CMP #11
	BCS ownosector

	\\ Get mmc addr of 1st sector
.owsk3
	LDX CurrentDrv
	JSR DiskStartX

	CLC
	LDA sec%
	ADC owsec%
	STA sec%
	LDA sec%+1
	ADC owsec%+1
	STA sec%+1
	BCC owsk4
	INC sec%+2

.owsk4
	LDA owfdcop%
	CMP #&4B			; Write
	BEQ owwrite

	JSR MMC_ReadBlock

.owexit
	LDA TubeNoTransferIf0
	BEQ owretok
	JSR TUBE_RELEASE_NoCheck

.owretok
	LDA #0
	RTS

.owwrite
	JSR MMC_WriteBlock
	JMP owexit
}
