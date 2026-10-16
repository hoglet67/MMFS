\\ MMFS2 on the Pi1MHz FAT service (_PI1MHZ_FILES_)
\\
\\ MM32.asm calls these where it would read or write FAT structures on
\\ the card: the Pi holds each drive's disc image open by name, keeps
\\ each drive's directory, and makes that directory's blocks as FAT
\\ entries, so the drives see files, not sectors.  The command blocks
\\ and pi1mhz_point/go/buffer are in MMC_Pi1MHz.asm; the service's
\\ commands are documented with the Pi1MHz firmware (docs/advanced.md).

\\ Directory block CLUST_X (mm32_Scan_Dir) into cat%: the Pi makes it from
\\ the slot's directory, as FAT entries (command 21)
.pi1mhz_dir_block
{
	LDA #&FF
	STA CurrentCat
	JSR pi1mhz_point
	LDA #21
	STA discaccess+3
	LDA #0
	STA discaccess+3
	STA discaccess+3
	STA discaccess+3
	JSR pi1mhz_buffer
	LDA CLUST_X
	STA discaccess+3
	LDA CLUST_X+1
	STA discaccess+3
	LDA #0
	STA discaccess+3
	STA discaccess+3
	JSR pi1mhz_go
	BEQ ok
	JMP errRead
.ok
	JSR SetupCatRW
	LDA #0
	STA discaccess
	LDA #&E0
	STA discaccess+1
	JSR MMC_Read256
	INC datptr%+1
	JSR MMC_Read256
	JMP ResetLEDS
}

.pi1mhz_drive_page
	EQUB &FB, &FA, &FB	; drive 0, drive 1, the current directory

\\ Copy the name at mm32_str%+X (ended by mm32_hash, or by the end of
\\ its 16-byte field) into the command block, then a 0.  X = the first
\\ character, after the mm32_hash.  Y preserved.
.pi1mhz_put_name
{
.loop
	LDA mm32_str%,X
	CMP #mm32_hash
	BEQ end
	STA discaccess+3
	INX
	TXA
	AND #15
	BNE loop
.end
	LDA #0
	STA discaccess+3
	RTS
}

\\ A found entry (mm32_chain_open): open the file on drive CurrentDrv's
\\ slot - kept, so a BBC reset reopens it, read-only when it is locked -
\\ or, for *DDIR, make it both drives' directory.  Name in mm32_str%.
\\ Exit C=1 if the Pi could not, the drive then empty (*DDIR: the root).
.pi1mhz_attach
{
	LDX CurrentDrv
	CPX #2
	BEQ dir
	LDA pi1mhz_drive_page,X
	STA pi1mhz_slot
	JSR pi1mhz_point
	LDA #2				; fopen
	STA discaccess+3
	STA discaccess+3
	LDA mm32_attrib%
	ASL A				; C = locked
	LDA #&83			; kept, read + write
	BCC rw
	LDA #&81			; kept, read only
.rw	STA discaccess+3
	LDX #1
	JSR pi1mhz_put_name
	JSR pi1mhz_go
	BNE fail
.ok	CLC
	RTS

.dir
	LDA #&FB
	JSR chdir
	BNE fail
	LDA #&FA
	JSR chdir
	BEQ ok
.fail
	\\ The Pi could not: an empty drive (or the root), as in *DDRIVE
	LDY #0
	JSR mm32_clear_cluster_index
	LDA CurrentDrv
	CMP #2
	BCS nodrv
	ASL A
	ASL A
	ASL A
	ASL A
	TAX
	LDA #' '
	STA mm32_drvtbl%,X
.nodrv
	SEC
	RTS

.chdir
	STA pi1mhz_slot
	JSR pi1mhz_point
	LDA #22				; the slot's directory
	STA discaccess+3
	LDX #1
	JSR pi1mhz_put_name
	JMP pi1mhz_go
}

\\ Drive X (0 or 1) emptied: close its file on the Pi.  X=2: both drives'
\\ directory back to the root.  X & Y preserved.
.pi1mhz_release
{
	TXA
	PHA
	TYA
	PHA
	CPX #2
	BEQ root
	LDA pi1mhz_drive_page,X
	STA pi1mhz_slot
	JSR pi1mhz_point
	LDA #3				; fclose
	STA discaccess+3
	JSR pi1mhz_go
	JMP done

.root
	LDA #&FB
	JSR toroot
	LDA #&FA
	JSR toroot
.done
	PLA
	TAY
	PLA
	TAX
	RTS

.toroot
	STA pi1mhz_slot
	JSR pi1mhz_point
	LDA #22
	STA discaccess+3
	LDA #0				; "": the root
	STA discaccess+3
	JMP pi1mhz_go
}

\\ The card id, in file mode: the Pi's boot id, from its card-type
\\ command (20) at +1,+2 of the block, so a Pi restart reads as a changed
\\ card.  A Pi without commands 19, 21 and 22 leaves the zeros: it would
\\ never answer them.  Exit YA = the id.
.pi1mhz_boot_id
{
	LDA pi1mhz_slot
	PHA				; the caller's slot, kept
	LDA #&FB
	STA pi1mhz_slot
	JSR pi1mhz_point
	LDA #20
	STA discaccess+3
	LDA #0
	STA discaccess+3
	STA discaccess+3
	JSR pi1mhz_go
	JSR pi1mhz_point
	PLA
	STA pi1mhz_slot
	LDA discaccess+3		; +0
	LDY discaccess+3
	LDA discaccess+3
	BNE ok
	CPY #0
	BNE ok
	STA MMC_STATE			; A=0: not initialised, so ask again
	JSR ReportError
	EQUB &FF
	EQUS "Pi1MHz too old",0
.ok	RTS
}

\\ Set the read-only bit of the found entry (named in mm32_str%) to A.
\\ A refusal is a "Write fault", as a failed directory write-back is.
.pi1mhz_set_locked
{
	PHA
	LDA #&FB
	STA pi1mhz_slot
	JSR pi1mhz_point
	LDA #19				; set attributes
	STA discaccess+3
	LDA #1				; the bit: read only
	STA discaccess+3
	PLA
	STA discaccess+3
	LDX #1
	JSR pi1mhz_put_name
	JSR pi1mhz_go
	BNE fail
	RTS
.fail
	JMP errWrite			; A = the Pi's result
}

\\ *DRENAME: the found name (mm32_str%+1) to the new one (mm32_str%+33), in
\\ the directory (command 12).  Exit C=0: the end of mm32_Scan_Dir.
.pi1mhz_rename
{
	LDA #&FB
	STA pi1mhz_slot
	JSR pi1mhz_point
	LDA #12				; rename, in the slot's directory
	STA discaccess+3
	LDX #1
	JSR pi1mhz_put_name		; the found name
	LDX #33
	JSR pi1mhz_put_name		; the new one
	JSR pi1mhz_go
	BEQ ok
	JMP errWrite			; A = the Pi's result
.ok	CLC				; End mm32_Scan_Dir
	RTS
}

\\ mm32_init_dos at a hard reset (a soft one keeps CLUST_SIZE), or when
\\ the Pi has restarted (MMC_CheckCardID): empty drives, their files
\\ closed on the Pi, the directory the root.  The id first: it stops here
\\ on a Pi too old for what follows.
.pi1mhz_init
{
	JSR MMC_GetCIDCRC
	LDX #0
	JSR pi1mhz_release
	INX
	JSR pi1mhz_release
	INX
	JSR pi1mhz_release
	JSR CheckCRC7
	JSR MMC_GetCIDCRC	; Remember card id
	STA MMC_CIDCRC+1
	STY MMC_CIDCRC
	LDA #1
	STA CLUST_SIZE		; >0: done
	LDA #0
	LDY #8
.ploop
	STA CHAIN_INDEX,Y
	DEY
	BPL ploop
	LDA #' '			; and *DDRIVE's names (as setdefaults)
	STA mm32_drvtbl%
	STA mm32_drvtbl%+16
	JMP ResetCRC7
}

\\ Start at block 0 of drive Y's file (0 or 1), or of the current
\\ directory (Y=2): the Pi holds them open (MMC_Pi1MHz.asm), so a
\\ "cluster chain" is just a block counter in CLUST_X, and pi1mhz_slot the
\\ slot that has the file.  CHAIN_INDEX still holds an entry's id and flags.
\\ If Y<2 then V=1 if drive empty
.mm32_SECT_X_First
{
	LDA CHAIN_INDEX+6,Y
	STA ATTRIB_X		; Save flags
	AND #&3F
	ORA CHAIN_INDEX,Y
	ORA CHAIN_INDEX+3,Y
	BNE l2
	CPY #2
	BNE l3				; An empty drive (the current dir is the root)

.l2	LDA #0
	STA CLUST_X
	STA CLUST_X+1
	STA CLUST_X+2
	LDA pi1mhz_drive_page,Y
	STA pi1mhz_slot
	CLV
	RTS

.l3	BIT l4				; Set V

.l4	RTS
}

\\ The next 512-byte block: never the end (C=0) - the Pi answers a read
\\ past a file's end short (MMC_WaitForData).  X & Y preserved.
.mm32_SECT_X_Next
{
	INC CLUST_X
	BNE l1
	INC CLUST_X+1
.l1	CLC
	RTS
}

	ASSERT pi1mhz_slot = FRAG_X	; MMC_Pi1MHz.asm keeps the slot page there
