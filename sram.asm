osrdrm  =       $ffb9
osfile  =       &ffdd
osasci  =       &ffe3

;;; Zero-page workspace

        org     &a8
.oshwm  equw    &0000
.himem
.romtab equw    &0000
.romid  equb    &00
.srcpag equb    &00
.dstpag equb    &00
.pages
.copywr equb    &00

;;; OSFILE control block used for loading/saving files.

        org     &0100
.osf_fn equw    &0000
.osf_ld equd    &00000000
.osf_ex equd    &00000000
.osf_st equd    &00000000
.osf_en equd    &00000000

        org     bootend         ; continue from end of bootstrap code.

;;; Operating system command parser.  This is called in response to
;;; ROM service call 04.

.oscmd
{
        ldx     #&ff
        dey
        tya
        pha
.cmdlp  pla
        pha
        tay
.chrlp1 inx
        iny
        lda     cmdtab,x        ; character from command table.
        cmp     #'a'            ; if lower case then abbrevations are now ok.
        bcs     abbrok
        eor     (&f2),y         ; case-insenitive comparison.
        and     #&5f
        beq     chrlp1
.next   inx                     ; skip forward to the exec address.
        lda     cmdtab,x
        bpl     next
        inx                     ; skip past one byte of the address.
        cpx     #(cmdend-cmdtab-1)
        bne     cmdlp
.notfnd pla
        tay
        iny
        lda     #&04            ; restoring the service code 04 tells the
        rts                     ; OS we did not claim this command.
.done   pla
        tay
        iny
        lda     #&00            ; setting A=00 says we did claim this command.
        rts
.chrlp2 inx
        iny
.abbrok lda     (&f2),y
        cmp     #&2e
        beq     gotdot
        eor     cmdtab,x        ; case-insensitive comparison.
        and     #&5f
        beq     chrlp2
        lda     cmdtab,x
        bpl     next
        lda     (&f2),y
        cmp     #&0d
        beq     found
        cmp     #' '
        bne     next
.splp   iny                     ; skip spaces before calling the subroutine
        lda     (&f2),y         ; to handle this command.
        cmp     #' '
        beq     splp
.found  lda     #>(done-1)      ; set the return address that the subroutine
        pha                     ; implementing this command will return to.
        lda     #<(done-1)
        pha
        lda     cmdtab,x        ; push the subroutine address ready for RTS
        pha                     ; to go to it.  It is stored MSB first as that
        lda     cmdtab+1,x      ; is negative and thus marks the end of the
        pha                     ; command.
        rts
.enlp   inx
.gotdot lda     cmdtab,x        ; as the command was abbreviated, skip past
        bpl     enlp            ; the remaining letters to find the address
        bmi     splp
}

;;; HELP command processing.  This is ROM service call 09.

.help
{
        tya
        pha
        lda     (&f2),y         ; If the command tail is empty we just
        cmp     #&0d            ; give the short help.
        beq     short
        ldx     #&00            ; otherwise check if the tail matches our
        cmp     hlpstr,x        ; name.
        bne     notus
.chrlp  inx
        iny
        lda     (&f2),y
        cmp     hlpstr,X
        beq     chrlp
        cmp     #&0d
        beq     found
        cmp     #' '
        beq     found
.notus  pla
        tay
        lda     #&09            ; set A=09 so as not to claim the call.
        rts
.short  jsr     prtitl          ; short version of the help output, i.e.
        lda     #' '            ; ROM title, version, and which name will
        jsr     oswrch          ; give the long version.
        jsr     oswrch
        ldx     #&00
        lda     hlpstr
.shlp   jsr     oswrch
        inx
        lda     hlpstr,x
        bne     shlp
        jsr     osnewl
        pla
        tay
        lda     #&09
        rts
.found  jsr     prtitl          ; long version of the help output which
        ldx     #&00            ; gives the syntax for the commands.
        lda     hlptxt
.floop  jsr     osasci
        inx
        lda     hlptxt,x
        bne     floop
        pla
        tay
        lda     #&00
        rts

.prtitl jsr     osnewl          ; print our ROM title.
        ldx     #&00
        lda     title
.prtlp1 jsr     oswrch
        inx
        lda     title,x
        bne     prtlp1
        lda     #' '
        jsr     oswrch
        inx
        lda     title,x         ; print our version.
.prtlp2 jsr     oswrch
        inx
        lda     title,x
        bne     prtlp2
        jmp     osnewl

.hlpstr equs    "SRAM"          ; the name for HELP.
        equb    &00

.hlptxt equb    &0d
        equs    "  INSERT <rom>"
        equb    &0d
        equs    "  ROMS"
        equb    &0d
        equs    "  SRLOAD <filename> <rom>"
        equb    &0d
        equs    "  SRSAVE <filename> <rom>"
        equb    &0d
        equs    "  SRWIPE <rom>"
        equb    &0d
        equs    "  UNPLUG <rom>"
        equb    &0d
IF _SRAM_WP_
        equs    "  WPSET <rom>"
        equb    &0d
        equs    "  WPCLR <rom>"
        equb    &0d
ENDIF
        equb    &00
        }

;;; Construct the address for the command table.  The -1 accounts for the
;;; behaviour of RTS and the MSB is first as all ROM addresses have a
;;; negative MSB, when seen as an 8 bit twos compliment number, so this
;;; neatly marks the end of the command name.

        MACRO   cmdadr addr
        equb    >(addr-1)
        equb    <(addr-1)
        ENDMACRO

;;; Command table.

.cmdtab equs    "INSert"
        cmdadr  insert
        equs    "ROMS"
        cmdadr  roms
        equs    "SRLoad"
        cmdadr  srload
        equs    "SRSave"
        cmdadr  srsave
        equs    "SRWipe"
        cmdadr  srwipe
        equs    "UNPlug"
        cmdadr  unplug
IF _SRAM_WP_
        equs    "WPSet"
        cmdadr  wpset
        equs    "WPClr"
        cmdadr  wpclr
ENDIF
.cmdend

;;; Abort with an error message.  The error number and text immediately
;;; follow the call.  These are transferred to the bottom of page 1 to
;;; be executed.

.errmsg
{
        pla
        sta     &fd
        pla
        sta     &fe
        ldy     #$00
.loop   iny
        lda     (&fd),y
        sta     &0100,y
        bne     loop
        sta     &0100
        jmp     &0100
}

;;; Test if a given ROM bank contains RAM.  The ROM number is in ZP at romid
;;; Returns Z=1 if RAM, Z=0 if ROM.

.ramtst
{
        ldx     #tsten-tstst    ; copy routine to the bottom of page 1.
.cplp   lda     tstst,X
        sta     osf_ld,x
        dex
        bpl     cplp
        jmp     osf_ld          ; run the routine.
.tstst  ldy     &f4
        ldx     romid
        page_rom_x
        lda     &8006
        eor     #&FF
        sta     &8006
        cmp     &8006
        php
        eor     #&FF
        sta     &8006
        tya
        tax
        page_rom_x              ; back to the ROM.
        jmp     tsten
.tsten  plp
        rts
}

;;; Get the address of the OS ROM table.

.getrtb
{
        lda     #&aa
        ldx     #&00
        ldy     #&ff
        jsr     osbyte
        stx     romtab
        sty     romtab+1
        rts
}

;;; use OSRDRM to fetch the copyright pointer.

.rdcpyr
{
        lda     #&07            ;
        sta     &f6
        lda     #&80
        sta     &f7
        ldy     romid
        jsr     osrdrm
        sta     copywr
        rts
}

;;; Get the next byte from the ROM being worked on.

.gtrbyt ldy     romid
        jsr     osrdrm
        inc     &f6
        rts

;;; Check if a ROM bank contains a valid ROM image.

.chkrom
{
        jsr     rdcpyr          ; find copyright pointer.
        sta     &f6
        jsr     gtrbyt          ; check bytes...
        cmp     #&00
        bne     empty
        jsr     gtrbyt
        cmp     #'('
        bne     empty
        jsr     gtrbyt
        cmp     #'C'
        bne     empty
        jsr     gtrbyt
        cmp     #')'
        bne     empty
        lda     #&06            ; get the ROM type byte.
        sta     &f6
        ldy     romid
        jsr     osrdrm
        clc                     ; indicate sucess.
        rts
.empty  sec                     ; indicate failure.
        rts
}

;;; The ROMS command.

.roms
{
        jsr     getrtb          ; find the OS ROM table.
        ldy     #&0f            ; loop through 15->0
.rmloop sty     romid
        jsr     ramtst
        beq     gotram
        ldy     romid
        lda     (romtab),y
        bne     gotrom
.next   dey                     ; next ROM.
        bpl     rmloop
        rts
.gotrom tax                     ; Found ROM in this slot.  For ROMs the
        jsr     prinfo          ; information printed is based on ROM type
        jsr     space           ; byte from the OS ROM table.
        jsr     cparen
        jsr     rdcpyr
        jsr     prtitl
        ldy     romid
        jmp     next
.gotram jsr     chkrom          ; Found RAM.  Valid ROM image?
        bcs     empty
        tax                     ; As this RAM contains what looks like a ROM
        jsr     prinfo          ; image, print info based on the ROM type
        jsr     rparen          ; byte read from the RAM bank itself.
        jsr     prtitl
        ldy     romid
        jmp     next
.empty  ldx     #&00            ; RAM without a valid ROM image so print
        jsr     prinfo          ; info based on a ROM type byte of 0 and
        jsr     rparen          ; make not attempt to print a title.
        jsr     osnewl
        ldy     romid
        jmp     next

.prinfo lda     #'R'            ; print info for one ROM slot up to the
        jsr     oswrch          ; end of the service/lang flags.
        lda     #'o'
        jsr     oswrch
        lda     #'m'
        jsr     oswrch
        jsr     space
        lda     romid
        cmp     #&0A            ; decimal printing limited to a one or
        bcs     geten           ; zero as the tens column.
        lda     #'0'
        jsr     oswrch
        lda     romid
        jmp     both
.geten  lda     #'1'
        jsr     oswrch
        lda     romid
        sec
        sbc     #&0a
.both   and     #&0f            ; print the units digit.
        clc
        adc     #'0'
        jsr     oswrch
        jsr     space
        lda     #':'
        jsr     oswrch
        jsr     space
        lda     #'('
        jsr     oswrch
        txa                     ; get ROM type byte back.
        and     #&80
        beq     notsrv
        lda     #'S'
        bne     issrv
.notsrv lda     #' '
.issrv  jsr     oswrch
        txa
        and     #&40
        beq     space
        lda     #'L'
        bne     islng
.space  lda     #' '
.islng  jmp     oswrch

.prtitl jsr     space           ; print ROM title and version.  Stops when
        lda     #&09            ; the copyright pointer is reached.
        sta     &f6
        lda     #&80
        sta     &f7
.tloop  ldy     romid
        jsr     osrdrm
        cmp     #&00            ; convert zero bytes to space.
        bne     notnul
        lda     #' '
.notnul jsr     oswrch
        inc     &f6
        lda     &f6
        cmp     copywr          ; reached the copyright pointer?
        bne     tloop
        jmp     osnewl

.rparen lda     #'R'
        jsr     oswrch
.cparen lda     #')'
        jmp     oswrch
}

;;; Check that the destination ROM slot contains RAM and issue an
;;; error message if not.

.ramchk
{
        jsr     ramtst
        bne     notram
        rts
.notram jsr     errmsg
        equb    &80
        equs    "No RAM in that slot"
        equb    &00
}

;;; Convert one ASCII hex digit to binary.
;;; On exit C=0 if valid, C=1 if not.

.xd2bin
{
        cmp     #'0'
        bcc     fail
        cmp     #'9'+1
        bcc     deci
        cmp     #'A'
        bcc     fail
        and     #&df
        cmp     #'G'
        bcs     fail
        sbc     #&06
.deci   and     #&0f
        clc
        rts
.fail   sec
        rts
}

;;; Parse the filename for an SRLOAD/SRSAVE command.

.parse_fn
{
        lda     (&f2),y         ; check there is a filename!
        cmp     #' '
        beq     badfil
        cmp     #&0d
        beq     badfil
        tya                     ; fold Y into the base in &F2
        clc
        adc     &f2
        sta     osf_fn          ; and store the address, which is of the
        lda     #&00            ; filename, into the OSFILE block.
        adc     &f3
        sta     osf_fn+1
.loop1  iny                     ; skip over the characters of the filename.
        lda     (&f2),y
        cmp     #&0d
        beq     fnend
        cmp     #' '
        bne     loop1
.loop2  iny
        lda     (&f2),y         ; skip spaces
        cmp     #' '
        beq     loop2
        cmp     #'8'            ; beginning of 8000?
        bne     fnend
        iny
        lda     (&f2),y
        cmp     #' '
        beq     decend
        cmp     #&0d
        beq     decend
        cmp     #'0'
        bne     n8000
        lda     #'0'
.loop3  iny
        cmp     (&f2),y
        bne     n8000
        iny
        cmp     (&f2),y
        bne     n8000
        iny
.fnend  rts
.decend dey
        rts
.badfil jsr     errmsg
        equb    &80
        equs    "Missing filename"
        equb    &00
.n8000  jsr     errmsg
        equb    &80
        equs    "Only supported at 8000"
        equb    &00
}

;;; Parse a ROM ID.

.parse_rid_opt
{
.loop2  lda     (&f2),y         ; skip spaces between filename and ROM id.
        iny
        cmp     #' '
        beq     loop2
        cmp     #&0d
        beq     misid
        jsr     xd2bin          ; convert ROM id to binary.
        bcs     notdig
        sta     romid
        lda     (&f2),y         ; second digit?
        jsr     xd2bin
        bcs     onedig
        asl     romid           ; multiply first digit by 16
        asl     romid
        asl     romid
        asl     romid
        ora     romid           ; and add in second digit.
        sta     romid
.onedig lda     romid
        cmp     #&10            ; if the ROM ID is greater than 16 then it
        bcc     less16          ; is probably two digits in decimal but we
        sbc     #&06            ; converted as hex so subtract 6 and see if
        sta     romid           ; is now in range.
        cmp     #&10
        bcs     badid
.less16 rts
.notdig and     #&df
        cmp     #'M'
        bcc     badid
        eor     #&ff
        adc     #'Z'
        bmi     badid
        tax
        inx
        stx     pages
        ldy     &f4
        lda     &0df0,y
        sta     romid
.loop3  dec     romid
        bmi     notfnd
        jsr     ramtst
        bne     loop3
        dec     pages
        bne     loop3
        clc
        rts
.misid  sec
        rts
.badid  jsr     errmsg
        equb    &80
        equs    "Invalid ROM ID"
        equb    &00
}

.notfnd jsr     errmsg
        equb    &80
        equs    "RAM bank not found"
        equb    &00

.parse_rid
{
        jsr     parse_rid_opt
        bcs     misid
        rts
.misid  jsr     errmsg
        equb    &80
        equs    "Missing ROM ID"
        equb    &00
}

;;; Check there is buffer space in main RAM to hold a ROM image.

.bufchk
{
        lda     #&83            ; Get OSHWM.
        jsr     osbyte
        stx     oshwm
        sty     oshwm+1
        lda     #&84            ; Get HIMEM.
        jsr     osbyte
        stx     himem
        sty     himem+1
        tya                     ; Calculate HIMEM-OSHWM.
        clc
        sbc     oshwm+1
        cmp     #&40            ; 16k?
        bcc     newmod
        rts
.newmod lda     #&16            ; Not enough space so select mode 7
        jsr     oswrch          ; to make more room.
        lda     #&87
        jmp     oswrch
}

;;; The SRLOAD command.

.srload
{
        jsr     parse_fn        ; parse filename to OSFILE block.
        jsr     parse_rid_opt   ; parse ROM ID into ZP romid
        bcs     search
        jsr     ramchk          ; check there is RAM in the specified slot.
        jmp     both
.search ldy     &f4             ; no ROM ID specified on command line so
        lda     &0df0,y         ; search for the first slot which is...
        sta     romid
.loop   dec     romid
        bmi     notfnd
        jsr     ramtst          ; RAM
        bne     loop
        jsr     chkrom          ; and doesn't have a valid ROM image.
        bcc     loop
.both   jsr     bufchk          ; check there is memory for buffer.
        lda     oshwm           ; load ROM image at OSHWM.
        sta     osf_ld
        lda     oshwm+1
        sta     osf_ld+1
        lda     #&00            ; low byte of exec addr zero to have OSFILE
        sta     osf_ex          ; load the file at the address specified and
        sta     osf_ex+1        ; not its own load address.
        lda     #&ff
        jsr     fileop
        jsr     clrrtyp         ; stop serive calls to old image (if any).
        sta     &0df0,y         ; and forget previous workspace.
        ldx     oshwm+1         ; copy from OSHWM to &8000
        ldy     #&80            ; fall through into the copy routine.
}

;;; Routine to copy 16K from the page number in X to the page number in Y
;;; with the ROM specied in romid selected.

.copy16
{
base    =       &0102           ; where the main RAM copier will be.
        stx     srcpag
        sty     dstpag
        ldx     #cpyen-cpyst    ; copy the copy routine into place.
.cplp   lda     cpyst,X
        sta     base,x
        dex
        bpl     cplp
        lda     &f4             ; patch it with ROM
        sta     base+p_rom+1-cpyst
        lda     srcpag          ; source
        sta     base+p_src+2-cpyst
        lda     dstpag          ; and destination.
        sta     base+p_dst+2-cpyst
        lda     #&40            ; set the number of pages to copy.
        sta     pages
        ldy     #&00
        jmp     base            ; and go to the copy routine in main RAM.

;;; The following is copied into main RAM so as to have access to the
;;; source/destination ROM/RAM bank rather than the host ROM.  It is
;;; self-modifying code that is also patched by the setup above.

.cpyst  ldx     romid
        page_rom_x
.p_src  lda     &0000,y         ; self-modifying, initial value patched in.
.p_dst  sta     &0000,y         ; self-modifying, initial value patched in.
        iny
        bne     p_src
        inc     base+p_src+2-cpyst
        inc     base+p_dst+2-cpyst
        dec     pages
        bne     p_src
.p_rom  ldx     #&00            ; value patched in.
        page_rom_x
        rts
.cpyen
}

;;; The SRSAVE command.

.srsave
{
        jsr     parse_fn        ; parse filename to OSFILE block.
        jsr     parse_rid       ; parse ROM ID into ZP romid
        jsr     bufchk          ; check there is memory for buffer.
        ldx     #&80            ; copy from &8000 to OSHWM.
        ldy     oshwm+1
        jsr     copy16
        lda     #&00            ; set the load/exec addresses to be &8000
        sta     osf_ld
        sta     osf_ex
        lda     #&80
        sta     osf_ld+1
        sta     osf_ex+1
        lda     oshwm           ; set the "save from" address as OSHWM.
        sta     osf_st
        sta     osf_en
        lda     oshwm+1
        sta     osf_st+1
        clc                     ; add &40 pages to set the end address.
        adc     #&40
        sta     osf_en+1
        lda     #&00            ; select "save" and fall through.
}

;;; Perform a file operation with OSFILE with the upper words of the
;;; addresses in the control block set to &FFFF to indicate I/O processor.

.fileop
{
        ldx     #&ff            ; The function code is passed in in A so
        stx     osf_ld+2        ; use X as a temporary register instead.
        stx     osf_ld+3
        stx     osf_ex+2
        stx     osf_ex+3
        stx     osf_st+2
        stx     osf_st+3
        stx     osf_en+2
        stx     osf_en+3
        ldx     #<osf_fn        ; pass on to OSFILE.
        ldy     #>osf_fn
        jmp     osfile
}

;;; SRWIPE command

.srwipe
{
base    =       &0102           ; where the main RAM copier will be.
        jsr     parse_rid       ; parse ROM ID into ZP romid
        jsr     ramchk
        jsr     clrrtyp         ; stop serive calls to old image (if any).
        sta     &0df0,y         ; and forget previous workspace.
        ldx     #wipen-wipst    ; copy the copy routine into place.
.cplp   lda     wipst,X
        sta     base,x
        dex
        bpl     cplp
        lda     &f4             ; patch it with ROM
        sta     base+p_rom+1-wipst
        lda     #&40            ; set the number of pages to copy.
        sta     pages
        ldy     #&00
        jmp     base            ; and go to the clear routine in main RAM.

;;; The following is copied into main RAM so as to have access to the
;;; source/destination ROM/RAM bank rather than the host ROM.  It is
;;; self-modifying code that is also patched by the setup above.

.wipst  ldx     romid
        page_rom_x
        lda     #&00
.p_dst  sta     &8000,y         ; self-modifying, initial value patched in.
        iny
        bne     p_dst
        inc     base+p_dst+2-wipst
        dec     pages
        bne     p_dst
.p_rom  ldx     #&00            ; value patched in.
        page_rom_x
        rts
.wipen
}

;;; INSERT command.

.insert
{
        jsr     parse_rid       ; parse ROM ID into ZP romid
        jsr     chkrom          ; check if valid ROM image.
        bcs     badrom
        pha
        jsr     getrtb          ; get OS ROM table.
        pla
        ldy     romid
        sta     (romtab),y
        rts
.badrom jsr     errmsg
        equs    "Not a valid ROM image"
        equb    &00
}

.unplug
{
        jsr     parse_rid       ; parse ROM ID into ZP romid
        ;; fall through to clear the ROM type byte.
}

;;; Clear the ROM type bye from the ROM table.
;;; Rom ID of ROM to clear byte from iz in ZP at romid.

.clrrtyp
{
        jsr     getrtb          ; get OS ROM table.
        ldy     romid           ; clear the ROM type byte to prevent
        lda     #&00            ; service calls.
        sta     (romtab),y
        rts
}

IF _SRAM_WP_
base    =       &0102           ; where the main RAM copier will be.
.wpset  jsr     parse_rid       ; parse ROM ID into ZP romid
        lda     #&20
        bne     wpcom
.wpclr  jsr     parse_rid       ; parse ROM ID into ZP romid
        lda     #&30
.wpcom  ora     romid
        tay
        ldx     #wpcen-wpcst    ; copy the copy routine into place.
.wpclp  lda     wpcst,X
        sta     base,x
        dex
        bpl     wpclp
        ldx     &f4
        jmp     base
.wpcst  sty     &f4
        sty     &fe30
        stx     &f4
        stx     &fe30
        rts
.wpcen
ENDIF
