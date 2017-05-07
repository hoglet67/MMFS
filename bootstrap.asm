osbyte  =       &fff4
oswrch  =       &ffee
osnewl  =       &ffe7

        org     &a0
.cpsrc  equw    &0000
.cpdst  equw    &0000
.cplen  equb    &00
.ourrom equb    &00
.dstrom equb    &00

        org     &8000

; The included MMFS ROM is added to &4000 bytes (&3600 code, &A00 workspace)
; The guard is &CA00 as the workspace is allowed to "overflow"
        guard   &CA00

; A fixed buffer is now used for the copying code, as it's now self modifying
; and thus contains absolute addresses.

; Page &09 (9) - Envelope, Serial output, Speech, transient command buffer
; ========================================================================
; &0900-&09BF RS423 output buffer
; &09C0-&09FF Speech buffer
; &0900-&09FF CFS/RFS BPUT sequential output buffer (nb, *not* used by SAVE)
; &0900-&09BF ENVELOPES 5 to 16
;
; this area is reasonable safe to use in the reset sequence

code_buffer = &0900

; Page &0A (10) - Serial input, transient command buffer
; ======================================================
; &0A00-&0AFF Cassette/RS423 input buffer (nb: *not* used by LOAD)
;
; this area is reasonable safe to use in the reset sequence

copy_buffer = &0a00

; Macros to page in the ROM in the X register
;
; The Electron version is compicated by the need to
; do a double-write if the current ROM is 0x8-0xB
;
; As these slots correspond to Basic and the Keyboard
; they will not be used in the copy. So a faster version
; of the macro is used there.
;
; On the Beeb both versions are the same
IF _ELECTRON_
        MACRO  page_rom_x
        pha
        lda    #&0F
        sta    &f4
        sta    &fe05
        pla
        stx    &f4
        stx    &fe05
        ENDMACRO
        MACRO  page_rom_x_fast
        stx    &f4
        stx    &fe05
        ENDMACRO
ELSE
        MACRO  page_rom_x
        stx    &f4
        stx    &fe30
        ENDMACRO
        MACRO  page_rom_x_fast
        page_rom_x
        ENDMACRO
ENDIF

.start  equs    "MRB"
        jmp     serv
        equb    %10000010
        equb    copyr-&8000
        equb    &02
.title  equs    "MMFS Bootstrap"
        equb    &00
        equs    "1.2"
.copyr  equb    &00
        equs    "(C) Martin Mathers, David Banks, Steven Fosdick"
        equb    &00

.serv   cmp     #&01
        beq     absws
        cmp     #&04
        bne     notcmd
        jmp     oscmd
.notcmd cmp     #&09
        bne     nothlp
        jmp     help
.nothlp rts

.absws  tya
        pha
        lda     #&fd            ; get the last BREAK type.
        ldx     #&00
        ldy     #&ff
        jsr     osbyte
        cpx     #&00
        beq     docmp
.docopy lda     #<copyst
        sta     cpsrc
        lda     #>copyst
        sta     cpsrc+1
        lda     #copyen-copyst
        sta     cplen
        bne     common
.docmp  lda     #<cmpst
        sta     cpsrc
        lda     #>cmpst
        sta     cpsrc+1
        lda     #cmpen-cmpst
        sta     cplen
.common
        ldy     #0
.loop1  lda     (cpsrc),Y       ; copy the copy routine into main ram.
        sta     code_buffer, Y
        iny
        cpy     cplen
        bne     loop1
        lda     &f4             ; including the source ROM - this one.
        sta     ourrom
        jmp     code_buffer     ; points to the RAM copy of the copy routine.

.romfai
        ldx     #&00
        lda     faimsg
.failp  jsr     oswrch
        inx
        lda     faimsg,x
        bne     failp
        lda     cpdst+1
        jsr     hexbyt
        lda     cpdst
        jsr     hexbyt
        jsr     osnewl
        jsr     osnewl
        jmp     docopy

.hexbyt pha
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        jsr     hexnyb
        pla
.hexnyb and     #&0f
        cmp     #&0a
        bcc     lten
        adc     #&06
.lten   adc     #'0'
        jmp     oswrch

.faimsg equs    "MMFS RAM/ROM comparison failed at "
        equb    &00

.noram  ldx     #&00
        lda     normsg
.norlp  jsr     oswrch
        inx
        lda     normsg,x
        bne     norlp
        pla
        tay
        lda     #&01            ; and dont "claim" this call - others ROMS
        ldx     ourrom          ; restore the current ROM number in X
        rts                     ; can claim workspace (we don't).

.normsg equs    "No sideways RAM found for MMFS"
        equb    &0d,&0a,&00


        ;;  The following is copied into RAM.  It is relocatable code
        ;;  so there is no need to change it on the fly.

        MACRO   csetup mode
.base
        ldx     #&0f            ; find the highest free sideways RAM slot.
.romlp
        cpx     ourrom          ; don't write to ourrom in case it's FLASH
        beq     romnxt

        page_rom_x
        lda     &8006
        eor     #&FF
        sta     &8006
        cmp     &8006
        php
        eor     #&FF
        sta     &8006
        plp
        beq     testdone
.romnxt
        dex
        bpl     romlp

.testdone
        stx     dstrom
        ldx     ourrom          ; page back in the source ROM
        page_rom_x
.wait
        lda     &8000           ; some FLASH devices vanish for a while after being written to
        cmp     #'M'            ; MRB is a signature for for the source ROM
        bne     wait
        lda     &8001
        cmp     #'R'
        bne     wait
        lda     &8002
        cmp     #'B'
        bne     wait

        ldx     dstrom          ; X=FF if no RAM found
        bpl     gotram
        jmp     noram

.gotram
        lda     #>romst         ; set the embedded MMFS ROM as thes ource of the copy
        sta     code_buffer + patch1 + 2 - base
        sta     code_buffer + patch2 + 2 - base
        lda     #&80            ; set copy destination as the start the sideways RAM bank
        sta     code_buffer + patch3 + 2 - base
        sta     code_buffer + patch4 + 2 - base
.cploop
        ldx     ourrom
        page_rom_x_fast
        ldy     #&7f
.cploop1
.patch1
        lda     romst, y            ; 4
        sta     copy_buffer, y      ; 5
.patch2
        lda     romst + &80, y      ; 4
        sta     copy_buffer + &80,y ; 5
        dey                         ; 2
        bpl     cploop1             ; 3
        ldx     dstrom
        page_rom_x_fast
        ldy     #&7f
.cploop2
        lda     copy_buffer, y      ; 4
.patch3
IF mode=1
        cmp     &8000, y            ; 4
        bne     fail1               ; 2
ELSE
        sta     &8000, y            ; 5
ENDIF
        lda     copy_buffer + &80,y ; 4
.patch4
IF mode=1
        cmp     &8080, y            ; 4
        bne     fail2               ; 2
ELSE
        sta     &8080, y            ; 5
ENDIF
        dey                         ; 2
        bpl     cploop2             ; 3
        inc     code_buffer + patch1 + 2 - base
        inc     code_buffer + patch2 + 2 - base
        inc     code_buffer + patch3 + 2 - base
        inc     code_buffer + patch4 + 2 - base
        lda     code_buffer + patch4 + 2 - base
        cmp     #&b6
        bne     cploop
IF mode=1
        beq     exit            ; success: exit with Z=0
.fail2
        tya
        ora     #&80
        tay
.fail1
        sty     cpdst
        lda     code_buffer + patch4 + 2 - base
        sta     cpdst + 1       ; fail: exit with Z=1
ENDIF
.exit
        ENDMACRO

align   &100

.copyst csetup  0
        lda     #&aa            ; Find the ROM info table.
        ldx     #&00
        ldy     #&ff
        jsr     osbyte
        stx     cpdst
        sty     cpdst+1
        lda     &8006           ; copy the MMFS ROM type into the table
        ldy     dstrom          ; entry for the RAM copy.
        sta     (cpdst),Y
        tya                     ; record the number of the ROM bank containing
        ldy     ourrom          ; the RAM copy in the byte normally used for
        sta     &0df0,y         ; recording the start page of private worksapce.
        pla
        tay
        lda     #&01            ; and dont "claim" this call - others ROMS
        ldx     ourrom          ; restore the current ROM number in X
        rts                     ; can claim workspace (we don't).
.copyen

        align   &100

.cmpst  csetup  1
        bne     cmpfai
        lda     dstrom          ; record the number of the ROM bank containing
        ldy     ourrom          ; the RAM copy in the byte normally used for
        sta     &0df0,y         ; recording the start page of private worksapce.
        pla
        tay
        lda     #&01            ; and dont "claim" this call - others ROMS
        ldx     ourrom          ; restore the current ROM number in X
        rts                     ; can claim workspace (we don't).
.cmpfai ldx     ourrom
        page_rom_x
        jmp     romfai
.cmpen

.bootend        
        include "sram.asm"
        
; Page align the included MMFS ROM which will follow
; This avoids any penalties with page crossing during the copy
        align   &100
