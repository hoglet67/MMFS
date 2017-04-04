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

.start  equs    "MRB"
        jmp     serv
        equb    %10000010
        equb    copyr-&8000
        equb    &01
        equs    "MMFS ROM/RAM"
        equb    &00
        equs    "1.0"
.copyr  equb    &00
        equs    "(C) Martin Mathers, David Banks, Steven Fosdick"
        equb    &00

.serv   cmp     #&01
        beq     absws
        rts

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
.common lda     #&84            ; get HIMEM
        jsr     osbyte
        txa
        sec                     ; subtract the size of the copy routine.
        sbc     cplen
        sta     cpdst
        tya
        sbc     #&00
        sta     cpdst+1
        ldy     cplen
        dey
.loop1  lda     (cpsrc),Y       ; copy the copy routine into main ram.
        sta     (cpdst),Y
        dey
        bpl     loop1
        lda     #<romst         ; now set the embedded MMFS ROM as the
        sta     cpsrc           ; source of the copy.
        lda     #>romst
        sta     cpsrc+1
        lda     &f4             ; including the source ROM - this one.
        sta     ourrom
        jmp     (cpdst)         ; points to the RAM copy of the copy routine.

        ;;  The following is copied into RAM.  It is relocatable code
        ;;  so there is no need to change it on the fly.

        macro   csetup
        ldx     #&0f            ; find the highest free sideways RAM slot.
.romlp
        cpx     ourrom          ; don't write to ourrom in case it's FLASH
        beq     romnxt
        
        stx     &f4
        stx     &fe30
        lda     &8006
        inc     &8006
        cmp     &8006
        bne     gotram
.romnxt        
        dex
        bpl     romlp
        ldx     ourrom
        stx     &f4
        stx     &fe30
        jmp     noram
.gotram dec     &8006
        stx     dstrom
        lda     #&00            ; set copy destination as the start of
        sta     cpdst           ; the sideways RAM bank.
        lda     #&80
        sta     cpdst+1
        ldy     #&00
        endmacro

.copyst csetup
.copylp ldx     ourrom
        stx     &f4
        stx     &fe30
        lda     (cpsrc),Y
        ldx     dstrom          ; this is the destination RAM bank.
        stx     &f4
        stx     &fe30
        sta     (cpdst),Y
        iny
        bne     copylp
        inc     cpsrc+1
        inc     cpdst+1
        lda     cpdst+1
        cmp     #&b6            ; the high byte of the highest address used
        bne     copylp          ; by the MMFS ROM in its final place.
        lda     #&aa            ; Find the ROM info table.
        ldx     #&00
        ldy     #&ff
        jsr     osbyte
        stx     cpdst
        sty     cpdst+1
        lda     romst+6         ; copy the MMFS ROM type into the table
        ldy     dstrom          ; entry for the RAM copy.
        sta     (cpdst),Y
        pla
        tay
        lda     #&01            ; and dont "claim" this call - others ROMS
        rts                     ; can claim workspace (we don't).
.copyen

.cmpst  csetup
.cmplp  ldx     ourrom
        stx     &f4
        stx     &fe30
        lda     (cpsrc),Y
        ldx     dstrom          ; this is the destination RAM bank.
        stx     &f4
        stx     &fe30
        cmp     (cpdst),Y
        bne     cmpfai
        iny
        bne     cmplp
        inc     cpsrc+1
        inc     cpdst+1
        lda     cpdst+1
        cmp     #&b6            ; the high byte of the highest address used
        bne     cmplp           ; by the MMFS ROM in its final place.
        pla
        tay
        lda     #&01            ; and dont "claim" this call - others ROMS
        rts                     ; can claim workspace (we don't).
.cmpfai lda     ourrom
        sta     &f4
        sta     &fe30
        jmp     romfai
.cmpen

.romfai clc
        tya
        adc     cpdst
        sta     cpdst
        lda     cpdst+1
        adc     #&00
        sta     cpdst+1
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
        rts                     ; can claim workspace (we don't).

.normsg equs    "No sideways RAM found for MMFS"
        equb    &0d,&0a,&00

        
