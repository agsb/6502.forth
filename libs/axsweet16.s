;
; 03/10/2023    agsb@ adapted for ca65
;
;   The source code used here comes from SW16RB.TXT,
;   presumed to be the original listing.
;
;********************************
;                               *
;    apple-ii  pseudo machine   *
;          interpreter          *
;                               *
;       copyright (c) 1977      *
;      apple computer,  inc     *
;                               *
;      all  rights reserved     *
;                               *
;          s. wozniak           *
;                               *
;********************************
;                               *
;  title:  sweet 16 interpreter *
;                               *
;********************************

r0l     =  $0
r0h     =  $1
r14h    =  $1d
r15l    =  $1e
r15h    =  $1f


.org  $f689

;
; only Woz knows why 32 asterisks :)
;        ast  32
sweet16:

@sw16a:
        jsr  @save           ;preserve 6502 reg contents
        pla
        sta  r15l           ;init sweet16 pc
        pla                 ;from return
        sta  r15h           ;address
@sw16b:   
        jsr  @sw16c          ;interpret and execute
        jmp  @sw16b          ;one sweet16 instr.
@sw16c:   
        inc  r15l
        bne  @sw16d          ;incr sweet16 pc for fetch
        inc  r15h
@sw16d:   
        lda  >set           ;common high byte for all routines
        pha                 ;push on stack for rts
        ldy  #$0
        lda  (r15l),y       ;fetch instr
        and  #$f            ;mask reg specification
        asl                 ;double for two byte registers
        tax                 ;to x reg for indexing
        lsr
        eor  (r15l),y       ;now have opcode
        beq  @tobr           ;if zero then non-reg op
        stx  r14h           ;indicate "prior result reg"
        lsr
        lsr                 ;opcode*2 to lsb's
        lsr
        tay                 ;to y reg for indexing
        lda  optbl-2,y      ;low order adr byte
        pha                 ;onto stack
        rts                 ;goto reg-op routine
@tobr:    
        inc  r15l
        bne  @tobr2          ;incr pc
        inc  r15h
@tobr2:   
        lda  brtbl,x        ;low order adr byte
        pha                 ;onto stack for non-reg op
        lda  r14h           ;"prior result reg" index
        lsr                 ;prepare carry for bc, bnc.
        rts                 ;goto non-reg op routine
@rtnz:    
        pla                 ;pop return address
        pla
        jsr  @load           ;restore 6502 reg contents
        jmp  (r15l)         ;return to 6502 code via pc
@setz:    
        lda  (r15l),y       ;high order byte of constant
        sta  r0h,x
        dey
        lda  (r15l),y       ;low order byte of constant
        sta  r0l,x
        tya                 ;y reg contains 1
        sec
        adc  r15l           ;add 2 to pc
        sta  r15l
        bcc  @set2
        inc  r15h
@set2:    
        rts
@optbl:   
        .byte  @set-1          ;1x
@brtbl:   
        .byte  @rtn-1          ;0
        .byte  @ld-1           ;2x
        .byte  @br-1           ;1
        .byte  @st-1           ;3x
        .byte  @bnc-1          ;2
        .byte  @ldat-1         ;4x
        .byte  @bc-1           ;3
        .byte  @stat-1         ;5x
        .byte  @bp-1           ;4
        .byte  @lddat-1        ;6x
        .byte  @bm-1           ;5
        .byte  @stdat-1        ;7x
        .byte  @bz-1           ;6
        .byte  @pop-1          ;8x
        .byte  @bnz-1          ;7
        .byte  @stpat-1        ;9x
        .byte  @bm1-1          ;8
        .byte  @add-1          ;ax
        .byte  @bnm1-1         ;9
        .byte  @sub-1          ;bx
        .byte  @bk-1           ;a
        .byte  @popd-1         ;cx
        .byte  @rs-1           ;b
        .byte  @cpr-1          ;dx
        .byte  @bs-1           ;c
        .byte  @inr-1          ;ex
        .byte  @nul-1          ;d
        .byte  @dcr-1          ;fx
        .byte  @nul-1          ;e
        .byte  @nul-1          ;unused
        .byte  @nul-1          ;f

;  following code must be
;  contained on a single page!

.align $100

@set:     
        bpl  @setz           ;always taken
@ld:      
        lda  r0l,x
@bk:      
        equ  *-1
        sta  r0l
        lda  r0h,x          ;move rx to r0
        sta  r0h
        rts
@st:      
        lda  r0l
        sta  r0l,x          ;move r0 to rx
        lda  r0h
        sta  r0h,x
        rts
@stat:    
        lda  r0l
@stat2:   
        sta  (r0l,x)        ;store byte indirect
        ldy  #$0
@stat3:   
        sty  r14h           ;indicate r0 is result neg
@inr:     
        inc  r0l,x
        bne  @inr2           ;incr rx
        inc  r0h,x
@inr2:    
        rts
@ldat:    
        lda  (r0l,x)        ;load indirect (rx)
        sta  r0l            ;to r0
        ldy  #$0
        sty  r0h            ;zero high order r0 byte
        beq  @stat3          ;always taken
@pop:     
        ldy  #$0            ;high order byte = 0
        beq  @pop2           ;always taken
@popd:    
        jsr  @dcr            ;decr rx
        lda  (r0l,x)        ;pop high order byte @rx
        tay                 ;save in y reg
@pop2:    
        jsr  @dcr            ;decr rx
        lda  (r0l,x)        ;low order byte
        sta  r0l            ;to r0
        sty  r0h
@pop3:    
        ldy  #$0             ;indicate r0 as last result reg
        sty  r14h
        rts
@lddat:   
        jsr  @ldat           ;low order byte to r0, incr rx
        lda  (r0l,x)        ;high order byte to r0
        sta  r0h
        jmp  @inr            ;incr rx
@stdat:   
        jsr  @stat           ;store indirect low order
        lda  r0h            ;byte and incr rx. then
        sta  (r0l,x)        ;store high order byte.
        jmp  @inr            ;incr rx and return
@stpat:   
        jsr  @dcr            ;decr rx
        lda  r0l
        sta  (r0l,x)        ;store r0 low byte @rx
        jmp  @pop3           ;indicate r0 as last result reg
@dcr:     
        lda  r0l,x
        bne  @dcr2           ;decr rx
        dec  r0h,x
@dcr2:    
        dec  r0l,x
        rts
@sub:     
        ldy  #$0            ;result to r0
@cpr:     
        sec                 ;note y reg = 13*2 for cpr
        lda  r0l
        sbc  r0l,x
        sta  r0l,y          ;r0-rx to ry
        lda  r0h
        sbc  r0h,x
@sub2:    
        sta  r0h,y
        tya                 ;last result reg*2
        adc  #$0            ;carry to lsb
        sta  r14h
        rts
@add:     
        lda  r0l
        adc  r0l,x
        sta  r0l            ;r0+rx to r0
        lda  r0h
        adc  r0h,x
        ldy  #$0            ;r0 for result
        beq  sub2           ;finish add
@bs:      
        lda  r15l           ;note x reg is 12*2!
        jsr  @stat2          ;push low pc byte via r12
        lda  r15h
        jsr  @stat2          ;push high order pc byte
@br:      
        clc
@bnc:     
        bcs  @bnc2           ;no carry test
@br1:     
        lda  (r15l),y       ;displacement byte
        bpl  br2
        dey
@br2:     
        adc  r15l           ;add to pc
        sta  r15l
        tya
        adc  r15h
        sta  r15h
@bnc2:    
        rts
@bc:      
        bcs  @br
        rts
@bp:      
        asl                 ;double result-reg index
        tax                 ;to x reg for indexing
        lda  r0h,x          ;test for plus
        bpl  @br1            ;branch if so
        rts
@bm:      
        asl                 ;double result-reg index
        tax
        lda  r0h,x          ;test for minus
        bmi  @br1
        rts
@bz:      
        asl                 ;double result-reg index
        tax
        lda  r0l,x          ;test for zero
        ora  r0h,x          ;(both bytes)
        beq  @br1            ;branch if so
        rts
@bnz:     
        asl                 ;double result-reg index
        tax
        lda  r0l,x          ;test for non-zero
        ora  r0h,x          ;(both bytes)
        bne  @br1            ;branch if so
        rts
@bm1:     
        asl                 ;double result-reg index
        tax
        lda  r0l,x          ;check both bytes
        and  r0h,x          ;for $ff (minus 1)
        eor  #$ff
        beq  @br1            ;branch if so
        rts
@bnm1:    
        asl                 ;double result-reg index
        tax
        lda  r0l,x
        and  r0h,x          ;check both bytes for no $ff
        eor  #$ff
        bne  @br1            ;branch if not minus 1
@nul:     
        rts
@rs:      
        ldx  #$18           ;12*2 for r12 as stack pointer
        jsr  @dcr            ;decr stack pointer
        lda  (r0l,x)        ;pop high return address to pc
        sta  r15h
        jsr  @dcr            ;same for low order byte
        lda  (r0l,x)
        sta  r15l
        rts
@rtn:     
        jmp  @rtnz

@save:   
        sta accm
        stx xreg
        sty yreg
        php
        pla
        sta stats
        cld
        rts

@load:   
        lda stats
        pha
        lda accm
        ldx xreg
        ldy yreg
        plp
        rts

