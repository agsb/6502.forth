; vim: filetype=asm sw=4 ts=4 autoindent expandtab shiftwidth=4 et:
;----------------------------------------------------------------------
; Copyright (c) 2023, Alvaro Gomes Sobral Barcellos
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 
; 1. Redistributions of source code must retain the above copyright 
;    notice, this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in 
;    the documentation and/or other materials provided with the 
;    distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES, LOSS
; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED 
; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
; POSSIBILITY OF SUCH DAMAGE.
;---------------------------------------------------------------------

;--------------------------------------------------------
;
;  ca65 assembler specifics
;
;--------------------------------------------------------

; identifiers

.case +

; enable features

.feature c_comments

.feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment

; enable 6502 mode

.p02

;---------------------------------------------------------------------
;    constants
FALSE = 0
TRUE = 1

STACKSIZE = $30

HALFSTK =   $30 / 2

TERMINAL  = $100

;---------------------------------------------------------------------
;   using absolute memory indirect references
;
;   .byte idx
;   .byte idy
;   .word spt
;   .word rpt
;   .word ipt
;   .word dpt
;   .word tos
;   .word usr
;
;---------------------------------------------------------------------
; Forth like functions
; to keep code safe by not using "fall throught".
; uses A, Y, X caller must saves.
; needs 2 levels of hardware stack
; uses 4 bytes in page zero as temporary, TOS and NOS
; uses 6 bytes in memory for internal use
;---------------------------------------------------------------------
.segment "ZERO"

* = $F0

; forth sp index
six:    .byte $0
; forth rp index
rix:    .byte $0
; data stack base pointer
spt:    .word $0
; return stack base pointer
rpt:    .word $0
; dictionary next free cell pointer
dpt:    .word $0
; instruction pointer
ipt:    .word $0

; registers

wrk:    .word $0
tos:    .word $0
nos:    .word $0
; tmp:    .word $0

;---------------------------------------------------------------------
.segment "CODE"

tib:
.res TERMINAL, $0

; could be at page zero, less code, less cycles

.res STACKSIZE, $0
dat_zero: .word $0

.res STACKSIZE, $0
ret_zero: .word $0

;---------------------------------------------------------------------
.segment "ONCE"

;---------------------------------------------------------------------
;   data stack stuff

keep_: ; to push
    ; ldx six
    dex
    dex
    stx six
    rts

lose_: ; to pull
    ; ldx six
    inx
    inx
    stx six
    rts

spush:
push_:
    ldx six
    lda tos + 0
    sta dat_zero - 2, x
    lda tos + 1
    sta dat_zero - 1, x
    jmp keep_

spull:
pull_:
    ldx six
    lda dat_zero + 0, x
    sta tos + 0
    lda dat_zero + 1, x
    sta tos + 1
    jmp lose_

push2_:
    ldx six
    lda nos + 0
    sta dat_zero - 4, x
    lda nos + 1
    sta dat_zero - 3, x
    lda tos + 0
    sta dat_zero - 2, x
    lda tos + 1
    sta dat_zero - 1, x
    jsr keep_
    jmp keep_

pull2_:
    ldx six
    lda dat_zero + 0, x
    sta tos + 0
    lda dat_zero + 1, x
    sta tos + 1
    lda dat_zero + 2, x
    sta nos + 0
    lda dat_zero + 3, x
    sta nos + 1
    jsr lose_
    jmp lose_

drop_:
    ldx six
    jsr lose_
    ; rts
    jmp link_

dup_:
    ldx six
    lda dat_zero + 0, x
    sta dat_zero - 2
    lda dat_zero + 1, x
    sta dat_zero - 1
    jsr keep_
    ; rts
    jmp link_

over_:
    ldx six
    lda dat_zero + 2, x
    sta dat_zero - 2
    lda dat_zero + 3, x
    sta dat_zero - 1
    jsr keep_
    ; rts
    jmp link_

swap_:
    ldx six
    lda dat_zero + 0, x
    sta dat_zero - 2
    lda dat_zero + 1, x
    sta dat_zero - 1
    lda dat_zero + 2, x
    sta dat_zero + 0
    lda dat_zero + 3, x
    sta dat_zero + 1
    lda dat_zero - 2, x
    sta dat_zero + 2
    lda dat_zero - 1, x
    sta dat_zero + 3
    ; rts
    jmp link_

rot_:
    ldx six
    lda dat_zero + 4, x
    sta dat_zero - 2
    lda dat_zero + 5, x
    sta dat_zero - 1
    lda dat_zero + 2, x
    sta dat_zero + 4
    lda dat_zero + 3, x
    sta dat_zero + 5
    lda dat_zero + 0, x
    sta dat_zero + 2
    lda dat_zero + 1, x
    sta dat_zero + 3
    lda dat_zero - 2, x
    sta dat_zero + 0
    lda dat_zero - 1, x
    sta dat_zero + 1
    ; rts
    jmp link_

and_:
    ldx six
    lda dat_zero + 0, x
    and dat_zero + 2, x
    sta dat_zero + 2, x
    lda dat_zero + 1, x
    and dat_zero + 3, x
    sta dat_zero + 3, x
    jmp drop_

or_:
    ldx six
    lda dat_zero + 0, x
    ora dat_zero + 2, x
    sta dat_zero + 2, x
    lda dat_zero + 1, x
    ora dat_zero + 3, x
    sta dat_zero + 3, x
    jmp drop_

xor_:
    ldx six
    lda dat_zero + 0, x
    eor dat_zero + 2, x
    sta dat_zero + 2, x
    lda dat_zero + 1, x
    eor dat_zero + 3, x
    sta dat_zero + 3, x
    jmp drop_

cpt_:
    ldx six
    sec
    tya
    sbc dat_zero + 0, x
    sta dat_zero + 0, x
    sec
    tya
    sbc dat_zero + 1, x
    sta dat_zero + 1, x
    ; rts
    jmp link_

neg_:
    lda #$00
    tay
    jmp cpt_

inv_:
    lda #$FF
    tay
    jmp cpt_

sub_:
    ldx six
    sec
    lda dat_zero + 2, x
    sbc dat_zero + 0, x
    sta dat_zero + 2, x
    lda dat_zero + 3, x
    sbc dat_zero + 1, x
    sta dat_zero + 3, x
    jmp drop_

add_:
    ldx six
    clc
    lda dat_zero + 2, x
    adc dat_zero + 0, x
    sta dat_zero + 2, x
    lda dat_zero + 3, x
    adc dat_zero + 1, x
    sta dat_zero + 3, x
    jmp drop_

cmp_:
    ldx six
    sec
    lda dat_zero + 2, x
    sbc dat_zero + 0, x
    lda dat_zero + 3, x
    sbc dat_zero + 1, x
    rts

eq_:
    jsr cmp_
    beq true2_
    bne false2_

lt_:
    jsr cmp_
    bmi true2_
    bpl false2_

gt_:
    jsr cmp_
    bmi false2_
    beq false2_
    bpl true2_

same2_:
    ldx six
    sta dat_zero + 2, x
    sta dat_zero + 3, x
    jmp drop_

false2_:
    lda #(FALSE)
    beq same2_

true2_:
    lda #(TRUE)
    bne same2_

shl_:
    ldx six
    asl dat_zero + 0, x
    rol dat_zero + 1, x
    ; rts
    jmp link_

shr_:
    ldx six
    lsr dat_zero + 0, x
    ror dat_zero + 1, x
    ; rts
    jmp link_

cto_:
    jsr pull2_
    ldy #0
    lda nos + 0
    sta (tos), y
    rts

to_:
    jsr cto_
    iny
    lda nos + 1
    sta (tos), y
    rts

cStore_:
    jsr cto_
    ; rts
    jmp link_

store_:
    jsr to_
    ; rts
    jmp link_

cat_:
    ldx six
    lda dat_zero + 0, x
    sta tos + 0
    lda dat_zero + 1, x
    sta tos + 1
    ldy #0
    lda (tos), y
    sta dat_zero + 0, x
    rts

at_:
    jsr cat_
    iny
    lda (tos), y
    sta dat_zero + 1, x
    rts

cFetch_:
    jsr cat_
    ; rts
    jmp link_

fetch_:
    jsr at_
    ; rts
    jmp link_

incr_:
    ldx six
    inc dat_zero + 0, x
    bne @ends
    inc dat_zero + 1, x
@ends:
    ; rts
    jmp link_

decr_:
    ldx six
    lda dat_zero + 0, x
    bne @ends
    dec dat_zero + 1, x
@ends:
    dec dat_zero + 0, x
    ; rts
    jmp link_

goto_:
    ldx six
    lda dat_zero + 1,x
    pha
    lda dat_zero + 0,x
    pha
    php
    rti

addto_:
    jsr pull2_
    ldy #0
    clc
    lda (tos), y
    adc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    adc nos + 1
    sta (tos), y
    ; rts
	jmp link_

subto_:
    jsr pull2_
    ldy #0
    sec
    lda (tos), y
    sbc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    sbc nos + 1
    sta (tos), y
    ; rts
	jmp link_

;----------------------------------------------------------------------
;   return stack stuff

rpush:
rpush_:
    ldx rix
    lda tos + 0
    sta ret_zero - 2, x
    lda tos + 1
    sta ret_zero - 1, x
    dex
    dex
    stx rix
    rts

rpull:
rpull_:
    ldx rix
    lda ret_zero + 0, x
    sta tos + 0
    lda ret_zero + 1, x
    sta tos + 1
    inx
    inx
    stx rix
    rts

;----------------------------------------------------------------------

rshow_:
    ldx rix
    lda ret_zero + 0, x
    sta tos + 0
    lda ret_zero + 1, x
    sta tos + 1
    jsr push_
    ; rts
    jmp link_

r2d_:
    jsr rpull_
    jsr push_
    ; rts
    jmp link_

d2r_:
    jsr pull_
    jsr rpush_
    ; rts
    jmp link_

stkis_:
    sta tos + 0
    lda #0
    sta tos + 1
    jsr spush
    ; rts
    jmp link_

dat2t_:
    lda six
    bcc stkis_

ret2t_:
    lda rix
    bcc stkis_

t2dat_:
    jsr spull
    lda tos + 0
    sta six
    ; rts
    jmp link_

t2ret_:
    jsr spull
    lda tos + 0
    sta rix
    ; rts
    jmp link_

;----------------------------------------------------------------------

;----------------------------------------------------------------------
; prepare for mult or divd
opin:
    ldx six
    ; pseudo tos
    lda dat_zero + 0, x
    sta wrk + 0
    lda dat_zero + 1, x
    sta wrk + 1
    ; pseudo nos
    lda dat_zero + 2, x
    sta tmp + 0
    lda dat_zero + 3, x
    sta tmp + 1
    ; clear results
    lda #0
    sta tos + 0
    sta tos + 1
    sta nos + 0
    sta nos + 1
    ; countdown
    ldy #16
    rts

;----------------------------------------------------------------------
; resume from mult or divd
opout:
    ; copy results
    ldx six
    lda nos + 0
    sta dat_zero + 0, x
    lda nos + 1
    sta dat_zero + 1, x
    lda tos + 0
    sta dat_zero + 2, x
    lda tos + 1
    sta dat_zero + 3, x
    ; rts
    jmp link_

;----------------------------------------------------------------------
; Divide the top 2 cell of the stack
; http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
; dividend divisor -- result remainder
; ( tmp wrk -- nos tos )
div_:
    jsr opin
@loop:
    asl tmp + 0
    rol tmp + 1
    rol tos + 0
    rol tos + 1
    sec
    lda tos + 0
    sbc wrk + 0
    tax
    lda tos + 1
    sbc wrk + 1
    bcc @skip
    sta tos + 1
    stx tos + 0
    inc tmp + 0
@skip:
    ; countdown
    dey
    bne @loop
    ; results
    lda tmp + 0
    sta nos + 0
    lda tmp + 1
    sta nos + 1
    ; ends
    jmp opout

;----------------------------------------------------------------------
; 16-bit multiply 16x16, 32 result
; http://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
; ( multiplicand multiplier -- resultMSW resultLSW )
; ( tmp wrk -- nos tos )
mul_:
    jsr opin
@shift_r:
    ; divide by 2
    lsr wrk + 1
    ror wrk + 0
    bcc @rotate_r
    ; add multiplicand to upper half product
    tax
    clc
    lda tmp + 0
    adc tos + 0
    sta tos + 0
    txa
    adc tmp + 1
@rotate_r:
    ; rotate partial product upper to low
    ror
    ror tos + 1
    ror nos + 1
    ror nos + 0
    ; countdown
    dey
    bne @shift_r
    sta tos + 0
    ; ends
    jmp opout


;----------------------------------------------------------------------
;
;   Forth stuff:
;   ATT: KEEP THE WORDS AT BRANCH OFFSETS (-127 to +127) or COLAPSE
;
; tos and nos are NOT keeped, all operations are by offsets
; ipt MUST be preserved and reserved for those routines
;
; HEADER "ENDS", "ENDS", F_LEAP, LEAF
unnest_:  ; aka semis:
    ; pull from return stack
    ldx rix
    lda ret_zero + 0, x
    sta ipt + 0
    lda ret_zero + 1, x
    sta ipt + 1
    inx
    inx
    stx rix

    ; as is, Minimal Thread Code 6502
next_:
    ldy #0
    lda (ipt), y
    sta wrk + 0
    iny
    lda (ipt), y
    sta wrk + 1
    ; load index

    ; pointer to next reference
    clc 
    lda #2
    adc ipt + 0
    sta ipt + 0
    bne @end
    inc tos + 1
@end:

pick_:
    ; just compare high byte
    lda wrk + 1
    cmp #>init
    beq jump_

nest_:   
    ; aka docol
    ; push into return stack
    ldx rix
    lda ipt + 0
    sta ret_zero - 2, x
    lda ipt + 1
    sta ret_zero - 1, x
    dex
    dex
    stx rix

link_:
    ; next reference
    lda wrk + 0
    sta ipt + 0
    lda wrk + 1
    sta ipt + 1
    jmp next_

jump_:
    ; do the jump
    jmp (wrk)

;----------------------------------------------------------------------
; extras for 6502
; vide eorBookV1.0.1

; set overflow bit
setovr_:
    bit @ends
@ends:
    rts

; where I am
here_:
    jsr @pops
@pops:
    pla
    tay
    pla
    tax
    rts

; Z flag is zero in NMOS6502
nmos_:
    sed
    clc
    lda #$99
    adc #$01
    cld
    rts

ends:
;----------------------------------------------------------------------
init:
