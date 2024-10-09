; vim: filetype=asm sw=4 ts=4 autoindent expandtab shiftwidth=4 et:
;-----------------------------------------------------------------------
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
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;
;   A Forth for 6502
;   using minimal thread code and
;   absolute memory indirect references
;   stacks LSB and MSB splited by STACKSIZE
;
;   FALSE is $0000 TRUE is $FFFF
;   no real SP@ or RP@ or SP! or RP! 
;   just internal offsets SP0+idx or RP0+idx
;   SP0 and RP0 could be anywhere in RAM
;
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;
;  ca65 assembler specifics
;
;----------------------------------------------------------------------

; identifiers

; enable 6502 mode

.p02

;.case +

; enable features

.feature c_comments

.feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment


;   .byte idx
;   .byte idy

;   define .word sp0
;   define .word rp0
;
;   .word ipt
;   .word dpt
;   .word usr
;   .word tos
;   .word nos
;
;-----------------------------------------------------------------------
; macros for dictionary, makes:
;
;   h_name:
;   .word  link_to_previous_entry
;   .byte  strlen(name) + flags
;   .byte  name
;   name:
;
; label for primitives
.macro makelabel arg1, arg2
.ident (.concat (arg1, arg2)):
.endmacro

; header for primitives
; the entry point for dictionary is h_~name~
; the entry point for code is ~name~
.macro def_word name, label, flag
makelabel "h_", label
.ident(.sprintf("H%04X", hcount + 1)) = *
.word .ident (.sprintf ("H%04X", hcount))
hcount .set hcount + 1
.byte .strlen(name) + flag + 0 ; nice trick !
.byte name
makelabel "", label
.endmacro

;-----------------------------------------------------------------------
; variables for macros

hcount .set 0

H0000 = 0

;-----------------------------------------------------------------------
;    constants

FALSE = 0

TRUE = -1

; forth stack size
STACKSIZE = $24

sps = STACKSIZE

; forth terminal input area
TERMINAL  = $50

; cell size, two bytes, 16-bit
CELL = 2

; highlander, immediate flag.
FLAG_IMM = 1<<7

; length of words
LENGTH = 15

;-----------------------------------------------------------------------
; Forth like functions
; to keep code safe by not using "fall throught".
; uses A, Y, X caller must saves.
; needs 2 levels of hardware stack
; uses 4 bytes in page zero as temporary, TOS and NOS
; uses 6 bytes in memory for internal use
;-----------------------------------------------------------------------
.segment "ZERO"

* = $F0

; save x
svx:    .byte $0
; save y
svy:    .byte $0

; forth sp index offset
six:    .byte $0
; forth rp index offset
rix:    .byte $0

; no data stack base pointer
; spt:    .word $0
; no return stack base pointer
; rpt:    .word $0

; dictionary next free cell pointer
dpt:    .word $0
; instruction pointer
ipt:    .word $0

; registers

wrk:    .word $0
tos:    .word $0
nos:    .word $0
tmp:    .word $0

;-----------------------------------------------------------------------
.segment "CODE"

tib:
.res TERMINAL, $0

; could be at page zero, less code, less cycles

.res sps, $0
sp0: .word $0

.res sps, $0
rp0: .word $0

;
; leave space for page zero, hard stack,
; and buffer, locals, forth stacks
;
* = $400
;-----------------------------------------------------------------------
;   data stack stuff

spush:
    ldx six
    lda tos + 0
    sta sp0 - 1, x
    lda tos + 1
    sta sp0 - 1 + sps, x
    jmp keep

spull:
    ldx six
    lda sp0 + 0, x
    sta tos + 0
    lda sp0 + 0 + sps, x
    sta tos + 1
    jmp lose

spush2:
    ldx six
    lda nos + 0
    sta sp0 - 1, x
    lda nos + 1
    sta sp0 - 1 + sps, x
    lda tos + 0
    sta sp0 - 2, x
    lda tos + 1
    sta sp0 - 2 + sps, x
    dex
    jmp keep

spull2:
    ldx six
    lda sp0 + 0, x
    sta tos + 0
    lda sp0 + 0 + sps, x
    sta tos + 1
    lda sp0 + 1, x
    sta nos + 0
    lda sp0 + 1 + sps, x
    sta nos + 1
    inx 
    jmp lose

;-----------------------------------------------------------------------
; ( w -- )
def_word "drop", "drop", 0
    ldx six
lose:
    inx
    stx six
    jmp next_

;-----------------------------------------------------------------------
; ( w -- w w ) 
def_word "dup", "dup", 0
    ldx six
    lda sp0 + 0, x
    sta sp0 - 1, x
    lda sp0 + 0 + sps, x
    sta sp0 - 1 + sps, x
keep:
    dex
    stx six
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 w1 w2) 
def_word "over", "over", 0
    ldx six
    lda sp0 + 1, x
    sta sp0 - 1, x
    lda sp0 + 1 + sps, x
    sta sp0 - 1 + sps, x
    jmp keep

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 w1 ) 
def_word "swap", "swap", 0
    ldx six
    lda sp0 + 0, x
    sta sp0 - 1, x
    lda sp0 + 0 + sps, x
    sta sp0 - 1 + sps, x
    lda sp0 + 1, x
    sta sp0 + 0, x
    lda sp0 + 1 + sps, x
    sta sp0 + 0 + sps, x
    lda sp0 - 1, x
    sta sp0 + 1, x
    lda sp0 - 1 + sps, x
    sta sp0 + 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 w3 -- w3 w1 w2 ) 
def_word "rot", "rotf", 0
    ldx six
    lda sp0 + 2, x
    sta sp0 - 1, x
    lda sp0 + 2 + sps, x
    sta sp0 - 1 + sps, x
    lda sp0 + 1, x
    sta sp0 + 2, x
    lda sp0 + 1 + sps, x
    sta sp0 + 2 + sps, x
    lda sp0 + 0, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    sta sp0 + 1 + sps, x
    lda sp0 - 1, x
    sta sp0 + 0, x
    lda sp0 - 1 + sps, x
    sta sp0 + 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 w3 -- w2 w3 w1 ) 
def_word "rot-", "rotb", 0
    ldx six
    lda sp0 + 0, x
    sta sp0 - 1, x
    lda sp0 + 0 + sps, x
    sta sp0 - 1 + sps, x
    lda sp0 + 1, x
    sta sp0 + 0, x
    lda sp0 + 1 + sps, x
    sta sp0 + 0 + sps, x
    lda sp0 + 2, x
    sta sp0 + 1, x
    lda sp0 + 2 + sps, x
    sta sp0 + 1 + sps, x
    lda sp0 - 1, x
    sta sp0 + 2, x
    lda sp0 - 1 + sps, x
    sta sp0 + 2 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 AND w2) ) 
def_word "and", "andt", 0
    ldx six
    lda sp0 + 0, x
    and sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    and sp0 + 1 + sps, x
    sta sp0 + 1 + sps, x
    jmp lose

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 OR w2) ) 
def_word "or", "ort", 0
    ldx six
    lda sp0 + 0, x
    ora sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    ora sp0 + 1 + sps, x
    sta sp0 + 1 + sps, x
    jmp lose

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 XOR w2) ) 
def_word "xor", "xort", 0
    ldx six
    lda sp0 + 0, x
    eor sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    eor sp0 + 1 + sps, x
    sta sp0 + 1 + sps, x
    jmp lose

;-----------------------------------------------------------------------
cpt_:
    ldx six
    sec
    pha
    sbc sp0 + 0, x
    sta sp0 + 0, x
    sec
    pla
    sbc sp0 + 0 + sps, x
    sta sp0 + 0 + sps, x
    ; rts
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- ($0000 - w1) ) 
def_word "neg", "negate", 0
    lda #$00
    jmp cpt_

;-----------------------------------------------------------------------
; ( w1 -- ($FFFF - w1) ) 
def_word "inv", "invert", 0
    lda #$FF
    jmp cpt_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 - w2) ) 
def_word "-", "sub", 0
    ldx six
    sec
    lda sp0 + 0, x
    sbc sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    sbc sp0 + 1 + sps, x
    sta sp0 + 1 + sps, x
    jmp drop

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 + w2) ) 
def_word "+", "add", 0
    ldx six
    clc
    lda sp0 + 0, x
    adc sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    adc sp0 + 1 + sps, x
    sta sp0 + 1 + sps, x
    jmp drop

;-----------------------------------------------------------------------
cmpr:
    ldx six
    sec
    lda sp0 + 0, x
    sbc sp0 + 1, x
    lda sp0 + 0 + sps, x
    sbc sp0 + 1 + sps, x
    rts

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 = w2) ) 
def_word "=", "eq", 0
    jsr cmpr
    beq true2
    bne false2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 < w2) ) 
def_word "<", "lt", 0
    jsr cmpr
    bmi true2
    bpl false2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 > w2) ) 
def_word ">", "gt", 0
    jsr cmpr
    bpl true2
    ; bmi false2
    ; beq false2

;-----------------------------------------------------------------------
false2:
    lda #$00    ; false
    beq same2

;-----------------------------------------------------------------------
true2:
    lda #$FF    ; true
    bne same2   ; could be falltrought

;-----------------------------------------------------------------------
same2:
    ldx six
    sta sp0 + 2, x
    sta sp0 + 2 + sps, x
    inx 
    jmp lose

;-----------------------------------------------------------------------
; ( w1 -- (w1 << 1) ) 
def_word "shl", "shl", 0
    ldx six
    asl sp0 + 0, x
    rol sp0 + 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- (w1 >> 1) ) 
def_word "shr", "shr", 0
    ldx six
    lsr sp0 + 0, x
    ror sp0 + 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
cto_:
    jsr spull2
    ldy #0
    lda nos + 0
    sta (tos), y
    rts

;-----------------------------------------------------------------------
to_:
    jsr cto_
    iny
    lda nos + 1
    sta (tos), y
    rts

;-----------------------------------------------------------------------
; ( w1 w2 -- ) *w1 = (0x00FF AND w2)
def_word "c!", "cstore", 0
    sty svy 
    jsr cto_
    ldy svy
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- ) *w1 = w2 
def_word "!", "store", 0
    sty svy
    jsr to_
    ldy svy
    jmp next_

;-----------------------------------------------------------------------
cat_:
    ldx six
    lda sp0 + 0, x
    sta tos + 0
    lda sp0 + 0 + sps, x
    sta tos + 1
    ldy #0
    sta sp0 + 0 + sps, x
    lda (tos, x)
    sta sp0 + 0, x
    rts

at_:
    jsr cat_
    iny
    lda (tos), y
    sta sp0 + 0 + sps, x
    rts

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = 0x00FF AND *w1 
def_word "c@", "cfetch", 0
    sty svy
    jsr cat_
    ldy svy
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = *w1 
def_word "@", "fetch", 0
    sty svy
    jsr at_
    ldy svy
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = *w1 
def_word "1+", "incr", 0
    ldx six
    inc sp0 + 0, x
    bne @ends
    inc sp0 + 0 + sps, x
@ends:
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = *w1 
def_word "1-", "decr", 0
    ldx six
    lda sp0 + 0, x
    bne @ends
    dec sp0 + 0 + sps, x
@ends:
    dec sp0 + 0, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- )  
def_word "exec", "exec", 0
    ldx six
    lda sp0 + 0, x
    pha
    lda sp0 + 0 + sps, x
    pha
    php
    rti

;-----------------------------------------------------------------------
; ( w1 w2 -- )  
def_word "+!", "addto", 0
    jsr spull2
    sty svy
    ldy #0
    clc
    lda (tos), y
    adc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    adc nos + 1
    sta (tos), y
    ldy svy
	jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- )  
def_word "-!", "subto", 0
    jsr spull2
    sty svy
    ldy #0
    sec
    lda (tos), y
    sbc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    sbc nos + 1
    sta (tos), y
    ldy svy
	jmp next_

;-----------------------------------------------------------------------
;   return stack stuff
;-----------------------------------------------------------------------
rpush:
    ldx rix
    lda tos + 0
    sta rp0 - 1, x
    lda tos + 1
    sta rp0 - 1 + sps, x
rkeep:
    dex
    stx rix
    rts

;-----------------------------------------------------------------------
rpull:
    ldx rix
    lda rp0 + 0, x
    sta tos + 0
    lda rp0 + 0 + sps, x
    sta tos + 1
rlose:
    inx
    stx rix
    rts

;-----------------------------------------------------------------------
; ( -- w1 ) R( w1 -- )  
def_word "r@", "rat", 0
    jsr rpull
    jsr rkeep
    jsr spush
    jmp next_

;-----------------------------------------------------------------------
; ( -- w1 ) R( w1 -- )  
def_word "r>", "rto", 0
    jsr rpull
    jsr spush
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- ) R( -- w1 )  
def_word ">r", "tor", 0
    jsr spull
    jsr rpush
    jmp next_

;-----------------------------------------------------------------------
stkis_:
    sta tos + 0
    lda #0
    sta tos + 1
    jsr spush
    jmp next_

;-----------------------------------------------------------------------
; ( -- w1 ) offset index of SP0 
def_word "sp@", "spat", 0
    lda six
    bcc stkis_

;-----------------------------------------------------------------------
; ( -- w1 )  offset index of RP0
def_word "rp@", "rpat", 0
    lda rix
    bcc stkis_

;-----------------------------------------------------------------------
; ( w1 -- )  offset index of SP0
def_word "sp!", "tosp", 0
    jsr spull
    lda tos + 0
    sta six
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- )  offset index of RP0
def_word "rp!", "torp", 0
    jsr spull
    lda tos + 0
    sta rix
    jmp next_

;-----------------------------------------------------------------------
; prepare for mult or divd
opin:
    ldx six
    ; pseudo tos
    lda sp0 + 0, x
    sta wrk + 0
    lda sp0 + 0 + sps, x
    sta wrk + 1
    ; pseudo nos
    lda sp0 + 1, x
    sta tmp + 0
    lda sp0 + 1 + sps, x
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

;-----------------------------------------------------------------------
; resume from mult or divd
opout:
    ; copy results
    ldx six
    lda nos + 0
    sta sp0 + 0, x
    lda nos + 1
    sta sp0 + 0 + sps, x
    lda tos + 0
    sta sp0 + 1, x
    lda tos + 1
    sta sp0 + 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
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

;-----------------------------------------------------------------------
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


;-----------------------------------------------------------------------
;
; Forth stuff:
;
; uses wrk and ipt, all operations by offsets
; ipt MUST be preserved and reserved for those routines
;
;-----------------------------------------------------------------------
; ( -- )  
def_word "exit", "exit", 0
unnest_:  ; aka semis:
    ; pull from return stack
    ldx rix
    lda rp0 + 0, x
    sta ipt + 0
    lda rp0 + 0 + sps, x
    sta ipt + 1
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
    sta rp0 - 1, x
    lda ipt + 1
    sta rp0 - 1 + sps, x
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

;-----------------------------------------------------------------------
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


;----------------------------------------------------------------------

main:

;----------------------------------------------------------------------
; common must
;
cold:
;   disable interrupts
    sei

;   clear BCD
    cld

;   set real stack at $0100
    ldx #$FF
    txs

;   enable interrupts
    cli

;-----------------------------------------------------------------------
warm:

    ldy sps
    sty six
    sty rix


ends:
;-----------------------------------------------------------------------
init:
