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

;/*
;note todo:
;
;    1. resolver for functional accept and word, (expect is obsolete)
;
;*/

;-----------------------------------------------------------------------
;
;   A Forth for 6502
;   using minimal thread code and
;   absolute memory indirect references
;   stacks LSB and MSB splited by STACKSIZE
;
;   FALSE is $0000 TRUE is $FFFF
;   no real SP@ or RP@ or SP! or RP! 
;   just internal offsets SP0+index or RP0+index
;   SP0 and RP0 could be anywhere in RAM
;   stacks moves backwards, as -2 -1 0 1 2 3 ...
;   0 is one, 1 is two,
; 
;   uses A, X, Y, caller will keep
;
;   why another Forth? To learn how to.
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
;-----------------------------------------------------------------------
.segment "ZERO"

;-----------------------------------------------------------------------
* = $E0

; saved X register
svx:    .byte $0

; saved Y register
svy:    .byte $0

; forth sp index offset
spi:    .byte $0

; forth rp index offset
rpi:    .byte $0

; data stack base pointer
;spt:    .word $0

; return stack base pointer
;rpt:    .word $0

; dictionary next free cell pointer
dpt:    .word $0

; instruction pointer
ipt:    .word $0

; extra registers
one:    .word $0
two:    .word $0
six:    .word $0
ten:    .word $0

;-----------------------------------------------------------------------
*= $F0

stat:   .word $0
base:   .word $0
here:   .word $0
last:   .word $0
back:   .word $0

tibz:   .word $0
toin:   .word $0

;-----------------------------------------------------------------------
.segment "CODE"

boot:
    ; prepare hardware 
    jmp main

task: .word $0
page: .word $0
buff: .word $0
head: .word $0
tail: .word $0

;-----------------------------------------------------------------------
tib:
.res TERMINAL, $0

; could be at page zero, less code, less cycles

.res sps, $0
sp0: .word $0

.res sps, $0
rp0: .word $0

;-----------------------------------------------------------------------
;
; leave space for page zero, hard stack,
; and buffer, locals, forth stacks
;
* = $400
;----------------------------------------------------------------------

main:

;   disable interrupts
    sei

;   clear BCD
    cld

;   set real stack at $0100
    ldx #$00
    txs

;   enable interrupts
    cli

; init forth

    jmp cold

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

;-----------------------------------------------------------------------
;   return stack stuff
;-----------------------------------------------------------------------
rpush:
    ldx rpi
    dec rpi
    lda one + 0
    sta rp0 - 1, x
    lda one + 1
    sta rp0 - 1 + sps, x
    rts

;-----------------------------------------------------------------------
rpull:
    ldx rpi
    inc rpi
    lda rp0 + 0, x
    sta one + 0
    lda rp0 + 0 + sps, x
    sta one + 1
    rts

;-----------------------------------------------------------------------
;   data stack stuff
;-----------------------------------------------------------------------

spush:
    ldx spi
    dec spi
    lda one + 0
    sta sp0 - 1, x
    lda one + 1
    sta sp0 - 1 + sps, x
    rts

spull:
    ldx spi
    inc spi
    lda sp0 + 0, x
    sta one + 0
    lda sp0 + 0 + sps, x
    sta one + 1
    rts

spull2:
    ldx spi
    inc spi
    inc spi
    lda sp0 + 0, x
    sta one + 0
    lda sp0 + 0 + sps, x
    sta one + 1
    lda sp0 + 1, x
    sta two + 0
    lda sp0 + 1 + sps, x
    sta two + 1
    rts

;-----------------------------------------------------------------------
; forth primitives
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; ( w -- )
def_word "drop", "drop", 0
    inc spi
    jmp next_

;-----------------------------------------------------------------------
; ( w -- w w ) 
def_word "dup", "dup", 0
    ldx spi
    dec spi
    lda sp0 + 0, x
    sta sp0 - 1, x
    lda sp0 + 0 + sps, x
    sta sp0 - 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 w1 w2) 
def_word "over", "over", 0
    ldx spi
    dec spi
    lda sp0 + 1, x
    sta sp0 - 1, x
    lda sp0 + 1 + sps, x
    sta sp0 - 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 w1 ) 
def_word "swap", "swap", 0
    ldx spi
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
    ldx spi
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
    ldx spi
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
    ldx spi
    inc spi
    lda sp0 + 0, x
    and sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    and sp0 + 1 + sps, x
this_:
    sta sp0 + 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 OR w2) ) 
def_word "or", "ort", 0
    ldx spi
    inc spi
    lda sp0 + 0, x
    ora sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    ora sp0 + 1 + sps, x
    jmp this_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 XOR w2) ) 
def_word "xor", "xort", 0
    ldx spi
    inc spi
    lda sp0 + 0, x
    eor sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    eor sp0 + 1 + sps, x
    jmp this_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 - w2) ) 
def_word "-", "sub", 0
    ldx spi
    inc spi
    sec
    lda sp0 + 0, x
    sbc sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    sbc sp0 + 1 + sps, x
    jmp this_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 + w2) ) 
def_word "+", "add", 0
    ldx spi
    inc spi
    clc
    lda sp0 + 0, x
    adc sp0 + 1, x
    sta sp0 + 1, x
    lda sp0 + 0 + sps, x
    adc sp0 + 1 + sps, x
    jmp this_

;-----------------------------------------------------------------------
cpt_:
    ldx spi
    sec
    pha
    sbc sp0 + 0, x
    sta sp0 + 0, x
    pla
    sbc sp0 + 0 + sps, x
    sta sp0 + 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- ($0000 - w1) ) 
def_word "negate", "neg", 0
    lda #$00
    jmp cpt_

;-----------------------------------------------------------------------
; ( w1 -- ($FFFF - w1) ) 
def_word "invert", "inv", 0
    lda #$FF
    jmp cpt_

;-----------------------------------------------------------------------
cmp_:
    ldx spi
    inc spi
    inc spi
    sec
    lda sp0 + 0, x
    sbc sp0 + 1, x
    lda sp0 + 0 + sps, x
    sbc sp0 + 1 + sps, x
    rts

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 = w2) ) 
def_word "=", "eq", 0
    jsr cmp_
    beq true2
    bne false2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 < w2) ) 
def_word "<", "lt", 0
    jsr cmp_
    bmi true2
    bcs false2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 > w2) ) 
def_word ">", "gt", 0
    jsr cmp_
    bpl true2
    bcc false2

;-----------------------------------------------------------------------
; ( w1 -- (w1 << 1) ) 
def_word "FALSE", "false", 0
false2:
    lda #$00    ; false
    beq same2_

;-----------------------------------------------------------------------
; ( w1 -- (w1 << 1) ) 
def_word "TRUE", "true", 0
true2:
    lda #$FF    ; true
    bne same2_   ; could be falltrought

;-----------------------------------------------------------------------
same2_:
    dec spi
    ldx spi
    sta sp0 + 0, x
    sta sp0 + 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- (w1 << 1) ) 
def_word "shl", "shl", 0
    ldx spi
    asl sp0 + 0, x
    rol sp0 + 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- (w1 >> 1) ) 
def_word "shr", "shr", 0
    ldx spi
    lsr sp0 + 0, x
    ror sp0 + 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
cto_:
    jsr spull2
    ldy #0
    lda two + 0
    sta (one), y
    rts

;-----------------------------------------------------------------------
to_:
    jsr cto_
    iny
    lda two + 1
    sta (one), y
    rts

;-----------------------------------------------------------------------
; ( w1 w2 -- ) *w1 = (0x00FF AND w2)
def_word "c!", "cstore", 0
    jsr cto_
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- ) *w1 = w2 
def_word "!", "store", 0
    jsr to_
    jmp next_

;-----------------------------------------------------------------------
cat_:
    ldx spi
    lda sp0 + 0, x
    sta one + 0
    lda sp0 + 0 + sps, x
    sta one + 1
    ldy #0
    lda (one), y
    sta sp0 + 0, x
    rts

at_:
    jsr cat_
    iny
    lda (one), y
    sta sp0 + 0 + sps, x
    rts

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = 0x00FF AND *w1 
def_word "c@", "cfetch", 0
    jsr cat_
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = *w1 
def_word "@", "fetch", 0
    jsr at_
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = w1+1 
def_word "1+", "incr", 0
    ldx spi
    inc sp0 + 0, x
    bne @ends
    inc sp0 + 0 + sps, x
@ends:
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = w1-1 
def_word "1-", "decr", 0
    ldx spi
    lda sp0 + 0, x
    bne @ends
    dec sp0 + 0 + sps, x
@ends:
    dec sp0 + 0, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- )  
def_word "cell", "cell", 0
    lda #2
    sta one + 0
    lda #0
    sta one + 1
    jsr spush
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- )  
def_word "exec", "exec", 0
    jsr spull
    jmp (one)

;-----------------------------------------------------------------------
; ( w1 w2 -- )  
def_word "+!", "addto", 0
    jsr spull2
    ldy #0
    clc
    lda (one), y
    adc two + 0
    sta (one), y
    iny
    lda (one), y
    adc two + 1
    sta (one), y
	jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- )  
def_word "-!", "subto", 0
    jsr spull2
    ldy #0
    sec
    lda (one), y
    sbc two + 0
    sta (one), y
    iny
    lda (one), y
    sbc two + 1
    sta (one), y
	jmp next_

;-----------------------------------------------------------------------
; ( -- w1 ) R( w1 -- )  
def_word "r@", "rat", 0
    jsr rpull
    dec rpi
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
    sta one + 0
    lda #0
    sta one + 1
    jsr spush
    jmp next_

;-----------------------------------------------------------------------
; ( -- w1 ) offset index of SP0 
def_word "sp@", "spat", 0
    lda spi
    bcc stkis_

;-----------------------------------------------------------------------
; ( -- w1 )  offset index of RP0
def_word "rp@", "rpat", 0
    lda rpi
    bcc stkis_

;-----------------------------------------------------------------------
; ( w1 -- )  offset index of SP0
def_word "sp!", "tosp", 0
    ldx spi
    lda sp0 + 0, x
    sta spi
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- )  offset index of RP0
def_word "rp!", "torp", 0
    ldx rpi
    lda rp0 + 0, x
    sta rpi
    jmp next_

;-----------------------------------------------------------------------
; prepare for mult or divd
opin:
    ldx spi
    ; pseudo one
    lda sp0 + 0, x
    sta ten + 0
    lda sp0 + 0 + sps, x
    sta ten + 1
    ; pseudo two
    lda sp0 + 1, x
    sta six + 0
    lda sp0 + 1 + sps, x
    sta six + 1
    ; clear results
    lda #0
    sta one + 0
    sta one + 1
    sta two + 0
    sta two + 1
    ; countdown
    ldy #16
    rts

;-----------------------------------------------------------------------
; resume from mult or divd
opout:
    ; copy results
    ldx spi
    lda two + 0
    sta sp0 + 0, x
    lda two + 1
    sta sp0 + 0 + sps, x
    lda one + 0
    sta sp0 + 1, x
    lda one + 1
    sta sp0 + 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; Divide the top 2 cell of the stack
; http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
; dividend divisor -- result remainder
; ( six ten -- two one )
div_:
    jsr opin
@loop:
    asl six + 0
    rol six + 1
    rol one + 0
    rol one + 1
    sec
    lda one + 0
    sbc ten + 0
    tax
    lda one + 1
    sbc ten + 1
    bcc @skip
    sta one + 1
    stx one + 0
    inc six + 0
@skip:
    ; countdown
    dey
    bne @loop
    ; results
    lda six + 0
    sta two + 0
    lda six + 1
    sta two + 1
    ; ends
    jmp opout

;-----------------------------------------------------------------------
; 16-bit multiply 16x16, 32 result
; http://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
; ( multiplicand multiplier -- resultMSW resultLSW )
; ( six ten -- two one )
mul_:
    jsr opin
@shift_r:
    ; divide by 2
    lsr ten + 1
    ror ten + 0
    bcc @rotate_r
    ; add multiplicand to upper half product
    tax
    clc
    lda six + 0
    adc one + 0
    sta one + 0
    txa
    adc six + 1
@rotate_r:
    ; rotate partial product upper to low
    ror
    ror one + 1
    ror two + 1
    ror two + 0
    ; countdown
    dey
    bne @shift_r
    sta one + 0
    ; ends
    jmp opout

;-----------------------------------------------------------------------
;
; Forth stuff:
;
; uses one and ipt, all operations by offsets
; ipt MUST be preserved and reserved for those routines
;
; as is, Minimal Thread Code for 6502
;
;-----------------------------------------------------------------------
; ( -- )  
def_word "exit", "exit", 0
unnest_:  ; aka semis:
    ; pull from return stack
    ldx rpi
    inc rpi
    lda rp0 + 0, x
    sta ipt + 0
    lda rp0 + 0 + sps, x
    sta ipt + 1

    ; this code repeats elsewhere 
    ; but use jsr/rts will delay 12 cycles

next_:
    ldy #0
    lda (ipt), y
    sta ten + 0
    iny
    lda (ipt), y
    sta ten + 1

    ; to next reference
    inc ipt + 0
    bne @p1
    inc ipt + 1
@p1:
    inc ipt + 0
    bne @p2
    inc ipt + 1
@p2:

pick_:
    ; just compare MSB bytes
    lda ten + 1
    cmp #>ends+1    ; init of heap dictionary
    bmi jump_

nest_:   
    ; aka docol
    ; push into return stack
    ldx rpi
    dec rpi
    lda ipt + 0
    sta rp0 - 1, x
    lda ipt + 1
    sta rp0 - 1 + sps, x

link_:
    ; next reference
    lda ten + 0
    sta ipt + 0
    lda ten + 1
    sta ipt + 1
    jmp next_

jump_:
    ; do the jump
    jmp (ten)

;-----------------------------------------------------------------------
; ( -- )  
def_word ":$", "colon_code", 0
    jmp (ipt)

;-----------------------------------------------------------------------
; ( -- ) zzzz must compile jmp next_  
def_word ";$", "comma_code", 0
    jmp next_

;-----------------------------------------------------------------------
; ( -- ipt )  
def_word "lit@", "litat", 0
    ldx spi
    dec spi
    lda ipt + 0
    sta sp0 - 1, x
    lda ipt + 1
    sta sp0 - 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( -- )  
def_word "lit", "lit", 0
    ldx spi 
    dec spi
    ldy #0
    lda (ipt), y
    sta sp0 - 1, x
    iny
    lda (ipt), y
    sta sp0 - 1 + sps, x
    
bump_:
    ; to next reference
    inc ipt + 0
    bne @p1
    inc ipt + 1
@p1:
    inc ipt + 0
    bne @p2
    inc ipt + 1
@p2:
    jmp next_

;-----------------------------------------------------------------------
; ( -- )  
def_word "0branch", "zbranch", 0
    ldx spi
    inc spi
    lda sp0 + 0, x
    and sp0 + 0 + sps, x
    beq bran_
    bne bump_

;-----------------------------------------------------------------------
; ( -- )    branch by a word offset  
def_word "branch", "branch", 0
bran_:
    ldy #0
    lda (ipt), y
    sta one + 0
    iny
    lda (ipt), y
    sta one + 1

    clc
    lda ipt + 0
    adc one + 0
    sta ipt + 0
    lda ipt + 1
    adc one + 1
    sta ipt + 1
    jmp next_

; OK = 1

.ifdef OK
;----------------------------------------------------------------------
; inside routines
getch:
putch:
same:
parse:

; for feedback
    lda stat + 0
    bne resolve
    lda #'O'
    jsr putchar
    lda #'K'
    jsr putchar
    lda #10
    jsr putchar

resolve:
; get a token
    jsr token

find:
; load last
    lda last + 1
    sta two + 1
    lda last + 0
    sta two + 0
    
@loop:
; lsb linked list
    lda two + 0
    sta wrd + 0

; verify \0x0
    ora two + 1
    beq abort

;   maybe to place a code for number? 
;   but not for now.

;   uncomment for feedback, comment out "beq abort" above
    lda #'?'
    jsr putchar
    lda #'?'
    jsr putchar
    lda #10
    jsr putchar
    jmp abort  ; end of dictionary, no more words to search, abort

@each:    

; msb linked list
    lda two + 1
    sta wrd + 1

; update next link 
    ldx #(wrd) ; from 
    ldy #(two) ; into
    jsr copyfrom

; compare words
    ldy #0

; save the flag, first byte is (size and flag) 
    lda (wrd), y
    sta stat + 1

; compare chars
@equal:
    lda (tou), y
; space ends
    cmp #32  
    beq @done
; verify 
    sec
    sbc (wrd), y     
; clean 7-bit ascii
    asl        
    bne @loop

; next char
    iny
    bne @equal

@done:
; update wrd
    tya
    ;; ldx #(wrd) ; set already
    ;; addwx also clear carry
    jsr addwx
    
eval:
; executing ? if == \0
    lda stat + 0   
    beq execute

; immediate ? if < \0
    lda stat + 1   
    bmi immediate      

compile:

    ; lda #'C'
    ; jsr putchar

    ;jsr wcomma

    ;bcc resolve

immediate:
execute:

    ; lda #'E'
    ; jsr putchar

    lda #>resolvept
    sta ipt + 1
    lda #<resolvept
    sta ipt + 0

    jmp pick_

;----------------------------------------------------------------------
; receive a char and masks it
getchar: 
    jsr getch
    and #$7F    ; mask 7-bit ASCII
    cmp #' ' 
    rts

;----------------------------------------------------------------------
; accept an asciiz line 
accept:
    ldy #0
@loop:
    sta (tibz), y
    iny 
    jsr getchar
    bpl @loop
; edit for \b \u \n \r ...
@bck:
    cmp #8  ;   \t
    bne @cnc 
    jmp @ctl
@cnc:
    cmp #24 ;   '\u'
    beq accept
@nl:
    cmp #10 ;   '\n'
    beq @end
@cr:
    cmp #13 ;   '\r'
    beq @end
@ctl:
    dey
    dey
    jmp @loop
@end:
    dey
    lda #0
    sta (tibz), y
    rts

;---------------------------------------------------------------------
; receive a word between spaces
; to a buffer as c-str
; note: also ends at controls
; 
word:
    ldy #0

@skip:  ; skip spaces
    jsr getchar
    bmi @ends
    beq @skip

@scan:  ; scan spaces
    iny
    sta (toin), y
    jsr getchar
    bmi @ends
    bne @scan

@ends:  ;  store lenght at head
    dey
    tya
    ldy #0
    sta (toin), y
    rts

.endif

;----------------------------------------------------------------------
; common must
;
cold:

;----------------------------------------------------------------------
warm:
; link list of headers
    lda #>h_exit
    sta last + 1
    lda #<h_exit
    sta last + 0

; next heap free cell, at 256-page:
    lda #>ends + 1
    sta here + 1
    lda #0
    sta here + 0

;---------------------------------------------------------------------
; supose never change
reset:
    ;ldy #>tib
    ;sty toin + 1
    ;sty tout + 1

abort:
    ldx #0
    stx spi           

quit:
    ldx #0
    stx rpi           

;-----------------------------------------------------------------------
; BEWARE, MUST BE AT END! MINIMAL THREAD CODE DEPENDS ON IT!
ends:
;-----------------------------------------------------------------------
