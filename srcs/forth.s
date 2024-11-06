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
;note
;
;   done:
;
;   06/11/2024
;       wise keep Y=0 in next, 
;       keep X as dedicated data stack index.
;       rewrite for not splited MSB LSB stacks. 
;            It was a waste cycles for every @ !
;        
;   05/11/2024
;       rewrite for use stacks at pages zero and one
;
;   03/11/2024
;       using absolute splited stacks. 
;       interrupt service from R65F11, 
;           also http://wilsonminesco.com/6502primer/IRQconx.html
;
;   todo:
;       no vocabularies.
;
;*/

;-----------------------------------------------------------------------
;
;   A(nother) Forth for 6502 with
;   data stack in page zero,
;   return stack in page one
;   and using minimal thread code
;   Fig-Forth alike.
;
;   uses X as data stack index, Y as keeped zero 
;
;   (still) non-rellocable code, 
;   (still) non-optimized code,
;
;   FALSE is $0000 (0) TRUE is $FFFF (-1)
;   SP0 and RP0 uses $FF as botton
;
;   stacks LSB and MSB 
;   stacks grows backwards, as -2 -1 0 1 2 3 ...
;   0 is TOS, 1 is NOS,
;
;   No real CFA, always is next cell after c-name
;   No need of DOCOL at CFA, vide MTC
;
;   why another Forth? To learn how to.
;
;   for use in the 6502.toy SBC
;
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;  init of ca65 assembler specifics
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
makelabel "H_", label
.ident(.sprintf("H%04X", hcount + 1)) = *
.word .ident (.sprintf ("H%04X", hcount))
hcount .set hcount + 1
.byte .strlen(name) + flag + 0 ; nice trick !
.byte name
makelabel "", label
.endmacro

; generic macros
; arguments must be in page zero,
; could be routines

; 1+
.macro addone wrd
    inc wrd + 0
    bne :+
    inc wrd + 1
:
.endmacro

; 2+
.macro addtwo wrd
    inc wrd + 0
    bne :+
    inc wrd + 1
:
    inc wrd + 0
    bne :+
    inc wrd + 1
:
.endmacro
    
; acm +
.macro addacm wrd
    clc
    adc wrd + 0
    sta wrd + 0
    bcc :+
    inc wrd + 1
:
.endmacro

.macro copyinto from, into
    ldy #0
    lda from + 0
    sta (into), y
    iny
    lda from + 1
    sta (into), y
.endmacro

.macro copyfrom from, into
    ldy #0
    lda (from), y
    sta into + 0
    iny
    lda (from), y
    sta into + 1
.endmacro


;-----------------------------------------------------------------------
; variables for macros

hcount .set 0

H0000 = 0

;-----------------------------------------------------------------------
;  end of ca65 assembler specifics
;-----------------------------------------------------------------------

;---------------------------------------------------------------------
;  this code depends on system or emulators
;
;  lib6502  emulator
;
getch:
    lda $E000

eofs:
; EOF ?
    cmp #$FF ; also clean carry :)
    beq byes

putch:
    sta $E000
    rts

; exit for emulator
byes:
    jmp $0000

;
;   lib6502 emulator
;---------------------------------------------------------------------

;-----------------------------------------------------------------------
;    constants

FALSE = 0

TRUE = -1

; cell size, two bytes, 16-bit
CELL = 2

; forth stack size
STACKSIZE = $18 ; 24 cells

; forth TIB terminal input area
TERMINAL  = $50 + 4

; highlander, immediate flag.
FLAG_IMM = 1<<7

IMMEDIATE = FLAG_IMM

; highlander, restrict for compiler flag.
; FLAG_CPL = 1<<6

; highlander, reserved flag.
; FLAG_RSV = 1<<5

; maximum length of words
LENGTH = 15

;-----------------------------------------------------------------------
; Forth like functions
; uses A, Y, X caller must saves.
;-----------------------------------------------------------------------
.segment "ZERO"

; left $00 to $2F for extras, eg. SWEET-16, MONITOR, etc

* = $40

upt: .word $0
ipt: .word $0
dpt: .word $0
wrk: .word $0

xsave:
XS: .byte $0
ysave:
YS: .byte $0

; extra dummies, 
; must be 4 for multiply and division
N:  
one:    .word $0
two:    .word $0
six:    .word $0
ten:    .word $0

;-----------------------------------------------------------------------
; *= $F0  ; external use

user:   .word $0    ; start of user area
stat:   .word $0    ; state of Forth, 0 is interpret, 1 is compiling
base:   .word $0    ; number radix for input and output
last:   .word $0    ; reference to last word, link field

here:   .word $0    ; here to compiling
back:   .word $0    ; keep the here while compiling
tibz:   .word $0    ; TIB
toin:   .word $0    ; TIB reference to next word


;-----------------------------------------------------------------------
.segment "CODE"

boot:
    ; prepare hardware 
    jmp main

;task: .word $0
;page: .word $0
;buff: .word $0
;head: .word $0
;tail: .word $0

* = $0100
;-----------------------------------------------------------------------
tib:
.res TERMINAL, $0

;-----------------------------------------------------------------------
;
; leave space for page zero, hard stack,
; and buffers, locals, stacks, etc
;
* = $1000
;----------------------------------------------------------------------

main:

;   if is executing then boot setup was done :)
;   but wise keep clean

;   disable interrupts
    sei

;   clear BCD
    cld

;   set S0 
    ldx #$FF

;   set R0 
    txs

;   enable interrupts
    cli

;   extended init functions 
    
;   init forth

    jmp cold

;-----------------------------------------------------------------------
; extras for 6502
; vide eorBookV1.0.1

; set overflow bit
setovr_:
    bit @ends
@ends:
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
; forth primitives
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; ( w1 -- (w1 << 1) ) 
def_word "2*", "SFHL", 0
    asl 0, x
    rol 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- (w1 >> 1) ) 
def_word "2/", "SFHR", 0
    lsr 0, x
    ror 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w -- )
def_word "DROP", "DROP", 0
    clc
    bcc drop_

;-----------------------------------------------------------------------
; ( w -- w w ) 
def_word "DUP", "DUP", 0
    dex
    dex
    lda 2, x
    sta 0, x
    lda 3, x
    sta 1, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 w2 w1) 
def_word "OVER", "OVER", 0
    dex
    dex
    lda 4, x
    sta 0, x
    lda 5, x
    sta 1, x
    jmp next_

;-----------------------------------------------------------------------
; ( -- w1 ) R( w1 -- w1 )  
def_word "R@", "RAT", 0
    dex
    dex
    pla
    sta 1, x
    pla 
    sta 0, x
; zzzz
    lda 0, x
    pha
    lda 1, x
    pha
    jmp next_
    
;-----------------------------------------------------------------------
; ( w1 -- ) R( -- w1) 
def_word ">R", "TOR", 0
tor_:
    lda 0, x
    pha
    lda 1, x
    pha
drop_:
    inx
    inx
    jmp next_

;-----------------------------------------------------------------------
; ( -- w1 ) R( w1 -- ) 
def_word "R>", "RTO", 0
push:
    dex
    dex
putw:
    pla
    sta 1, x
    pla
    sta 0, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 w1 ) 
def_word "SWAP", "SWAP", 0
    lda 2, x
    pha
    lda 3, x
    pha
    lda 0, x
    sta 2, x
    lda 1, x
    sta 3, x
    clc 
    bcc putw

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 AND w2) ) 
def_word "AND", "ANDT", 0
    lda 2, x
    and 0, x
    sta 2, x
    lda 3, x
    and 1, x
    sta 3, x
    clc
    bcc drop_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 OR w2) ) 
def_word "OR", "ORT", 0
    lda 2, x
    ora 0, x
    sta 2, x
    lda 3, x
    ora 1, x
    sta 3, x
    clc
    bcc drop_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 XOR w2) ) 
def_word "XOR", "XORT", 0
    lda 2, x
    eor 0, x
    sta 2, x
    lda 3, x
    eor 1, x
    sta 3, x
    clc
    bcc drop_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 + w2) ) 
def_word "+", "ADD", 0
    clc
    lda 2, x
    adc 0, x
    sta 2, x
    lda 3, x
    adc 1, x
    sta 3, x
    clc
    bcc drop_

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 - w2) ) 
def_word "-", "SUB", 0
    sec
    lda 2, x
    sbc 0, x
    sta 2, x
    lda 3, x
    sbc 1, x
    sta 3, x
    clc
    bcc drop_

;-----------------------------------------------------------------------
; ( -- 0x0000 ) 
def_word "FALSE", "FFALSE", 0
false2:
    lda #$00    ; false
    pha
    pha
    bne push_

;-----------------------------------------------------------------------
; ( -- 0xFFFF ) 
def_word "TRUE", "TTRUE", 0
true2:
    lda #$FF    ; true
    pha
    pha
    bne push_

;-----------------------------------------------------------------------
; ( w1 -- (w1 == 0) ) 
def_word "0=", "EQZ", 0
    inx
    inx
    lda -1, x
    ora -2, x
    beq true2
    bne false2

;-----------------------------------------------------------------------
; ( w1 -- (w1 < 0) ) 
def_word "0<", "LTZ", 0
    lda 1, x
    asl
    inx
    inx
    bcc false2
    bcs true2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 < w2) )   
def_word "U<", "LTU", 0
    lda 2, x 
    cmp 0, x
    bne @mcc
    lda 3, x
    cmp 1, x
@mcc:
    inx
    inx
    inx
    inx
    bcs false2
    bcc true2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 = w2) ) 
def_word "=", "EQ", 0
    lda 0, x
    xor 2, x
    bne false2
    lda 1, x
    xor 3, x
    bne false2
    beq true2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 < w2) ) 
def_word "<", "LT", 0
    jsr cmp_
    bmi true2
    bpl false2

;-----------------------------------------------------------------------
; ( w1 w2 -- (w1 > w2) ) 
def_word ">", "GT", 0
    jsr cmp_
    bmi false2
    beq false2
    bpl true2

;-----------------------------------------------------------------------
; ( c a -- ) *a = (0x00FF AND c)
def_word "C!", "CSTORE", 0
    lda 2, x
    sta (0,x)
    clc
    bcc drop2

;-----------------------------------------------------------------------
; ( w a -- ) *a = w 
def_word "!", "STORE", 0
    lda 2, x
    sta (0, x)
    inc 0, x
    bne @bne
    inc 1, x
@bne:
    lda 3, x
    sta (0, x)
    clc
    bcc drop2

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = 0x00FF AND *w1 
def_word "C@", "CFETCH", 0
    lda (0,x)
    sta 0, x
    sty 0, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = *w1 
def_word "@", "FETCH", 0
    lda (0,x)
    pha
    inc 0, x
    bne @bne
    inc 1, x
@bne:
    lda (0,x)
    sta 1, x
    pla
    sta 0, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = w1+1 
def_word "1+", "INCR", 0
    inc 0, x
    bne @bne
    inc 1, x
@bne:
    jmp next_

;-----------------------------------------------------------------------
; ( w1  -- w2 ) w2 = w1-1 
def_word "1-", "DECR", 0
    lda 0, x
    bne @bne
    dec 1, x
@bne:
    dec 0, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- )  
def_word "+!", "ADDTO", 0
    ; zzzz
	jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- )  
def_word "-!", "SUBTO", 0
    ; zzzz
	jmp next_

;-----------------------------------------------------------------------
; ( -- w1 ) index of SP0 
def_word "SP@", "SPAT", 0
    txa
    pha
    lda #$00
    pha
    clc
    bcc putw

;-----------------------------------------------------------------------
; ( -- w1 )  index of RP0
def_word "RP@", "RPAT", 0
    stx xsave
    tsx
    txa
    ldx xsave
    pha
    lda #$01
    pha
    clc 
    bcc putw

;-----------------------------------------------------------------------
; ( w1 -- )  S0 or offset index of SP0
def_word "SP!", "TOSP", 0
    lda 0, x
    tax
    jmp next_

;-----------------------------------------------------------------------
; ( w1 -- )  R0 or offset index of RP0
def_word "RP!", "TORP", 0
    lda 0, x
    stx xsave
    tax
    txs
    ldx xsave
    jmp next_

;-----------------------------------------------------------------------
; ( -- w)  
def_word "DP", "DP", 0
    lda dpt + 0
    pha
    lda dpt + 1
    pha
    clc
    bcc push_

;-----------------------------------------------------------------------
; ( -- ) assure word is even, because CELL is 2 
def_word "ALIGN", "ALIGN", 0
    lda 0, x
    lsr
    bnc @ends
    jmp INCR 
@ends:
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
def_word "EXIT", "EXIT", 0
unnest_:  ; aka semis:
    ; pull from return stack, also semis ;S
    pla
    sta ipt + 1
    pla
    sta ipt + 0

    ; this code repeats elsewhere 
    ; but use jsr/rts will delay 12 cycles

next_:
    ; todo:
    ; bit INTFLG
    ; bvs intr_

    ldy #1
    lda (ipt), y
    sta wrk + 1
    dey
    lda (ipt), y
    sta wrk + 0

    ; next reference
    lda #$02
    clc
    adc ipt + 0
    sta ipt + 0
    bcc pick_
    inc ipt + 1

pick_:
    ; just compare MSB bytes
    lda wrk + 1
    cmp #>ends+1    ; init of heap compose dictionary
    bmi jump_

nest_:   
    ; aka docol
    ; push into return stack
    lda ipt + 0
    pha
    sta ipt + 1
    pha

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
;
; intr_:
;   ; process a interrupt 
    ; could be a pool
    ; as Forth 
;    lda #$C0
;    sta INTFLG
;    rti

;-----------------------------------------------------------------------
; ( -- w)  
def_word "0", "ZERO", 0
    lda #0
lows_:
    dex
    dex
    sta 0, x
    lda #0
    sta 1, x
    jmp next_

;-----------------------------------------------------------------------
; ( -- w)  
def_word "1", "ONE", 0
    lda #1
    clc 
    bcc lows_

;-----------------------------------------------------------------------
; ( -- w)  
def_word "2", "TWO", 0
    lda #2
    clc 
    bcc lows_

;-----------------------------------------------------------------------
; ( -- w)  
def_word "3", "THREE", 0
    lda #3
    clc 
    bcc lows_

;-----------------------------------------------------------------------
; ( -- w)  
def_word "BL", "BLANK", 0
    lda #32
    clc 
    bcc lows_

;-----------------------------------------------------------------------
; ( -- CELL )
def_word "CELL", "CCELL", 0
    lda CELL
    clc 
    bcc lows_

;-----------------------------------------------------------------------
; ( w1 -- )  
; EXEC is done by ´ R> EXIT ´
def_word "EXEC", "EXEC", 0
    lda 0, x
    pha
    lda 1, x
    pha
    ;zzzz

;-----------------------------------------------------------------------
; ( -- )  for jump into a native code
; wise native code ends with unnest
def_word ":$", "COLON_CODE", 0
    jmp (ipt)

;-----------------------------------------------------------------------
; ( -- ) zzzz for return from native code 
; the code is not inner mode ! must compile native code for it
def_word ";$", "COMMA_CODE", IMMEDIATE

; zzzz must compile that
    lda here + 0
    sta ipt  + 0
    lda here + 1
    sta ipt + 1

    jmp next_

; some magics
where_i_am:
    ; zzzz
    rts

;-----------------------------------------------------------------------
bump_:
    ; next reference
    lda #$02
    clc
    adc ipt + 0
    sta ipt + 0
    bcc @bcc
    inc ipt + 1
@bcc:
    jmp next_

;-----------------------------------------------------------------------
; ( -- ipt )  
def_word "(DODOE)", "DODOES", 0
    ; zzzz
    jmp next_

;-----------------------------------------------------------------------
; ( -- ipt ) dovar  
def_word "(DOVAR)", "DOVAR", 0
    dex
    dex
    lda ipt + 0
    sta 0, x
    lda ipt + 1
    sta 1, x
    jmp bump_

;-----------------------------------------------------------------------
; ( -- (ipt) ) docon 
def_word "(DOCON)", "DOCON", 0
    dex
    dex
    ldy #1
    lda (ipt), y
    sta 1, x
    dey 
    lda (ipt), y
    sta 0, x
    jmp bump_
    
;-----------------------------------------------------------------------
; ( -- )  
def_word "LIT", "LIT", 0
    jmp DOCON

;-----------------------------------------------------------------------
; ( -- )  
def_word "0BRANCH", "ZBRANCH", 0
    inx
    inx
    lda -1, x
    ora -2, x
    beq bran_
    jmp bump_

;-----------------------------------------------------------------------
; ( -- )    branch by a word offset  
def_word "BRANCH", "BRANCH", 0
bran_:
    ldy #1
    lda (ipt), y
    sta wrk + 1
    iny
    lda (ipt), y
    sta wrk + 0

    clc
    lda ipt + 0
    adc wrk + 0
    sta ipt + 0
    lda ipt + 1
    adc wrk + 1
    sta ipt + 1
    jmp next_

;-----------------------------------------------------------------------
;
; as is, Minimal Thread Code for 6502
;
;-----------------------------------------------------------------------
; ( -- )  
def_word "EXIT", "EXIT", 0
unnest_:  ; aka semis:
    ; pull from return stack, also semis ;S
    pla
    sta ipt + 1
    pla
    sta ipt + 0

    ; this code repeats elsewhere 
    ; but use jsr/rts will delay 12 cycles

next_:
    ; todo:
    ; bit INTFLG
    ; bvs intr_

    ldy #1
    lda (ipt), y
    sta wrk + 1
    dey
    lda (ipt), y
    sta wrk + 0

    ; next reference
    lda #$02
    clc
    adc ipt + 0
    sta ipt + 0
    bcc pick_
    inc ipt + 1

pick_:
    ; just compare MSB bytes
    lda wrk + 1
    cmp #>ends+1    ; init of heap compose dictionary
    bmi jump_

nest_:   
    ; aka docol
    ; push into return stack
    lda ipt + 0
    pha
    sta ipt + 1
    pha

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
; ( -- )    find a word in dictionary, return CFA or FALSE (0x0)  
def_word "'", "TICK", 0
    jsr find_
    jsr spush
    jmp next_

;-----------------------------------------------------------------------
; ( -- )    compile a word in dictionary, from TOS  
def_word ",", "COMMA", 0
    jsr spull
    jsr coma_
    jmp next_

;-----------------------------------------------------------------------
;   push a cell in heap
coma_:
    copyinto one, here
    addtwo here
    rts

;-----------------------------------------------------------------------
; search the dictionary for a word, returns the cfa or null
;
find_:
; load last
    lda last + 1
    sta two + 1
    lda last + 0
    sta two + 0
    
@loop:
; lsb linked list
    lda two + 0
    sta one + 0
    lda two + 1
    sta one + 1

; verify \0x0
    ora two + 0
    beq @ends

@each:    
; update next link 
    copyfrom one, two

; update to c-name    
    addtwo one

; compare words
    ldy #0

; save the flag, MSB of state is (size and flag) 
    lda (one), y
    sta stat + 1

; compare chars
@equal:
    lda (two), y
; lte space ends
    cmp #(32 + 1)  
    bmi @done

; verify 
    sec
    sbc (one), y     
; clean 7-bit ascii
    asl        
    bne @loop

; next char
    iny
    bne @equal

@done:
; update to code
    tya
    addacm one

@ends:
    rts

;----------------------------------------------------------------------
; inside routines

parse:

outerptr:
    .word outer_

outer_:

; for feedback
    lda stat + 0
    bne resolve

    lda #'O'
    jsr putch
    lda #'K'
    jsr putch
    lda #10
    jsr putch

resolve:
; get a token
    jsr word_

; is a defined word ?
    jsr find_
   
    bne eval_

    jsr nums_

    bne eval_

quit_:
    lda #'?'
    jsr putch
    lda #'?'
    jsr putch
    lda #10
    jsr putch
    jmp abort  ; end of dictionary, no more words to search, abort

;----------------------------------------------------------------------
eval_:

; executing ? if == \0
    lda stat + 0   
    beq execute

; immediate ? if < \0
    lda stat + 1   
    bmi immediate      

compile:

    jsr coma_

    jmp outer_

immediate:
execute:

    lda #>outerptr
    sta ipt + 1
    lda #<outerptr
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
; accept an ascii line, stores a asciiz line
accept:
    ldy #0
@loop:
    sta (tibz), y
    iny 
    jsr getchar
    bpl @loop
; minimal edit for \b \u \n \r ...
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
word_:
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

;----------------------------------------------------------------------
nums_:
    ; parse a number
    rts

;----------------------------------------------------------------------
; common must
;
cold:

;----------------------------------------------------------------------
warm:
; link list of headers
    lda #>NULL
    sta last + 1
    lda #<NULL
    sta last + 0

; next heap free cell, at 256-page:
    lda #>ends + 1
    sta here + 1
    lda #0
    sta here + 0

;---------------------------------------------------------------------
;   supose never change
;   stacks grows backwards
reset:
    ;ldy #>tib
    ;sty toin + 1
    ;sty tout + 1

abort:
    ldx #$FF
    stx spi           

quit:
    ldx #$FF
    stx rpi           

;-----------------------------------------------------------------------
; BEWARE, MUST BE AT END OF PRIMITIVES ! 
; MINIMAL THREAD CODE DEPENDS ON IT !
ends:
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;   COMPOSE WORDS, pre-compiled
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; ( -- )     
def_word "BEGIN", "BEGIN", 0
    .word DUP, CELL, ADD, FETCH, SWAP, FETCH, EXIT


;-----------------------------------------------------------------------
; two cells stuff

;-----------------------------------------------------------------------
; ( w1 -- w2 w3 )     
def_word "2@", "TWOAT", 0
    .word DUP, CELL, ADD, FETCH, SWAP, FETCH, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2!", "TWOTO", 0
    .word SWAP, OVER, STORE, CELL, ADD, STORE, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2R>", "TWORTO", 0
    .word RTO, RTO, SWAP, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2>R", "TWOTOR", 0
    .word SWAP, TOR, TOR, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2R@", "TWORPAT", 0
    .word RTO, RTO, TWODUP, TWORTO, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2DROP", "TWODROP", 0
    .word DROP, DROP, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2DUP", "TWODUP", 0
    .word OVER, OVER, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2OVER", "TWOOVER", 0
    .word TWOTOR, TWODUP, TWORTO, TWOSWAP, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "2SWAP", "TWOSWAP", 0
    .word ROT, TOR, ROT, RTO, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "CELL+", "CELLADD", 0
    .word CELL, ADD, EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 --  )     
def_word "CELLS", "CELLS", 0
    ; cells is two 
    .word SFHL, EXIT

;-----------------------------------------------------------------------
; reviews
;-----------------------------------------------------------------------
; ( w1 -- ($0000 - w1) ) 
def_word "NEGATE", "NEGATE", 0
    lda #$00
    bne cpt_

;-----------------------------------------------------------------------
; ( w1 -- ($FFFF - w1) ) 
def_word "INVERT", "INVERT", 0
    lda #$FF
cpt_:
    pha
    sec
    sbc 0, x
    sta 0, x
    pla
    sbc 0 + sps, x
    sta 0 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 -- (w1 w2 + w3 w4) ) 
def_word "D+", "DADD", 0
    clc
    lda 0, x
    adc 2, x
    sta 2, x
    lda 0 + sps, x
    adc 2 + sps, x
    sta 2 + sps, x
    lda 1, x
    adc 3, x
    sta 3, x
    lda 1 + sps, x
    adc 3 + sps, x
    sta 3 + sps, x
drop2:
    inx
    inx
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 -- (w1 w2 - w3 w4) ) 
def_word "D-", "DSUB", 0
    sec
    lda 0, x
    sbc 2, x
    sta 2, x
    lda 0 + sps, x
    sbc 2 + sps, x
    sta 2 + sps, x
    lda 1, x
    sbc 3, x
    sta 3, x
    lda 1 + sps, x
    sbc 3 + sps, x
    sta 3 + sps, x
    clc
    bcc drop2

;-----------------------------------------------------------------------
; ( w1 w2 w3 -- w3 w1 w2 ) 
def_word "ROT", "ROT", 0
    lda 4, x
    pha
    lda 5, x
    pha
    lda 2, x
    sta 4, x
    lda 3, x
    sta 5, x
    lda 1, x
    sta 3, x
    lda 0, x
    sta 2, x
    clc 
    bcc putw

;-----------------------------------------------------------------------
; ( w1 w2 w3 -- w2 w3 w1 ) 
def_word "-ROT", "BROT", 0
    lda 0, x
    pha
    lda 0 + sps, x
    pha
    lda 1, x
    sta 0, x
    lda 1 + sps, x
    sta 0 + sps, x
    lda 2, x
    sta 1, x
    lda 2 + sps, x
    sta 1 + sps, x
    clc 
    bcc putw

;-----------------------------------------------------------------------
;   COMPOSED WORDS
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; BEWARE, MUST BE AT END OF COMPOSED ! 
;-----------------------------------------------------------------------
; ( w1 -- w2 w3 )     
def_word "NULL", "NULL", 0
    .word FALSE, EXIT

