; vim: filetype=asm sw=8 ts=8 autoindent expandtab shiftwidth=8 et:
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
;
; please vide notes.md
;
;-----------------------------------------------------------------------
;  ca65 assembler 
;----------------------------------------------------------------------

; identifiers
.pC02
.feature c_comments
.feature string_escapes
.feature org_per_seg
.feature dollar_is_pc
.feature pc_assignment
;.case +

;-----------------------------------------------------------------------
; macros for dictionary, makes:
;
;   h_name:
;   .word  link_to_previous_entry
;   .byte  strlen((name)) + flags
;   .byte  name ; sequence of bytes
;   name:
;
;-----------------------------------------------------------------------
; labels
.macro makelabel arg1, arg2
.ident (.concat (arg1, arg2)):
.endmacro

; goto inner next
; as hook for user 
.macro release
        ; goto next
        jmp (void)
.endmacro


;-----------------------------------------------------------------------
.if 0

        comments

.endif
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; headers 
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

;-----------------------------------------------------------------------
; variables for macro header

hcount .set 0

H0000 = 0

;-----------------------------------------------------------------------
;  end of ca65 assembler 
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;   init of Forth 
;-----------------------------------------------------------------------
;    constants

;-----------------------------------------------------------------------
.if 0

; highlander, reserved flag.
FLAG_RSV = 1<<5

; highlander, restrict for compiler flag.
FLAG_CPL = 1<<6

.endif
;-----------------------------------------------------------------------

; highlander, immediate flag.
FLAG_IMM = 1<<7

IMMEDIATE = FLAG_IMM

; maximum length of words
LENGTH = 32

; cell size, two bytes, 16-bit
CELL = 2

; forth TIB terminal input area
TERMINAL  = $50 + 4

FALSE = 0

TRUE = -1

;-----------------------------------------------------------------------
; For use of 6502.toy, reserved for bios
;
; $0000 to $003F 
; $0100 to $013F 
; $0200 to $02FF

;-----------------------------------------------------------------------
.segment "ZERO"

* = $40 

; depends on BIOS, define later

status:        .byte $0
irqreq:        .byte $0
irqjmp:        .word $0

; forth variables

up:     .word $0        ; user pointer
dp:     .word $0        ; dictionary pointer, mixed header + code
ip:     .word $0        ; instruction pointer
wk:     .word $0        ; fixed above np

; extra dummies
np:     .res 8

; alias
one = np + 0
two = np + 2
six = np + 4
ten = np + 6

;-----------------------------------------------------------------------
; bottom of data stack, at least 52 bytes
S0 = $00FF

;-----------------------------------------------------------------------
; bottom of return stack, at least 52 bytes
R0 = $01FF

;-----------------------------------------------------------------------
; terminal input buffer, at least 82 bytes, 
T0 = $0200

;-----------------------------------------------------------------------
;
; leave space for page zero, hard stack,
; and buffers, locals, stacks, etc
;
.segment "CODE"

* = $1000

boot:
        ; prepare hardware 
        nop             ; align even
        jmp main

.byte $DE, $AD, $BE, $EF

;-----------------------------------------------------------------------
; forth variables start at U0
U0 = *

void:           .word $0        ; reserved, will point to next_

s0:             .word $0        ; mark of data stack
r0:             .word $0        ; mark of return stack

state:          .word $0        ; state of Forth, 1 is compiling
base:           .word $0        ; number radix for input and output
latest:         .word $0        ; reference to last link, is LASTEST 
last:           .word $0        ; reference to last here

tibz:           .word $0        ; TIB, fixed terminal input buffer 
toin:           .word $0        ; reference to next input

scr:            .word $0        ; actual editing screen number
blk:            .word $0        ; actual interpretation block number
block:          .word $0        ; actual reference for block space
source:         .word $0        ; CFA of inputs, 0 is terminal

width:          .word $0        ; maximun size of a word name
current:        .word $0        ; current vocabulary
context:        .word $0        ; context vocabularies chain
vocabulary:     .word $0        ; newest  vocabulary

;-----------------------------------------------------------------------
main:

;   if is executing then boot setup was done :))
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

;   init forth

        jmp cold

;---------------------------------------------------------------------
;  init of lib6502 emulator 
;---------------------------------------------------------------------
getc:
        lda $E000

eofs:
        cmp #$FF 
        beq byes

putc:
        sta $E000
        rts

; exit from emulator
byes:
        jmp $0000

;---------------------------------------------------------------------
;  end of lib6502 emulator 
;---------------------------------------------------------------------

;-----------------------------------------------------------------------
; forth primitives
; A is for dummy
; X is the data stack index
; Y is zero almost time, by next_
; stacks grows backwards, push decreases, pull increases
; notation: left is the TOP
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; ( -- ) just the first word
def_word "NULL", "NULL", 0
        nop
        nop
        nop
        nop
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w -- w << 1 ) roll left, C=0, C -> b0, b7 -> C
def_word "RSFL", "RSFL", 0
        clc
        rol 0, x
        rol 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w -- w >> 1 ) roll right, C=0, C -> b7, b0 -> C
def_word "RSFR", "RSFR", 0
        clc
        ror 1, x
        ror 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w -- w << 1 ) arithmetic left, sign keep, 0 -> b0, b7 -> C
def_word "2*", "ASFL", 0
        asl 0, x
        rol 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w -- w >> 1 ) arithmetic right, sign keep, C -> b7, b0 -> C
def_word "2/", "ASFR", 0
        lda 1, x
        asl
        ror 1, x
        ror 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( -- ) R( w -- ) 
def_word "RDROP", "RDROP", 0
        pla
        pla
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w -- ) R( -- w ) 
def_word ">R", "TOR", 0
tor_:
        lda 0, x
        pha
        lda 1, x
        pha
        bra drop_

;-----------------------------------------------------------------------
; ( w -- w | w w ) 
def_word "?DUP", "QDUP", 0
        lda 0, x
        ora 1, x
        bne dups_
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w -- w w ) 
def_word "DUP", "DUP", 0
dups_:
        dex
        dex
        lda 2, x
        sta 0, x
        lda 3, x
stan1_:
        sta 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 w2 w1 ) copy the second element to top.
def_word "OVER", "OVER", 0
        dex
        dex
        lda 4, x
        sta 0, x
        lda 5, x
        bra stan1_

;-----------------------------------------------------------------------
; ( w u -- w << u ) 
def_word "LSHIFT", "LSHIFT", 0
        ldy 0, x
	beq @ends
@loop:
	asl 2, x
        rol 3, x
        dey
	bne @loop
@ends:
        bra drop_

;-----------------------------------------------------------------------
; ( w u -- w >> u )) 
def_word "RSHIFT", "RSHIFT", 0
        ldy 0, x
	beq @ends
@loop:
	lsr 3, x
	ror 2, x
        dey
	bne @loop
@ends:
        bra drop_

;-----------------------------------------------------------------------
; ( w -- )
def_word "DROP", "DROP", 0
drop_:
        inx
        inx
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 ) 
def_word "NIP", "NIP", 0
        lda 0, x
        sta 2, x
        lda 1, x
        sta 3, x
        bra drop_

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 + w2 ) 
def_word "+", "PLUS", 0
        clc
        lda 2, x
        adc 0, x
        sta 2, x
        lda 3, x
        adc 1, x
        bra stan3_

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 - w1 ) 
def_word "-", "MINUS", 0
        sec
        lda 2, x
        sbc 0, x
        sta 2, x
        lda 3, x
        sbc 1, x
        bra stan3_

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 OR w2 ) 
def_word "OR", "ORT", 0
        lda 2, x
        ora 0, x
        sta 2, x
        lda 3, x
        ora 1, x
        bra stan3_

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 XOR w2 ) 
def_word "XOR", "XORT", 0
        lda 2, x
        eor 0, x
        sta 2, x
        lda 3, x
        eor 1, x
        bra stan3_

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 AND w2 ) 
def_word "AND", "ANDT", 0
        lda 2, x
        and 0, x
        sta 2, x
        lda 3, x
        and 1, x
stan3_:        
        sta 3, x
        inx
        inx
        ;goto next
	release

;-----------------------------------------------------------------------
; ( d -- FALSE | TRUE ) 
def_word "D0=", "DZEQ", 0
        lda 2, x
        bne nfalse
        lda 3, x
        bne nfalse
        lda 0, x
        bne nfalse
        lda 1, x
        bne nfalse
        bra ntrue

;-----------------------------------------------------------------------
; ( w -- w == 0 ) 
def_word "0=", "ZEQ", 0
        lda 0, x
        ora 1, x
        bne nfalse
        beq ntrue

;-----------------------------------------------------------------------
; ( w -- w < 0 ) 
def_word "0<", "ZLT", 0
        lda 1, x
        asl
        bcc nfalse
        bcs ntrue

;-----------------------------------------------------------------------
; ( -- $0000 ) 
def_word "FALSE", "FFALSE", 0
        dex
        dex
nfalse:
        lda #$00
        sta 2, x
        bra stan3_

;-----------------------------------------------------------------------
; ( -- $FFFF ) 
def_word "TRUE", "TTRUE", 0
        dex
        dex
ntrue:
        lda #$FF
        sta 2, x
        bra stan3_

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 < w2 )   
def_word "U<", "ULT", 0
        lda 3, x
        cmp 1, x
        bmi fends
        lda 2, x
        cmp 0, x
fends:
        inx
        inx
        bcc nfalse
        bcs ntrue

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 == w2 ) 
def_word "=", "EQ", 0
        clc
        lda 0, x
        eor 2, x
        bne fends
        lda 1, x
        eor 3, x
        bne fends
        sec
        bra fends

;-----------------------------------------------------------------------
; ( d1 d2 -- d1 < d2 ) 
def_word "D<", "DLTH", 0
        lda 5, x
        cmp 1, x
        bmi fends
        lda 4, x
        cmp 0, x
        bmi fends
        lda 7, x
        cmp 3, x
        bmi fends
        lda 6, x
        cmp 2, x
        bra fends

;-----------------------------------------------------------------------
; ( -- w ) R( w -- w )  
def_word "R@", "RAT", 0
        stx wk
        tsx
        lda 0, x
        pha
        lda 1, x
        pha 
        ldx wk
        bra push_
        
;-----------------------------------------------------------------------
; ( -- w ) R( w -- ) 
def_word "R>", "RTO", 0
push_:
        dex
        dex
putw_:
        pla
        sta 1, x
        pla
        sta 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 w1 ) 
def_word "SWAP", "SWAP", 0
        lda 2, x
        pha
        lda 3, x
        pha
iswa_:
        lda 0, x
        sta 2, x
        lda 1, x
        sta 3, x
        bra putw_

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
        bra iswa_

;-----------------------------------------------------------------------
; ( w1 w2 w3 -- w2 w3 w1 ) 
def_word "-ROT", "BROT", 0
        lda 2, x
        pha
        lda 3, x
        pha
        lda 4, x
        sta 2, x
        lda 5, x
        sta 3, x
        lda 0, x
        sta 4, x
        lda 1, x
        sta 5, x
        bra putw_

;-----------------------------------------------------------------------
; ( d1 d2 -- d1 + d2 ) 
def_word "D+", "DPLUS", 0
        clc
        lda 2, x
        adc 6, x
        sta 6, x
        lda 3, x
        adc 7, x
        sta 7, x
        lda 0, x
        adc 4, x
        sta 4, x
        lda 1, x
        adc 5, x
        bra stan5_

;-----------------------------------------------------------------------
; ( d1 d2 -- d2 - d1 ) 
def_word "D-", "DMINUS", 0
        sec
        lda 2, x
        sbc 6, x
        sta 6, x
        lda 3, x
        sbc 7, x
        sta 7, x
        lda 0, x
        sbc 4, x
        sta 4, x
        lda 1, x
        sbc 5, x
stan5_:
        sta 5, x
drop2_:
        inx
        inx
        inx
        inx
        ; goto next
        release

;-----------------------------------------------------------------------
; ( c a -- ) *a = 0x00FF AND c, c store
def_word "C!", "CTO", 0
        lda 2, x
        clc
        bcc stow_

;-----------------------------------------------------------------------
; ( w a -- ) *a = w, store 
def_word "!", "TO", 0
        lda 2, x
        sta (0, x)
        inc 0, x
        bne @bne
        inc 1, x
@bne:
        lda 3, x
stow_:
        sta (0, x)
        bra drop2_

;-----------------------------------------------------------------------
; ( w a -- )  
def_word "+!", "PLUSTO", 0
        clc
        lda (0, x)
        adc 2, x
        sta (0, x)
        inc 0, x
        bne @bcc
        inc 1, x
@bcc:
        lda (0, x)
        adc 3, x
        bra stow_

;-----------------------------------------------------------------------
; ( a  -- c )   c fetch
def_word "C@", "CAT", 0
        lda (0,x)
        sta 0, x
        sty 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( a  -- w )  fetch
def_word "@", "AT", 0
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
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w  -- w + 1 ) 
def_word "1+", "ONEPLUS", 0
        inc 0, x
        bne @bne
        inc 1, x
@bne:
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w  -- w - 1 )  
def_word "1-", "ONEMINUS", 0
        lda 0, x
        bne @bne
        dec 1, x
@bne:
        dec 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w  -- w + 2 )  
def_word "2+", "TWOPLUS", 0
        ; next reference
        clc
        lda 0, x
        adc #$02
        sta 0, x
        bcc @bcc
        inc 1, x
@bcc:
        ;goto next
	release
        
;-----------------------------------------------------------------------
; ( w  -- w - 2 )  
def_word "2-", "TWOMINUS", 0
        ; next reference
        sec
        lda 0, x
        sbc #$02
        sta 0, x
        bcc @bcc
        dec 1, x
@bcc:
        ;goto next
	release
        
;-----------------------------------------------------------------------
; ( w -- w ) assure word is even, because CELL is 2 
def_word "ALIGN", "ALIGN", 0
        ldy 0, x
        iny
        tya
        ror
        rol
        sta 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; ( w -- $0000 - w1 )   one complement 
def_word "NEGATE", "NEGATE", 0
        lda #$00
        beq complement_

;-----------------------------------------------------------------------
; ( w -- $FFFF - w )    two complement 
def_word "INVERT", "INVERT", 0
        lda #$FF
complement_:
        sec
        pha
        sbc 0, x
        sta 0, x
        pla
        sbc 1, x
        sta 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
;       copy data stack into work space, top 4 cells
ds2ws_:
        asl a           ; count bytes
        sta np - 1      ; wk counts 
@loop:
        lda 0, x
        sta one, y
        inx 
        iny
        cpy np - 1
        bne @loop
        ldy #0
        rts

;-----------------------------------------------------------------------
;       copy from work space into data stack, top 4 cells
ws2ds_:
        asl a      ; count bytes
        tay 
@loop:
        lda one, y
        dex
        sta 0, x
        dey
        bne @loop
        rts

;-----------------------------------------------------------------------
; ( -- )  for jump into a native code
; wise native code ends with unnest
def_word ":$", "COLON_CODE", 0
        jmp (ip)

;-----------------------------------------------------------------------
; ( -- ) zzzz for return from native code 
; the code is not inner mode ! must compile native code for it
def_word ";$", "COMMA_CODE", IMMEDIATE
where_i_am:
        ; zzzz
        rts

;-----------------------------------------------------------------------
; ( -- )  
def_word "LIT", "LIT", 0
        jmp DOCON

;-----------------------------------------------------------------------
; ( -- )  ; zzzzz
def_word "DODOES", "DODOES", 0
        lda ip + 0
        pha
        sta ip + 1
        pha
        ;goto next
	release

;-----------------------------------------------------------------------
; ( -- ip ) eForth dovar, push IP++  
def_word "DOVAR", "DOVAR", 0
        dex
        dex
        lda ip + 0
        sta 0, x
        lda ip + 1
        sta 1, x
        bra bump_

;-----------------------------------------------------------------------
; ( -- (ip) ) eForth docon, push (IP)++ 
def_word "DOCON", "DOCON", 0
        dex
        dex
        lda (ip), y
        sta 0, x
        iny
        lda (ip), y
        sta 1, x
        bra bump_
        
;-----------------------------------------------------------------------
; ( -- )  
def_word "0BRANCH", "QBRANCH", 0
        inx
        inx
        lda 255, x
        ora 254, x
        beq bran_
bump_:
        ; next reference
        clc
        lda #$02
        adc ip + 0
        sta ip + 0
        bcc @bcc
        inc ip + 1
@bcc:
        ;goto next
	release

;-----------------------------------------------------------------------
; ( -- )    branch by offset, 16-bit signed  
def_word "BRANCH", "BRANCH", 0
bran_:
        lda (ip), y
        sta wk + 0
        iny
        lda (ip), y
        sta wk + 1
offset_:
        clc
        lda ip + 0
        adc wk + 0
        sta ip + 0
        lda ip + 1
        adc wk + 1
        sta ip + 1
        ;goto next
	release


;-----------------------------------------------------------------------
; ( a1 a2 n -- )    move bytes, 16-bits, signed, not optmized
def_word "CMOVE", "CMOVE", 0
cmove_:
        lda #3
        jsr ds2ws_
        ; ldy #0
@loop:
        ; copy bytes
        lda (six), y
        sta (two), y
        ; decrement counter
        lda one + 0
        bne @bne0
        dec one + 1
@bne0:
        dec one + 0
        ; verify terminate
        lda one + 0
        ora one + 1
        beq @ends
@bne1:
        ; increment origin
        inc six + 0
        bne @bne2
        inc six + 1
@bne2:
        ; increment destiny
        inc two + 0
        bne @bne3
        inc two + 1
@bne3:
        ; and loop
        bne @loop
@ends:
        ;goto next
	release
        
;-----------------------------------------------------------------------
; ( a1 a2 n -- )    move words, 16-bit signed  
def_word "MOVE", "MOVE", 0
        ; words to bytes
        asl 0, x 
        rol 1, x
        clc
        bcc cmove_


;-----------------------------------------------------------------------
; prepare for mult or divd
opin:
        ; pseudo one
        lda 0, x
        sta ten + 0
        lda 1, x
        sta ten + 1
        ; pseudo two
        lda 2, x
        sta six + 0
        lda 3, x
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
        lda two + 0
        sta 0, x
        lda two + 1
        sta 1, x
        lda one + 0
        sta 2, x
        lda one + 1
        sta 3, x
        ; goto next
        release

;-----------------------------------------------------------------------
; Divide the top 2 cell of the stack
; http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
; dividend divisor -- result remainder
; (( six ten -- two one ))
;-----------------------------------------------------------------------
; ( w1 w2 -- w2 / w1 )   
def_word "/", "/", 0
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
        bra opout

;-----------------------------------------------------------------------
; 16-bit multiply 16x16, 32 result
; http://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
; (( multiplicand multiplier -- resultMSW resultLSW ))
; (( six ten -- two one ))
;-----------------------------------------------------------------------
; ( w1 w2 -- w2 * w1 )   
def_word "*", "*", 0
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
        bra opout

;----------------------------------------------------------------------
; ( w -- )  code a word, in ASCII, hexadecimal
def_word ".", "DOT", 0
showord:
	lda 1, x
	jsr puthex
	lda 0, x
	jsr puthex
	inx
	inx
	;goto next
	release

;----------------------------------------------------------------------
; converts a byte value to hexadecimal
puthex:
        pha
        ror
        ror
        ror
        ror
        jsr @conv
        pla
@conv:
        and #$0F
        ora #$30
        cmp #$3A
        bcc @ends
        adc #$06
@ends:
        jmp putc

;----------------------------------------------------------------------
; ( w -- )  code a word in ASCII, hexadecimal
def_word "??", "NUMBER", 0
        sty 0, x
        sty 1, x

	jsr gethex
        bmi @erro
	asl
	asl
	asl
	asl
	sta 1, x

	jsr gethex
        bmi @erro
	ora 1, x
	sta 1, x
	
	jsr gethex
        bmi @erro
	asl
	asl
	asl
	asl
	sta 0, x

	jsr gethex
        bmi @erro
        ora 0, x
	sta 0, x

@valid:
        ; got next_
        release

@erro:
        ; what todo 
        bra @valid

;----------------------------------------------------------------------
; converts a byte value to hexadecimal
;       00 to 0F valid, $FF error
gethex:
        ; control ?
	sec
	sbc #' '	; test < ' '
        bmi @ctrl
        ; digits ?
	cmp #10		; test < 10
	bcc @ends
        ; alphas ?
	sbc #07	        ; adjust for letters
	cmp #$10        	
        bmi @ends       ; test > 15
@ctrl:
        lda #$FF
@ends:
        ; returns a value 
	rts

;-----------------------------------------------------------------------
; convert a character to a value, 
; valid bases 1 to 36 only -+0-9A-Z.
; no float point allowed
;
digits:
	lda (two), y
	iny
@sneg:
	; negative sign ?
	cmp #'-'
        bne @spos
	lda #$FF
        bne @ends
@spos:
	; positive sign ?
	cmp #'+'
        bne @vdot
        lda #$FE
        bne @ends
@vdot:
        ; valid . ? 
	cmp #'.'	
        bmi @vala
        lda #$FD       
        bne @ends
@vala:
        ; valid 0 ? 
	cmp #'['	
        bmi @valb
        lda #$FC        
        bne @ends
@valb:
        ; valid Z ? 
	cmp #'0'	
        bmi @digs
        lda #$FB        
        bne @ends
@digs:
        ; control ?
	sec
	sbc #$30	; test < ' '
        ; digit ?
	cmp #10		; test < 10
	bcc @basv
	sbc #$07	; adjust for letters
@basv:
	cmp base + 0	
        bcc @ends
        lda #$FA        ; overflow base
@ends:
        ; a returns a value or error code if negative 
	rts

;----------------------------------------------------------------------
; accept an ascii 
getascii:
        jsr getc
        and #$7F    ; mask 7-bit ASCII
        cmp #' '
        rts

;----------------------------------------------------------------------
; ( a n -- a n ) NOT STANDART, for use  
def_word "UPPER", "UPPER", 0
        lda #2
        jsr ds2ws_
@loop:
        lda (two), y
        cmp #'a' + 0
        bmi @ends
        cmp #'z' + 1
        bpl @ends
        and #$DF
        sta (two), y
@ends:
        iny
        cmp one + 0
        bne @loop
        ;goto next_
        release

;----------------------------------------------------------------------
; accept an ascii line, stores a asciiz line, n < 255
; ( a n -- u ) NOT STANDART, for use  
def_word "EXPECT", "EXPECT", 0
expect:
        lda #2
        jsr ds2ws_
        
        lda #' '        
@loop:
        sta (two), y
        iny 
        cmp one + 0
        beq @end
        jsr getascii
        bpl @loop
; minimal edit for \b \t \u \n \r ...
@nl:
        cmp #10 ;   '\n'
        beq @end
@cr:
        cmp #13 ;   '\r'
        beq @end
@tab:
        cmp #9  ;   '\t'
        lda #' '
        beq @loop
@bck:
        cmp #8  ;   '\b'
        beq @ctl
@cnc:
        cmp #24 ;   '\u'
        beq expect
@ctl:
        dey
        dey
        jmp @loop
@end:
        lda #0
        sta (two), y
; push how many stored
        dex
        dex
        sty 0, x
        sta 1, x
        ;goto next
	release

; zzzz continue
;---------------------------------------------------------------------
; receive a word between spaces to a buffer as c-str
; ( c a -- c-str | 0 ) 
def_word "word", "word", 0
word:
        lda #2
        jsr ds2ws_
        ; ldy #0
        dey
        
@skip:  ; skip spaces
        iny
        lda (one), y
        beq @ends
        cmp two + 0
        beq @skip
        phy

@scan:  ; scan spaces
        iny
        lda (one), y
        beq @ends
        cmp two + 0
        bne @scan
; zzzz
@ends:  
        jsr pushy_
        dey
        jsr pushy_
        ply
        jsr pushy_
        ; goto next_
        release
@eols:
        
pushy_:
        inx
        inx
        sty 0, x
        lda #0
        sta 1, x
        rts

;-----------------------------------------------------------------------
; ( a1 a2 n -- m ) 
; compare n bytes of a2 and a1, return how many equals, n < 255  
def_word "CSAME", "CSAME", 0
        lda #3
        jsr ds2ws_

        dex
        dex
        sty 1, x
@loop:
        lda (six), y
        eor (two), y
        bne @ends
        iny
        cpy one + 0
        bne @loop
@ends:
        sty 0, x
        ;goto next
	release
;-----------------------------------------------------------------------
;
; Forth inner stuff:
;
; ip MUST be preserved and reserved, for those routines
;
; as is, Minimal Thread Code for 6502
; 
; only jumps for primitives, native code.
;
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; (( w1 -- ))  EXECUTE is done by nest in MTC
def_word "EXEC", "EXEC", 0
        lda 0, x
        sta wk + 0
        lda 1, x
        sta wk + 1
        inx 
        inx
        jmp nest_

;-----------------------------------------------------------------------
; (( w1 -- ))  EXIT is done by unnest in MTC
def_word "EXIT", "EXIT", 0
unnest_:  ; pull from return stack, aka semis
        pla
        sta ip + 1
        pla
        sta ip + 0
        ; ;goto next
	; release

;-----------------------------------------------------------------------
next_:  
        ; http://wilsonminesco.com/0-overhead_Forth_interrupts/
        ; if irqreq is zero, do interrupts; else irqreq must be 1
        ldy irqreq
        beq hang_

;-----------------------------------------------------------------------
look_:
        ; ldy #1
        lda (ip), y
        sta wk + 1
        dey
        lda (ip), y
        sta wk + 0

        ; next reference
        lda #$02
        clc
        adc ip + 0
        sta ip + 0
        bcc pick_
        inc ip + 1

;-----------------------------------------------------------------------
pick_:  ; unique in Minimal Thread Code, 3 + 2 + 2 cc
        lda wk + 1
        cmp #(>end_of_native + 1)    
        bmi jump_

;-----------------------------------------------------------------------
nest_:  ; push into return stack, aka docol
        lda ip + 0
        pha
        sta ip + 1
        pha

;-----------------------------------------------------------------------
link_: ; next reference
        lda wk + 0
        sta ip + 0
        lda wk + 1
        sta ip + 1
        ;goto next
	release

;-----------------------------------------------------------------------
; process a interrupt, could be a pool, void for now
hang_:
        inc irqreq
        lda irqjmp + 0
        sta wk + 0
        lda irqjmp + 1
        sta wk + 1

;-----------------------------------------------------------------------
jump_:  ; creed, do the jump
        
        ; jsr step_

        jmp (wk)

; alternatve for list the primitives
step_:
        rts
        ; alternative 
        pla
        pla
        jmp showord
        
;-----------------------------------------------------------------------
;       one byte constants
;-----------------------------------------------------------------------
; (( -- w1 )) index of SP0 
def_word "SP@", "SPAT", 0
        txa
        ; ldy #$00      ; page zero
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w1 ))  index of RP0
def_word "RP@", "RPAT", 0
        stx np + 0
        tsx
        txa
        ldx np + 0
        ldy #$01        ; page one
        bra lsbs_

;-----------------------------------------------------------------------
;       Constants
;-----------------------------------------------------------------------
; (( -- w))  
def_word "0", "ZERO", 0
        lda #0
lsbs_:
        dex
        dex
        sta 0, x
        sty 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( -- w))  
def_word "1", "ONE", 0
        lda #1
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "2", "TWO", 0
        lda #2
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "3", "THREE", 0
        lda #3
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "4", "FOUR", 0
        lda #4
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "BS", "BS", 0
        lda #$8
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "TB", "TB", 0
        lda #$9
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "CR", "CR", 0
        lda #$10
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "PACE", "PACE", 0
        lda #$0B
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "CANCEL", "CANCEL", 0
        lda #$18
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "ESCAPE", "ESCAPE", 0
        lda #$1B
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "BL", "BL", 0
        lda #$20
        bra lsbs_

;-----------------------------------------------------------------------
; (( -- CELL ))
def_word "CELL", "CCELL", 0
        lda CELL
        bra lsbs_

;-----------------------------------------------------------------------
;       Variables
;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "CURRENT", "CURRENT", 0
        lda #<current
        ldy #>current
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "CONTEXT", "CONTEXT", 0
        lda #<context
        ldy #>context
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "SOURCE", "SOURCE", 0
        lda #<source
        ldy #>source
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "SCR", "SCR", 0
        lda #<scr
        ldy #>scr
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "BLK", "BLK", 0
        lda #<blk
        ldy #>blk
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "UP", "UP", 0
        lda #<up
        ldy #>up
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "DP", "DP", 0
        lda #<dp
        ldy #>dp
lsbw_:
        dex
        dex
        sta 0, x
        sty 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( -- ))  used to reset S0
def_word "SP!", "SPTO", 0
        ; return hardwired S0
        lda #$FF
        ; ldy #$00
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- ))  used to reset R0
def_word "RP!", "RPTO", 0
        ; return hardwired R0
        lda #$FF
        ; ldy #$01
        iny
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
; must hold at least 84 chars, but 72 is enough
def_word "TIB", "TIB", 0
        ; return hardwire T0
        ; lda #$00
        tya
        ; ldy #$02
        iny
        iny
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
; 
def_word "TOIN", "TOIN", 0
        lda #<toin
        ldy #>toin
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
def_word "LATEST", "LATEST", 0
        lda #<latest
        ldy #>latest
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal  
def_word "LAST", "LAST", 0
        lda #<last
        ldy #>last
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
def_word "STATE", "STATE", 0
        lda #<state
        ldy #>state
        bra lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "BASE", "BASE", 0
        lda #<base
        ldy #>base
        bra lsbw_

;----------------------------------------------------------------------
;
; common
;
;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "COLD", "COLD", 0
cold:

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "WARM", "WARM", 0
warm:

; copy block rom to ram ?
; init variables ?

; only lsb
        ldx #$FF
        txs

        lda #>T0
        sta tibz + 1
        lda #<T0
        sta tibz + 0

        lda #00
        sta state + 1
        sta state + 0
        sta base + 1

        lda #$10
        sta base + 0

; link for inner next
        lda #<next_
        sta void + 0
        lda #>next_
        sta void + 1

; link list of headers, NOOP must be the last compiled word
        lda #<NOOP
        sta last + 0
        lda #>NOOP
        sta last + 1

; next heap free cell
        lda #<end_of_forth
        sta dp + 0
        lda #>end_of_forth
        sta dp + 1

;---------------------------------------------------------------------
;   supose never change

abort:

quit:

        jmp 0000

;----------------------------------------------------------------------
; BEWARE, MUST BE AT END OF PRIMITIVES ! 
; MINIMAL THREAD CODE DEPENDS ON IT !
end_of_native:  ;       end of primitives:
        nop
        nop

;-----------------------------------------------------------------------
;
; start of pre-compiled dictionary
;
.include "words.s"

;-----------------------------------------------------------------------
; (( w1 -- w1 ))     
def_word "NOOP", "NOOP", 0
	.word EXIT

;----------------------------------------------------------------------
end_of_forth:

.byte $DE, $AD, $C0, $DE

;----------------------------------------------------------------------
.end


