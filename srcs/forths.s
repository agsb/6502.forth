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
.p02
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

FALSE = 0

TRUE = -1

; cell size, two bytes, 16-bit
CELL = 2

; forth TIB terminal input area
TERMINAL  = $50 + 4

; highlander, reserved flag.
; FLAG_RSV = 1<<5

; highlander, restrict for compiler flag.
; FLAG_CPL = 1<<6

; highlander, immediate flag.
FLAG_IMM = 1<<7

IMMEDIATE = FLAG_IMM

; maximum length of words
LENGTH = 32

;-----------------------------------------------------------------------
; For use of 6502.toy, reserved for bios
;
; $0000 to $003F 
; $0100 to $013F 
; $0200 to $02FF

;-----------------------------------------------------------------------
.segment "ZERO"

* = $40 

intflag:        .byte $0
stsflag:        .byte $0

up:     .word $0        ; user pointer
dp:     .word $0        ; dictionary pointer, mixed header + code
ip:     .word $0        ; instruction pointer
wk:     .word $0        ; wise left wk just above np

; extra dummies, 
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
last:           .word $0        ; reference to last here, is not HERE

tibz:           .word $0        ; TIB, fixed terminal input buffer 
toin:           .word $0        ; reference to next word

scr:            .word $0        ; actual editing screen
blk:            .word $0        ; actual interpretation block
block:          .word $0
source:         .word $0        ; CFA of inputs, 0 is terminal

width:          .word $0        ; maximun size of a word name
vocabulary:     .word $0        ; newest  vocabulary
current:        .word $0        ; current vocabulary
context:        .word $0        ; context vocabularies chain

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
; (( -- )) nop
def_word "NULL", "NULL", 0
        nop
        nop
        nop
        nop
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 -- w1 << 1 )) roll left, C=0, C -> b0, b7 -> C
def_word "RSFL", "RSFL", 0
        clc
        rol 0, x
        rol 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 -- w1 >> 1 )) roll right, C=0, C -> b7, b0 -> C
def_word "RSFR", "RSFR", 0
        clc
        ror 1, x
        ror 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 -- w1 << 1 )) arithmetic right, C -> b7, b0 -> C
def_word "2/", "ASFR", 0
        ; get sign bit
        ldy 1, x
        ; does a roll
        clc
        ror 1, x
        ror 0, x
        clc
        bcc sign_

;-----------------------------------------------------------------------
; (( w1 -- w1 << 1 )) arithmetic left, 0 -> b0, b7 -> C
def_word "2*", "ASFL", 0
        ; get sign bit
        ldy 1, x
        ; rolls
        clc
        rol 0, x
        rol 1, x
; set sign bit
sign_:
        tya
        bmi sign_
        ;goto next
	release
@setbit:
        lda #$80
        ora 1, x
        sta 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 -- ))
def_word "DROP", "DROP", 0
        inx
        inx
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 )) 
def_word "NIP", "NIP", 0
        lda 0, x
        sta 2, x
        lda 1, x
        sta 3, x
        inx
        inx
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 -- w1 w1 )) 
def_word "?DUP", "QDUP", 0
        lda 0, x
        eor 1, x
        bne dups_
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 -- w1 w1 )) 
def_word "DUP", "DUP", 0
dups_:
        dex
        dex
        lda 2, x
        sta 0, x
        lda 3, x
        sta 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 w2 -- w2 w1 w2)) copy the second element to top.
def_word "OVER", "OVER", 0
        dex
        dex
        lda 2, x
        sta 0, x
        lda 3, x
        sta 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( -- w1 )) R(( w1 -- w1 ))  
def_word "R@", "RAT", 0
        stx wk
        tsx
        lda 0, x
        pha
        lda 1, x
        pha 
        ldx wk
        clc
        bcc push_
        
;-----------------------------------------------------------------------
; (( -- )) R(( w1 -- )) 
def_word "RDROP", "RDROP", 0
        pla
        pla
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 -- )) R(( -- w1)) 
def_word ">R", "TOR", 0
tor_:
        lda 0, x
        pha
        lda 1, x
        pha
drop_:
        inx
        inx
        ;goto next
	release

;-----------------------------------------------------------------------
; (( -- w1 )) R(( w1 -- )) 
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
; (( w1 w2 -- w2 w1 )) 
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
        bcc putw_

;-----------------------------------------------------------------------
; (( w1 w2 w3 -- w3 w1 w2 )) 
def_word "ROT", "ROT", 0
        lda 4, x
        pha
        lda 5, x
        pha
        lda 2, x
        sta 4, x
        lda 3, x
        sta 5, x
        lda 0, x
        sta 2, x
        lda 1, x
        sta 3, x
        clc 
        bcc putw_

;-----------------------------------------------------------------------
; (( w1 w2 w3 -- w2 w3 w1 )) 
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
        clc 
        bcc putw_

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 AND w2 )) 
def_word "AND", "ANDT", 0
        lda 2, x
        and 0, x
        sta 2, x
        lda 3, x
        and 1, x
msbs_:        
        sta 3, x
        inx
        inx
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 OR w2 )) 
def_word "OR", "ORT", 0
        lda 2, x
        ora 0, x
        sta 2, x
        lda 3, x
        ora 1, x
        clc 
        bcc msbs_

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 XOR w2 )) 
def_word "XOR", "XORT", 0
        lda 2, x
        eor 0, x
        sta 2, x
        lda 3, x
        eor 1, x
        clc 
        bcc msbs_

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 + w2 )) 
def_word "+", "PLUS", 0
        clc
        lda 2, x
        adc 0, x
        sta 2, x
        lda 3, x
        adc 1, x
        clc 
        bcc msbs_

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 - w2 )) 
def_word "-", "MINUS", 0
        sec
        lda 2, x
        sbc 0, x
        sta 2, x
        lda 3, x
        sbc 1, x
        clc 
        bcc msbs_

;-----------------------------------------------------------------------
; (( w1 w2 -- == 0 )) 
def_word "D0=", "DZEQ", 0
        clc
        ldy #$4
@loop:        
        lda 0, x
        bne @ends
        inx
        dey
        bne @loop
        sec
@ends:
        bcc false2
        bcs true2

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- (w1 w2 < w3 w4) )) 
def_word "D<", "DLTH", 0
        sec
        ldy #$4
@loop:
        lda 0, x
        sbc 4, x
        inx
        dey
        bne @loop
        bcs true2
        bcc false2

;-----------------------------------------------------------------------
; (( -- 0x0000 )) 
def_word "FALSE", "FFALSE", 0
        dex
        dex
false2:
        lda #$00    ; false
same2_:
        sta 0, x
        sta 1, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( -- 0xFFFF )) 
def_word "TRUE", "TTRUE", 0
        dex
        dex
true2:
        lda #$FF    ; true
        bne same2_

;-----------------------------------------------------------------------
; (( w1 -- w1 == 0 )) 
def_word "0=", "ZEQ", 0
        lda 0, x
        eor 1, x
        bne false2
        beq true2

;-----------------------------------------------------------------------
; (( w1 -- w1 < 0 )) 
def_word "0<", "ZLT", 0
        lda 1, x
        asl
        bcc false2
        bcs true2

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 < w2 ))   
def_word "U<", "ULT", 0
        sec
        lda 1, x
        sbc 3, x
        lda 0, x
        sbc 2, x
        inx
        inx
        bcc false2
        bcs true2

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 == w2 )) 
def_word "=", "EQ", 0
        clc
        lda 0, x
        eor 2, x
        bne @bne
        lda 1, x
        eor 3, x
        bne @bne
        sec
@bne:
        inx
        inx
        bcc false2
        bcs true2

;-----------------------------------------------------------------------
; (( a c -- )) *a = 0x00FF AND c
def_word "C!", "CSTORE", 0
        lda 2, x
        clc
        bcc stow_

;-----------------------------------------------------------------------
; (( a w -- )) *a = w 
def_word "!", "STORE", 0
        lda 2, x
        sta (0, x)
        inc 0, x
        bne @bne
        inc 1, x
@bne:
        lda 3, x
stow_:
        sta (0, x)
drop2:
        inx
        inx
drop1:
        inx
        inx
        ;goto next
	release

;-----------------------------------------------------------------------
; (( a w -- ))  
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
        sta (0, x)
        clc
        bcc drop2

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- ((w1 w2 + w3 w4)) )) 
def_word "D+", "DPLUS", 0
        clc
        ldy #4
@loop:
        lda 0, x
        adc 4, x
        sta 4, x
        inx
        dey
        bne @loop
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- ((w1 w2 - w3 w4)) )) 
def_word "D-", "DMINUS", 0
        sec
        ldy #4
@loop:
        lda 0, x
        sbc 4, x
        sta 4, x
        inx
        dey
        bne @loop
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1  -- w2 )) w2 = 0x00FF AND *w1 
def_word "C@", "CFETCH", 0
        lda (0,x)
        sta 0, x
        sty 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1  -- w2 )) w2 = *w1 
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
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1  -- w1+1 )) 
def_word "1+", "ONEPLUS", 0
        inc 0, x
        bne @bne
        inc 1, x
@bne:
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1  -- w1-1 ))  
def_word "1-", "ONEMINUS", 0
        lda 0, x
        bne @bne
        dec 1, x
@bne:
        dec 0, x
        ;goto next
	release

;-----------------------------------------------------------------------
; (( w1  -- w1+2 ))  
def_word "2+", "TWOPLUS", 0
        ; next reference
        lda 0, x
        clc
        adc #$02
        sta 0, x
        bcc @bcc
        inc 1, x
@bcc:
        ;goto next
	release
        
;-----------------------------------------------------------------------
; (( w1  -- w1-2 ))  
def_word "2-", "TWOMINUS", 0
        ; next reference
        lda 0, x
        sec
        sbc #$02
        sta 0, x
        bcc @bcc
        dec 1, x
@bcc:
        ;goto next
	release
        
;-----------------------------------------------------------------------
; (( w -- w )) assure word is even, because CELL is 2 
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
; (( w1 -- (($0000 - w1)) )) 
def_word "NEGATE", "NEGATE", 0
        lda #$00
        beq cpts_

;-----------------------------------------------------------------------
; (( w1 -- (($FFFF - w1)) )) 
def_word "INVERT", "INVERT", 0
        lda #$FF
cpts_:
        pha
        sec
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
; (( -- ))  for jump into a native code
; wise native code ends with unnest
def_word ":$", "COLON_CODE", 0
        jmp (ip)

;-----------------------------------------------------------------------
; (( -- )) zzzz for return from native code 
; the code is not inner mode ! must compile native code for it
def_word ";$", "COMMA_CODE", IMMEDIATE
where_i_am:
        ; zzzz
        rts

;-----------------------------------------------------------------------
; (( -- ))  
def_word "LIT", "LIT", 0
        jmp DOCON

;-----------------------------------------------------------------------
; (( -- ))  
def_word "DODOES", "DODOES", 0
        lda ip + 0
        pha
        sta ip + 1
        pha
        ; zzzzz
        ;goto next
	release

;-----------------------------------------------------------------------
; (( -- ip )) eForth dovar, push IP++  
def_word "DOVAR", "DOVAR", 0
        dex
        dex
        lda ip + 0
        sta 0, x
        lda ip + 1
        sta 1, x
        clc
        bcc bump_

;-----------------------------------------------------------------------
; (( -- (ip) )) eForth docon, push (IP)++ 
def_word "DOCON", "DOCON", 0
        dex
        dex
        lda (ip), y
        sta 0, x
        iny 
        lda (ip), y
        sta 1, x
        clc
        bcc bump_
        
;-----------------------------------------------------------------------
; (( -- ))  
def_word "0BRANCH", "QBRANCH", 0
        inx
        inx
        lda 255, x
        ora 254, x
        beq bran_
bump_:
        ; next reference
        lda #$02
        clc
        adc ip + 0
        sta ip + 0
        bcc @bcc
        inc ip + 1
@bcc:
        ;goto next
	release

;-----------------------------------------------------------------------
; (( -- ))    branch by offset, 16-bit signed  
def_word "BRANCH", "BRANCH", 0
bran_:
        lda (ip), y
        sta wk + 0
        iny
        lda (ip), y
        sta wk + 1

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
; (( c a -- n1 n2 n3 a )) 
; adapted from Fig-Forth, 
; c delimiter, a address, n3 offset to start,
; n2 offset to end, n1 next delimiter after
; return n1 = 0, when no word before end of line
def_word "ENCLOSE", "ENCLOSE", 0
        lda #2
        jsr ds2ws_
        
        txa
        sec
        sbc #8
        tax

        sty 3, x
        sty 1, x
        dey
@skip:
        iny
        lda (two), y
        bne @etcs
        ; if \0
        sta 0, x        ; no word, end of line
        beq @ends
@etcs:
        ; if delimiter
        cmp one + 0
        beq @skip
        ; start of word
        ; if not 
        sty 4, x        ; first in word
@scan:
        lda (two), y
        bne @cont
        ; if \0
        sty 2, x
        sty 0, x
        tya
        cmp 4, x
        bne @ends
        inc 2, x
@ends:
        ; goto next_
        release
@cont:
        sty 2, x        ; last in word
        iny
        ; if not delimiter
        cmp one + 0
        bne @scan
        ; end of word
        sty 0, x        ; first delimiter after word
        beq @ends

;-----------------------------------------------------------------------
; (( n a1 a2 -- m )) 
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
; (( u a1 a2 -- ))    move words, 16-bit signed  
def_word "MOVE", "MOVE", 0
        ; words to bytes
        asl 0, x 
        rol 1, x
        clc
        bcc cmove_

;-----------------------------------------------------------------------
; (( n a1 a2 -- ))    move bytes, 16-bits, signed, not optmized
def_word "CMOVE", "CMOVE", 0
cmove_:
        lda #3
        jsr ds2ws_
@loop:
        ; copy
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
        
;----------------------------------------------------------------------
; (( w -- ))  code a word, in ASCII, hexadecimal
def_word ".", "DOT", 0
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
;       code a ASCII $FF hexadecimal in a byte
;----------------------------------------------------------------------
; (( w1 -- ))  code a word in ASCII, hexadecimal
def_word "??", "NUMBER", 0
        sty 0, x
        sty 1, x

	jsr @gethex
        bmi @erro
	asl
	asl
	asl
	asl
	sta 1, x

	jsr @gethex
        bmi @erro
	ora 1, x
	sta 1, x
	
	jsr @gethex
        bmi @erro
	asl
	asl
	asl
	asl
	sta 0, x

	jsr @gethex
        bmi @erro
        ora 0, x
	sta 0, x

@valid:
        ; got next_
        release

@erro:
        ; what todo ?
        bmi @valid

@gethex:
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
getchar:
        jsr getc
        and #$7F    ; mask 7-bit ASCII
        cmp #' '
        rts

;----------------------------------------------------------------------
; accept an ascii line, stores a asciiz line, n < 255
; (( n a -- u )) NOT STANDART, for use  
def_word "EXPECT", "EXPECT", 0
expect:
        lda #2
        jsr ds2ws_
        ldy #0
        lda #' '        
@loop:
        sta (two), y
        iny 
        cmp one + 0
        beq @end
        jsr getchar
        bpl @loop
; minimal edit for \b \u \n \r ...
@nl:
        cmp #10 ;   '\n'
        beq @end
@cr:
        cmp #13 ;   '\r'
        beq @end
@bck:
        cmp #8  ;   '\t'
        lda #' '
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

;---------------------------------------------------------------------
; receive a word between spaces
; to a buffer as c-str
; note: also ends at controls
; (( a1 -- a2 n | 0 )) 
def_word "INWORD", "INWORD", 0
word:
        lda #01
        jsr ds2ws_
        ldy #0
        
@skip:  ; skip spaces
        lda (one), y
        beq @ends
        iny
        cmp #' '
        beq @skip
        tya
        pha

@scan:  ; scan spaces
        lda (one), y
        beq @ends
        iny
        cmp #' '
        bne @scan

@ends:  ; store lenght at head
        tya
        ldy #0
        sta (one), y
        rts

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
	release

;-----------------------------------------------------------------------
next_:  
        ; do interrupts
        bit intflag
        bvs hang_

;-----------------------------------------------------------------------
look_:
        ldy #1
        lda ((ip)), y
        sta wk + 1
        dey
        lda ((ip)), y
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
jump_:  ; creed, do the jump
        
        jmp (wk)
        nop
        ; alternatve for list the primitives
        ; jsr puthex
        ; ;goto next
	release

;-----------------------------------------------------------------------
; process a interrupt, could be a pool, void for now
hang_:
        lda #$C0        ; ????
        sta intflag
        jmp look_

;-----------------------------------------------------------------------
;       one byte constants
;-----------------------------------------------------------------------
; (( -- w1 )) index of SP0 
def_word "SP@", "SPAT", 0
        txa
        beq lsbs_

;-----------------------------------------------------------------------
; (( -- w1 ))  index of RP0
def_word "RP@", "RPAT", 0
        stx np + 0
        tsx
        txa
        ldx np + 0
        ldy #$01
        bne lsbs_

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
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "2", "TWO", 0
        lda #2
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "3", "THREE", 0
        lda #3
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "4", "FOUR", 0
        lda #4
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "BS", "BS", 0
        lda #$8
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "TB", "TB", 0
        lda #$9
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "CR", "CR", 0
        lda #$10
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "PACE", "PACE", 0
        lda #$0B
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "CANCEL", "CANCEL", 0
        lda #$18
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "ESCAPE", "ESCAPE", 0
        lda #$1B
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "BL", "BL", 0
        lda #$20
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- CELL ))
def_word "CELL", "CCELL", 0
        lda CELL
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "CURRENT", "CURRENT", 0
        lda #<current
        ldy #>current
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "CONTEXT", "CONTEXT", 0
        lda #<context
        ldy #>context
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "SOURCE", "SOURCE", 0
        lda #<source
        ldy #>source
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "SCR", "SCR", 0
        lda #<scr
        ldy #>scr
        clc
        bcc lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "BLK", "BLK", 0
        lda #<blk
        ldy #>blk
        clc
        bcc lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "UP", "UP", 0
        lda #<up
        ldy #>up
        clc
        bcc lsbw_

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
def_word "SP!", "TOSP", 0
        ; return hardwired S0
        lda #$FF
        ldy #$00
        clc
        bcc lsbw_

;-----------------------------------------------------------------------
; (( -- ))  used to reset R0
def_word "RP!", "TORP", 0
        ; return hardwired R0
        lda #$FF
        ldy #$01
        clc
        bcc lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
; must hold at least 84 chars, but 72 is enough
def_word "TIB", "TIB", 0
        ; return hardwire T0
        lda #$00
        ldy #$02
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
; 
def_word "TOIN", "TOIN", 0
        lda #<toin
        ldy #>toin
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
def_word "LATEST", "LATEST", 0
        lda #<latest
        ldy #>latest
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal  
def_word "LAST", "LAST", 0
        lda #<last
        ldy #>last
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
def_word "STATE", "STATE", 0
        lda #<state
        ldy #>state
        bne lsbw_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "BASE", "BASE", 0
        lda #<base
        ldy #>base
        bne lsbw_

;----------------------------------------------------------------------
; common must
;
cold:

;----------------------------------------------------------------------
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

; link list of headers
        lda #>NULL
        sta last + 1
        lda #<NULL
        sta last + 0

; next heap free cell, at 256-page:
        lda #>end_of_forth
        sta dp + 1
        lda #<end_of_forth
        sta dp + 0

;---------------------------------------------------------------------
;   supose never change
;   stacks grows backwards
reset:

abort:

quit:

        jmp 0000

;----------------------------------------------------------------------
; BEWARE, MUST BE AT END OF PRIMITIVES ! 
; MINIMAL THREAD CODE DEPENDS ON IT !
end_of_native:  ;       end of primitives:
        nop
        nop

.include "words.s"

end_of_forth:
        nop
        nop

;----------------------------------------------------------------------
.end


