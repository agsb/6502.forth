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
 
;/*
;note
;
;       09/11/2024        
;       review bios at 6502.toy, 
;               define $040, $140, $400, $1000 limits
;
;       06/11/2024
;       house clean
;       rewrite for not splited MSB LSB stacks. 
;       wise keep Y=0 in next, 
;       keep X as dedicated data stack index.
;       keep return stack at hard stack
;        
;       05/11/2024
;       rewrite for use stacks at pages zero and one
;               due overhead at @ and !
;
;       03/11/2024
;       using absolute splited stacks. 
;       interrupt service from R65F11, 
;       also http://wilsonminesco.com/6502primer/IRQconx.html
;           
;todo
;       still no vocabularies.
;
;
;*/

;-----------------------------------------------------------------------
;
;	A((nother)) Forth for 6502 with
;	data stack in page zero,
;	return stack in page one
;	and using minimal thread code
;	eForth and Fig-Forth, alike.
;	
;	uses X as data stack index, 
;       Y as keeped zero 
;	
;	((still)) non-rellocable code, 
;	((still)) non-optimized code,
;	
;	FALSE is $0000 (0)) TRUE is $FFFF ((-1))
;
;	SP0 and RP0 uses $FF as botton
;	
;	stacks grows backwards, as -2 -1 0 1 2 3 ...
;	0 is TOS, 1 is NOS, 2 is SOS, 3 is FORGET.
;	
;	Mixed header and code dictionary, no need CFA
;
;       the next cell after c-name is always a CFA
;
;	No need of DOCOL at CFA, vide MTC
;	
;       header:
;               link:   .word
;               byte:   .size_flag
;               name:   .byte * lenght
;       code:   
;               all bytes following name
;               are references or native code
;
;       Why another Forth? To learn how to.
;
;       for use in the 6502.toy SBC
;
;-----------------------------------------------------------------------
;
; ABOUT STACK NOTATION
;
; __The top of the stack is to the right.__, 
;       Forth 2012, Standart.
;
; __ OVER  ( n1 n2 — n1 n2 n1 )  Copies second item to top__, 
;       "Starting Forth", Leo Brodie
;
; I have a cognitive problem about stack notation.
;
; It represents the order of push into the stack, 
; not the order from top of stack.

; In standards 79, 83, 94, the cells in ( after -- before ), 
; should be arranged on the left as bottom and on the right as top.
;
; For example, when representing ( w1 w2 w3 -- ), the order 
; is be w3 at the top and w1 at the bottom.
;
; The cells indices for ( w1 w2 w3 -- ), are of w1 is 3, of w2 is 2,
; but for ( w1 w2 -- ), are of w1 is 2 and of w2 is 1.
;
; This is very confusing to me, when using the stack in words.
;
; I prefer to use w1 always as the top, and the following  
; indicating the position relative to the top of the stack.
;
; So w1 will always be 1, w2 will always be 2, and so on,
; the indices are always in the same order.
;
; To differentiate, I'm using double parentheses to indicate 
; that w1 is the last pushed into stack and always at top.
;
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;  init of ca65 assembler 
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
;   .byte  strlen((name)) + flags
;   .byte  name
;   name:
;
; label for primitives
.macro makelabel arg1, arg2
.ident ((.concat ((arg1, arg2)))):
.endmacro

; header for primitives
; the entry point for dictionary is h_~name~
; the entry point for code is ~name~
.macro def_word name, label, flag
makelabel "H_", label
.ident((.sprintf(("H%04X", hcount + 1)))) = *
.word .ident ((.sprintf (("H%04X", hcount))))
hcount .set hcount + 1
.byte .strlen((name)) + flag + 0 ; nice trick !
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
LENGTH = 15

; alias for user area

uVOID   = 0
uSO     = 2
uRO     = 4
uDP     = 6
uHP     = 8
uLAST   = 10
uLATEST = 12
uSTATE  = 14
uBASE   = 16
uTIB    = 18
uTOIN   = 20

uPAGE   = 22
uSCR    = 24
uBLK    = 26
uDEV    = 28

uTAIL   = 30
uHEAP   = 32

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

ip:     .word $0        ; instruction pointer
up:     .word $0        ; user pointer
dp:     .word $0        ; dictionary pointer, mixed header + code
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
;
; leave space for page zero, hard stack,
; and buffers, locals, stacks, etc
;
.segment "CODE"

* = $1000

boot:
        ; prepare hardware 
        nop
        jmp main

user:   .word $0    ; start of user area
stat:   .word $0    ; state of Forth, 0 is interpret, 1 is compiling
base:   .word $0    ; number radix for input and output
last:   .word $0    ; reference to last word, link field

here:   .word $0    ; here to compiling
back:   .word $0    ; keep the here while compiling
tibz:   .word $0    ; TIB
toin:   .word $0    ; TIB reference to next word

task:   .word $0
page:   .word $0
buff:   .word $0
head:   .word $0
tail:   .word $0

;----------------------------------------------------------------------

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

;   extended init functions 
        
;   init forth

        jmp cold

;---------------------------------------------------------------------
;  init of lib6502 emulator 
;---------------------------------------------------------------------
;
getchar:
        lda $E000

eofs:
; EOF ?
        cmp #$FF ; also clean carry :))
        beq byes

putchar:
        sta $E000
        rts

; exit for emulator
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
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; (( w1 -- w1 << 1 )) roll left, C=0, C -> b0, b7 -> C
def_word "RSFL", "RSFL", 0
        clc
        rol 0, x
        rol 1, x
        jmp next_

;-----------------------------------------------------------------------
; (( w1 -- w1 >> 1 )) roll right, C=0, C -> b7, b0 -> C
def_word "RSFR", "RSFR", 0
        clc
        ror 1, x
        ror 0, x
        jmp next_

;-----------------------------------------------------------------------
; (( w1 -- w1 << 1 )) arithmetic left, 0 -> b0, b7 -> C
def_word "2*", "ASFL", 0
        ; rolls
        clc
        rol 0, x
        rol 1, x
        bcc @ends
sign_:
        lda #$80
        ora 1, x
        sta 1, x
@ends:
        jmp next_

;-----------------------------------------------------------------------
; (( w1 -- w1 << 1 )) arithmetic right, C -> b7, b0 -> C
def_word "2/", "ASFR", 0
        ; get sign bit
        lda 1, x
        pha
        ; does a roll
        clc
        ror 1, x
        ror 0, x
        ; set sign bit
        pla
        bmi sign_
        jmp next_

;-----------------------------------------------------------------------
; (( w1 -- ))
def_word "DROP", "DROP", 0
        inx
        inx
        jmp next_

;-----------------------------------------------------------------------
; (( w1 -- w1 w1 )) 
def_word "DUP", "DUP", 0
        dex
        dex
        lda 2, x
        sta 0, x
        lda 3, x
        sta 1, x
        jmp next_

;-----------------------------------------------------------------------
; (( w1 w2 -- w2 w1 w2)) 
def_word "OVER", "OVER", 0
        dex
        dex
        lda 2, x
        sta 0, x
        lda 3, x
        sta 1, x
        jmp next_

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
        jmp next_

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
        jmp next_

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
        jmp next_

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
; (( -- 0x0000 )) 
def_word "FALSE", "FFALSE", 0
        dex
        dex
false2:
        lda #$00    ; false
same2_:
        sta 0, x
        sta 1, x
        jmp next_

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
        ora 1, x
        bne false2
        beq true2

;-----------------------------------------------------------------------
; (( w1 -- w1 < 0))) 
def_word "0<", "ZLT", 0
        lda 1, x
        asl
        bcc false2
        bcs true2

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 > w2 ))   
def_word "U<", "ULT", 0
        lda 3, x
        cmp 1, x
        bne @mcc
        lda 2, x 
        cmp 0, x
@mcc:
        inx
        inx
        bcs false2
        bcc true2

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 == w2 )) 
def_word "=", "EQ", 0
        inx 
        inx
        lda 0, x
        ora 254, x
        bne false2
        lda 1, x
        ora 255, x
        bne false2
        beq true2

;-----------------------------------------------------------------------
; (( a c -- )) *a = 0x00FF AND c
def_word "C!", "CSTORE", 0
        lda 2, x
        sta (0,x)
        clc
        bcc drop2

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
stow2:
        sta (0, x)
drop2:
        inx
        inx
        inx
        inx
        jmp next_

;-----------------------------------------------------------------------
; (( u a -- ))  
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
; (( w1 w2 -- == 0 )) 
def_word "D0=", "DZEQ", 0
        lda 0, x
        bne @noz
        ora 1, x
        bne @noz
        ora 2, x
        bne @noz
        ora 3, x
        bne @noz
@noz:
        clc
        beq @nozs
        sec
@nozs:
        inx
        inx
        bcc true2
        bcs false2

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- ((w1 w2 + w3 w4)) )) 
def_word "D+", "DPLUS", 0
        clc
        ldy #5
@loop:
        lda 0, x
        adc 4, x
        sta 4, x
        inx
        dey
        jmp next_

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- ((w1 w2 - w3 w4)) )) 
def_word "D-", "DMINUS", 0
        sec
        ldy #5
@loop:
        lda 0, x
        sbc 4, x
        sta 4, x
        inx
        dey
        bne @loop
        jmp mext_

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- ((w1 w2 - w3 w4)) )) ZZZZ 
def_word "D<", "DLTH", 0
        ldy #5
@loop:        
        lda 3, x
        cmp 7, x
        bne @cls
        dex
        dey
        bne @loop
        beq false2
@cls:
        bmi true2
        clc
        bcc false2


;-----------------------------------------------------------------------
; (( w1  -- w2 )) w2 = 0x00FF AND *w1 
def_word "C@", "CFETCH", 0
        lda (0,x)
        sta 0, x
        sty 0, x
        jmp next_

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
        jmp next_

;-----------------------------------------------------------------------
; (( w1  -- w2 )) w2 = w1+1 
def_word "1+", "ONEPLUS", 0
        inc 0, x
        bne @bne
        inc 1, x
@bne:
        jmp next_

;-----------------------------------------------------------------------
; (( w1  -- w2 )) w2 = w1-1 
def_word "1-", "ONEMINUS", 0
        lda 0, x
        bne @bne
        dec 1, x
@bne:
        dec 0, x
        jmp next_

;-----------------------------------------------------------------------
; (( w1  -- w2 )) w2 = w1+2 
def_word "2+", "TWOPLUS", 0
        ; next reference
        lda 0, x
        clc
        adc #$02
        sta 0, x
        bcc @bcc
        inc 1, x
@bcc:
        jmp next_
        
;-----------------------------------------------------------------------
; (( w1  -- w2 )) w2 = w1-2 
def_word "2-", "TWOMINUS", 0
        ; next reference
        lda 0, x
        sec
        sbc #$02
        sta 0, x
        bcc @bcc
        dec 1, x
@bcc:
        jmp next_
        
;-----------------------------------------------------------------------
; (( w -- w )) assure word is even, because CELL is 2 
def_word "ALIGN", "ALIGN", 0
        ldy 0, x
        iny
        tya
        ror
        rol
        sta 0, x
        jmp next_

;-----------------------------------------------------------------------
; (( w1 -- (($0000 - w1)) )) 
def_word "NEGATE", "NEGATE", 0
        lda #$00
        bne cpts_

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
        jmp next_

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
        jmp ((ip))

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
def_word "((DODOE))", "DODOES", 0
        ; zzzz
        jmp next_

;-----------------------------------------------------------------------
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
        jmp next_

;-----------------------------------------------------------------------
; (( -- ip )) eForth dovar, push IP++  
def_word "((DOVAR))", "DOVAR", 0
        dex
        dex
        lda ip + 0
        sta 0, x
        lda ip + 1
        sta 1, x
        jmp bump_

;-----------------------------------------------------------------------
; (( -- ((ip)) )) eForth, docon, push ((IP))++ 
def_word "((DOCON))", "DOCON", 0
        dex
        dex
        ldy #1
        lda ((ip)), y
        sta 1, x
        dey 
        lda ((ip)), y
        sta 0, x
        jmp bump_
        
;-----------------------------------------------------------------------
; (( -- ))    branch by offset, 16-bit signed  
def_word "BRANCH", "BRANCH", 0
bran_:
        ldy #1
        lda ((ip)), y
        sta wk + 1
        iny
        lda ((ip)), y
        sta wk + 0

        clc
        lda ip + 0
        adc wk + 0
        sta ip + 0
        lda ip + 1
        adc wk + 1
        sta ip + 1
        jmp next_

;-----------------------------------------------------------------------
; (( a c  -- n ))  skip c, n < 255 
def_word "CSKIP", "CSKIP", 0
        clc
        bcc csloop

;-----------------------------------------------------------------------
; (( a c  -- n ))  scan c, n < 255
def_word "CSCAN", "CSCAN", 0
        sec 
        bcs csloop

;-----------------------------------------------------------------------
csloop:
        lda #2
        jsr ds2ws_
        dex
        dex
        sty 1, x
@loop:
        lda ((two)), y
        eor one + 0
; skip or scan
        bcc @cseq
        beq @ends
        bne @csne
@cseq:
        bne @ends
@csne:
        iny
        bne @loop
@ends:
        sty 0, x
        jmp next_

;-----------------------------------------------------------------------
; (( a1 a2 n -- m )) 
; compare n bytes of a2 and a1, return how many equals, n < 255  
def_word "CSAME", "CSAME", 0
        lda #3
        jsr ds2ws_
        dex
        dex
        sty 1, x
@loop:
        lda ((two)), y
        eor ((six), y
        bne @ends
        iny
        cpy one + 0
        bne @loop
@ends:
        sty 0, x
        jmp next_

;-----------------------------------------------------------------------
; (( a1 a2 u  -- ))    move words, 16-bit signed  
def_word "MOVE", "MOVE", 0
        ; words to bytes
        asl 0, x 
        rol 1, x
        clc
        bcc cmove_

;-----------------------------------------------------------------------
; (( a1 a2 u  -- ))    move bytes, 16-bits  
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
        jmp next_
        
;----------------------------------------------------------------------
; (( w1 -- ))  code a word in ASCII, hexadecimal
def_word ".", "DOT", 0
	lda 0, x
	jsr puthex
	lda 1, x
	jsr puthex
	inx
	inx
	jmp next_

;----------------------------------------------------------------------
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
        jmp putchar
----------------------------------------------------------------------
;       code a ASCII $FF hexadecimal in a byte
;       not working
number:

	ldy #0

	jsr @very
	asl
	asl
	asl
	asl
	sta one + 1

	jsr @very
	ora one + 1
	sta one + 1
	
	jsr @very
	asl
	asl
	asl
	asl
	sta one + 0

	jsr @very
	ora one + 0
	sta one + 0

	clc ; clean
	rts

@very:
	lda ((tout)), y
	iny
	cpy #1
	bne @digt
        pha
	; negative sign ?
	cmp #'-'
	beq @very
	; positive sign ?
	cmp #'+'
        beq @very
@digt:
	sec
	sbc #$30	; test < ' '
	bmi @erro
	cmp #10		; test < 10
	bcc @endc
	sbc #$07	; adjust for letters
	; any valid digit, A-Z, do not care 
	cmp base + 0	; zzzz page zero ?
	bmi @endc
@erro:
        
@endc:
        pla
        cmp #'-'
        bne @ends

@ends:
	rts

;-----------------------------------------------------------------------
;
; Forth stuff:
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
        ; jmp next_

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
        cmp #>ends+1    ; init of heap compose dictionary
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
        jmp next_

;-----------------------------------------------------------------------
jump_:  ; creed, do the jump
        
        jmp ((wk))
        ; alternatve for list the primitives
        ; jsr puthex
        ; jmp next_

;-----------------------------------------------------------------------
; process a interrupt, could be a pool, void for now
hang_:
        lda #$C0
        sta intflag
        jmp look_

;-----------------------------------------------------------------------
;       one byte constants
;-----------------------------------------------------------------------

; (( -- w1 )) index of SP0 
def_word "SP@", "SPAT", 0
        txa
lsbs_:
        dex
        dex
        sta 0, x
        sty 1, x
        jmp next_

;-----------------------------------------------------------------------
; (( -- w1 ))  index of RP0
def_word "RP@", "RPAT", 0
        stx np + 0
        tsx
        txa
        ldx np + 0
        bne lsbs_

;-----------------------------------------------------------------------
; (( -- w))  
def_word "0", "ZERO", 0
        lda #0
        beq lsbs_

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
def_word "CR", "CR", 0
        lda #$10
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
;       user variables
;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal dictionary heap
def_word "UP", "UP", 0
        ldy #0
upsv_:
        dex
        dex
        lda ((up)), y
        sta 0, x
        iny
        lda ((up)), y
        sta 1, x
        jmp next_

;-----------------------------------------------------------------------
; (( -- ))  used to reset S0
def_word "SP!", "TOSP", 0
        ldy #uS0
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- ))  used to reset R0
def_word "RP!", "TORP", 0
        ldy #uR0
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "DP", "DP", 0
        ldy #uDP
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
; must hold at least 84 chars, but 72 is enough
def_word "TIB", "TIB", 0
        ldy #uTIB
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
; 
def_word "TOIN", "TOIN", 0
        ldy #uTOIN
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
def_word "LATEST", "LATEST", 0
        ldy #uLATEST
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal  
def_word "LAST", "LAST", 0
        ldy #uLAST
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal 
def_word "STATE", "STATE", 0
        ldy #uSTATE
        clc
        bcc upsv_

;-----------------------------------------------------------------------
; (( -- w ))  reference of forth internal
def_word "BASE", "BASE", 0
        ldy #uSTATE
        clc
        bcc upsv_

;----------------------------------------------------------------------
; common must
;
cold:

;----------------------------------------------------------------------
warm:

; copy block rom to ram ?
; init variables ?

; only lsb
        lda #$00FF
        sta up + uS0
        lda #$01FF
        sta up + uR0

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

abort:
        ldx #$FF

quit:
        txs

        jmp 0000

;-----------------------------------------------------------------------
; BEWARE, MUST BE AT END OF PRIMITIVES ! 
; MINIMAL THREAD CODE DEPENDS ON IT !
ends:
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;   COMPOSE WORDS, pre-compiled
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; (( w1 w2 -- ((w1 < w2)) )) 
def_word "<", "LTH", 0
        .word INVERT, PLUS, ZLT
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 -- ((w1 > w2)) )) 
def_word ">", "GTH", 0
        .word SWAP, LTH
	.word EXIT 

;-----------------------------------------------------------------------
; (( w1 w2 -- ((w2)) ((w3)) ))     
def_word "2@", "TWOAT", 0
        .word DUP, CELL, PLUS, FETCH, SWAP, FETCH
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 w3 --  ))     
def_word "2!", "TWOTO", 0
        .word SWAP, OVER, STORE, CELL, PLUS, STORE
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 --  ))     
def_word "2R>", "TWORTO", 0
        .word RTO, RTO, SWAP
	.word EXIT

;-----------------------------------------------------------------------
; (( -- w1 w2 ))     
def_word "2>R", "TWOTOR", 0
        .word SWAP, TOR, TOR
	.word EXIT

;-----------------------------------------------------------------------
; (( -- w1 w2 ))     
def_word "2R@", "TWORPAT", 0
        .word RTO, RTO, TWODUP, TWORTO
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 --  ))     
def_word "2DROP", "TWODROP", 0
        .word DROP, DROP
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2  -- w1 w2 w1 w2  ))     
def_word "2DUP", "TWODUP", 0
        .word OVER, OVER
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 w2 w1 w2   ))     
def_word "2OVER", "TWOOVER", 0
        .word TWOTOR, TWODUP, TWORTO, TWOSWAP
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- w3 w4 w1 w2 ))     
def_word "2SWAP", "TWOSWAP", 0
        .word ROT, TOR, ROT, RTO
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 --  w1 + CELL ))     
def_word "CELL+", "CELLPLUS", 0
        ; cells is two 
        .word TWOPLUS
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 -- w1 * 2 ))     
; cells is two 
def_word "CELLS", "CELLS", 0
        .word ASFL
	.word EXIT

;-----------------------------------------------------------------------
; dictionary stuff
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; (( -- ))    find a word in dictionary, return CFA or FALSE (0x0))  
; zzzz
def_word "((FIND))", "FINDF", 0
        .word DROP
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))      
def_word "HERE", "HERE", 0
        .word DP, FETCH
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))  
def_word "DP+", "DPADD", 0
        .word HERE, PLUS, DP, STORE
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))      
def_word "ALLOC", "ALLOC", 0
        .word CELLS, DPADD
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))   
def_word ",", "COMMA", 0
        .word HERE, STORE, CELL, DPADD
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))   
def_word "'", "TICK", 0
        .word FINDF
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))   
def_word "POSTPONE", "POSTPONE", IMMEDIATE
        .word TICK, COMMA
	.word EXIT

;----------------------------------------------------------------------
; (( -- ))  state mode compile
def_word "[", "LBRAC", 0
        .word ZERO, STATE, STORE
	.word EXIT

;----------------------------------------------------------------------
; (( -- ))  state mode interprete
def_word "]", "RBRAC", 0
        .word ONE, STATE, STORE
	.word EXIT

;----------------------------------------------------------------------
; (( a1 -- a2 n ))  word from buffer, 
;       return size and address, update toin 
;
;       DOES NOT HANDLE END OF LINE
;       classic, copy from TOIN to TIB, 
;       tib is a internal 32 byte buffer
;       WORD putw in TIB or HERE ???
;       toin is a external >80 bytes buffer
;       when empty refill, or accept, or expect

def_word "WORD", "WORD", 0
        .word TOIN, FETCH, 
        .word DUP, BL, CSKIP, PLUS   
        .word DUP, BL, CSCAN, OVER, OVER 
        .word PLUS, TOIN, STORE 
	.word EXIT

;----------------------------------------------------------------------
; (( w1 -- ))  code a word in ASCII hexadecimal, ZZZZ
def_word "CREATE", "CREATE", 0
        .word BL, WORD, HERE, CMOVE 
        .word EXIT

;----------------------------------------------------------------------
; (( w1 -- ))  code a word in ASCII hexadecimal, ZZZZ
def_word "ACCEPT", "ACCEPT", 0
        .word EXIT
;----------------------------------------------------------------------
; (( w1 -- ))  compile a word
def_word ":", "COLON", 0
	.word HERE, LAST, STORE
	.word LATEST, FETCH, COMMA
        .word CREATE, RBRAC
        .word EXIT

;----------------------------------------------------------------------
; (( w1 -- ))  ends a compile word
def_word ";", "SEMIS", 0
	.word DOCON, EXIT, COMMA
        .word LAST, FETCH, LATEST, STORE, LBRAC
        .word EXIT

;-----------------------------------------------------------------------
;       eForth alike, all offsets 
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; (( -- ))     
def_word "IF", "IF", IMMEDIATE
        .word DOCON, QBRANCH, COMMA
        .word HERE, ZERO, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "THEN", "THEN", IMMEDIATE
        .word HERE, OVER, MINUS, SWAP, STORE
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "ENDIF", "ENDIF", IMMEDIATE
        .word DOCON, THEN, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "END", "END", IMMEDIATE
        .word DOCON, UNTIL, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "ELSE", "ELSE", IMMEDIATE
        .word DOCON, BRANCH, COMMA
        .word HERE, ZERO, COMMA
        .word SWAP, THEN
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "BEGIN", "BEGIN", IMMEDIATE
        .word HERE
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "AGAIN", "AGAIN", IMMEDIATE
        .word DOCON, BRANCH, COMMA
        .word HERE, MINUS, COMMA
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))     
def_word "UNTIL", "UNTIL", IMMEDIATE
        .word DOCON, QBRANCH, COMMA 
        .word HERE, MINUS, COMMA
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))     
def_word "WHILE", "WHILE", IMMEDIATE
        .word IF, TWOPLUS
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))     
def_word "REPEAT", "REPEAT", IMMEDIATE
        .word DOCON, AGAIN, COMMA
        .word HERE, SWAP, STORE
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- )) counts down 
def_word "FOR", "FOR", IMMEDIATE
        .word DOCON, TOR, COMMA, HERE
	.word EXIT

;-----------------------------------------------------------------------
; (( -- )) until zero     
def_word "NEXT", "NEXT", IMMEDIATE
        .word DOCON, DONEXT, COMMA
        .word DOCON, UNTIL, COMMA
        .word EXIT

;-----------------------------------------------------------------------
; (( -- )) decrements the counter and verify
def_word "DONEXT", "DONEXT", 0
        .word RTO, ONEMINUS, DUP
        .word IF, TOR, FALSE
        .word ELSE, INVERT
        .word THEN
	.word EXIT

;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; BEWARE, MUST BE AT END OF PRE-POSTPONED-COMPOSED ! 
;-----------------------------------------------------------------------
; (( w1 -- w2 w3 ))     
def_word "NULL", "NULL", 0
        .word FALSE
	.word EXIT

;-----------------------------------------------------------------------
; END OF CODE
.end

;-----------------------------------------------------------------------
; search the dictionary for a word, returns the cfa or null
;

create_:

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
        ; copyfrom one, two

; update to c-name    
        ; addtwo one

; compare words
        ldy #0

; save the flag, MSB of state is ((size and flag)) 
        lda ((one)), y
        sta stat + 1

; compare chars
@equal:
        lda ((two)), y
; lte space ends
        cmp #((32 + 1))  
        bmi @done

; verify 
        sec
        sbc ((one)), y     
; clean 7-bit ascii
        asl        
        bne @loop

; next char
        iny
        bne @equal

@done:
; update to code
        tya
        ; addacm one

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
        sta ip + 1
        lda #<outerptr
        sta ip + 0

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
        sta ((tibz)), y
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
        sta ((tibz)), y
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
        sta ((toin)), y
        jsr getchar
        bmi @ends
        bne @scan

@ends:  ;  store lenght at head
        dey
        tya
        ldy #0
        sta ((toin)), y
        rts

;----------------------------------------------------------------------
nums_:
        ; parse a number
        rts

;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; to reviews
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
        jmp next_

;-----------------------------------------------------------------------
; Divide the top 2 cell of the stack
; http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
; dividend divisor -- result remainder
; (( six ten -- two one ))
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
; (( multiplicand multiplier -- resultMSW resultLSW ))
; (( six ten -- two one ))
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

.end

;-----------------------------------------------------------------------
; (( w1 w2 w3 -- w2 w3 w1 ))     
def_word "ROT2", "ROTF2", 0
        .word TOR, SWAP, RTO, SWAP
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 w3 -- w3 w1 w2 ))     
def_word "-ROT2", "ROTB2", 0
        .word SWAP, TOR, SWAP, RTO
	.word EXIT

