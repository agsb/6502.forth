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

;-----------------------------------------------------------------------
; A is for dummy
; X is the data stack index
; Y is zero almost time, by next_
; stacks grows backwards, push decreases, pull increases
; notation: left is the TOP
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; compare bytes, backwards, return zero if equal, n < 255  
; input: from (six), into (two), size (one+0)
; output: many not equal (one+1)
csame:
        ldy one + 0
@loop:
        lda (two), y
        eor (six), y
        bne @ends
        dey
        bne @loop
@ends:
        ; equals if zero
        sty one + 1
        rts

;-----------------------------------------------------------------------
;   copy bytes, forward, from (six), into (two), many (one)
ccopy_:
        ldy #0
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
        rts
        
;----------------------------------------------------------------------
; ( w -- )  code a word, in ASCII, hexadecimal
dotw_:
	lda one + 1
	jsr puthex
	lda one + 0
	jsr puthex
        rts

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
;   calculate a value for word, from a hexadecimal ascii FFFF
;   input: from (two), offset (y)
;   output: value (one), offset (Y) advanced
numb_:

        lda (two), y
        iny
	
        jsr gethex
        bmi @erro
	asl
	asl
	asl
	asl
	sta one + 1

        lda (two), y
        iny
	
        jsr gethex
        bmi @erro
	ora one + 1
        sta one + 1
	
        lda (two), y
        iny
	
        jsr gethex
        bmi @erro
	asl
	asl
	asl
	asl
	sta one + 0

        lda (two), y
        iny

	jsr gethex
        bmi @erro
        ora one + 0
	sta one + 0
        
        clc
@valid:
        rts
@erro:
        sec
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
digit:
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
; make uppercase, from (two), many (one) < 255
uppr_:
        ldy #0
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
        rts

;----------------------------------------------------------------------
; receive an ascii line, stores a asciiz line, n < 255
; appends \b ... \0
; input: into (two), size (one) < 255, 
; output: many (one) 
expect:
        ldy #0
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
; append a \0
        lda #0
        sta (two), y
; how many stored
        sty one + 0
        stz one + 1
        rts

;---------------------------------------------------------------------
; receive a word between spaces to a buffer as c-str
; input: from (two), separator (one)
; output: starts (one+0), stops (one+1)
;
word:
        ldy #255
        
@skip:  ; skip spaces
        iny
        lda (two), y
        beq @ends       ; ends at \0
        cmp one + 0
        beq @skip
        phy

@scan:  ; scan spaces
        iny
        lda (two), y
        beq @ends       ; ends at \0
        cmp one + 0
        bne @scan
@ends:  
        ; stops 
        sty one + 1
        ply
        ; starts
        sty one + 0
        rts

;---------------------------------------------------------------------

;---------------------------------------------------------------------
; the outer loop

resolvept:
        .word okey

;---------------------------------------------------------------------
okey:

;   uncomment for feedback
;       lda stat + 0
;       bne resolve
;       lda #'O'
;       jsr putchar
;       lda #'K'
;       jsr putchar
;       lda #10
;       jsr putchar

resolve:
; get a token
        jsr word

;-----------------------------------------------------------------------
find_:

; load latest link
        lda latest + 1
        sta six + 1
        lda latest + 0
        sta six + 0
        
@loop:
; lsb linked list
        lda six + 0
        sta wk + 0
        lda six + 1
        sta wk + 1

; verify \0x0
        ora wk + 0
        beq @ends

@each:    
; update next link 
        ldy #0
        lda (wk), y
        sta six + 0
        iny
        lda (wk), y
        sta six + 1

; update to c-name    
        clc
        lda #2
        adc wk + 0
        sta wk + 0
        bcc @bcc
        inc wk + 1
@bcc:

; compare chars
        lda one + 0
        tay
        iny
        ; backwards
@equal:
        dey
        beq @ends
        lda (two), y
        cmp (wk), y
        beq @equal
        bra @loop

@ends:
; save the flag, MSB of state is (size and flag) 
        lda (wk), y
        sta state + 1

; update to cfa
        clc
        lda wk + 0
        adc one + 0
        sta wk + 0
        bcc @done
        inc wk + 1
@done:
        
;-----------------------------------------------------------------------
evaluate:
; executing ? if == \0
        lda stat + 0   
        beq execute

; immediate ? if < \0
        lda stat + 1   
        bmi immediate      

compile:

        jsr wcomma

        bra resolve

immediate:
execute:

        lda #>resolvept
        sta ipt + 1
        lda #<resolvept
        sta ipt + 0
        bra pick

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
; ( -- ) just the first word
def_word "NULL", "NULL", 0
         nop
         nop
         nop
         nop
        ;goto next
        release

;----------------------------------------------------------------------
; (  -- c )  for use  
def_word "KEY", "KEY", 0
        jsr getc
        dex
        dex
        sta 0, x
        sty 1, x
        ;goto next_
        release

;----------------------------------------------------------------------
; ( c -- )  for use  
def_word "EMIT", "EMIT", 0
        lda 0, x
        jsr putc
        inx
        inx
        ;goto next_
        release

;----------------------------------------------------------------------

;-----------------------------------------------------------------------
; (( w1 -- ))  EXECUTE is done by nest in MTC
def_word "EXECUTE", "EXECUTE", 0
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
; Minimal Thread Code Forth
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
        
;---------------------------------------------------------------------

;---------------------------------------------------------------------
comma:
    	ldy #(wrd)
    	ldx #(here)

    	lda 0, y
    	sta (0, x)
    	jsr incwx
    	lda 1, y
    	sta (0, x)
    	jmp incwx
	rts

;---------------------------------------------------------------------
; ( -- ) zzzz
def_word ";", "semis",  FLAG_IMM
; update last, panic if colon not lead elsewhere 
        lda back + 0 
        sta last + 0
        lda back + 1 
        sta last + 1

; stat is 'interpret'
        lda #0
        sta stat + 0

; compound words must ends with exit
finish:
        lda #<exit
        sta wrd + 0
        lda #>exit
        sta wrd + 1
        jsr wcomma

        ; jmp next
        bra next    ; always taken

;---------------------------------------------------------------------
; ( -- ) zzzz
def_word ":", "colon", 0
; save dp, panic if semis not follow elsewhere
        lda dp + 0
        sta last + 0 
        lda dp + 1
        sta last + 1 

; stat is 'compile'
        lda #1
        sta state + 0

@header:
; copy last into (here)
        ldy #1
        lda last + 1
        sta (here), y
        dey
        lda last + 0
        sta (here), y

; get following token
        jsr token

; copy it
        ldy #0
@loop:  
        lda (tout), y
        cmp #33    ; stops at controls
        bmi @ends
        sta (here), y
        iny
        bne @loop

ends:
; update here
        tya
        clc
        adc here + 0
        sta here + 0
        bcc @bccs
        inc here + 1
@bccs:

; done
        ; jmp next
        bra next    ; always taken

;----------------------------------------------------------------------
;
; common
;
;-----------------------------------------------------------------------
; ( -- )  reference of forth internal
def_word "COLD", "COLD", 0
cold:

;-----------------------------------------------------------------------
; ( -- )  reference of forth internal
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


