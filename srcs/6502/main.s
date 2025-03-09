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

; identifiers 6502
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

;-----------------------------------------------------------------------
;   constants
;-----------------------------------------------------------------------

; highlander, reserved flag.
; FLAG_RSV = 1<<5
FLAG_RSV = 0

; highlander, compiler flag.
; FLAG_CPL = 1<<6
FLAG_CPL = 0

; highlander, immediate flag.
FLAG_IMM = 1<<7

MASK_FLAG = .NOT (FLAG_IMM .OR FLAG_CPL .OR FLAG_RSV) 

; maximum length of words
LENGTH = 32

; cell size, two bytes, 16-bit
CELL = 2

; terminal input buffer
TIB_SIZE = $54

; pad buffer
PAD_SIZE = $54

; sizeof a buffer
BLK_SIZE = 1024

; logical false
FALSE = 0

; logical true
TRUE = -1

;-----------------------------------------------------------------------
; For use of 6502.toy, reserved for bios
;
; $0000 to $003F 
; $0100 to $013F 
; $0200 to $02FF
; $0300 to $03FF

;-----------------------------------------------------------------------
.segment "ZERO"

* = $A0 

; depends on BIOS

status:        .byte $0
irqreq:        .byte $0
irqjmp:        .addr $0

; forth variables

up:     .addr $0        ; user pointer
dp:     .addr $0        ; dictionary pointer, mixed header + code
ip:     .addr $0        ; instruction pointer
wk:     .addr $0        ; fixed above np

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
toin:           .word $0        ; reference to next word

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

.include "minimal.s"

end_of_minimal: ;       end of minimals

.include "native.s"

end_of_native: ;        end of primitives

.include "compiled.s"

end_of_compiled: ;      end of primitives

; .include "extras.s"

end_of_forth:

.byte $DE, $AD, $C0, $DE

;----------------------------------------------------------------------
.end

