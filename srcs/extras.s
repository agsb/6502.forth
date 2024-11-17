
; snipets of code to review

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

; .byte $02 will halt

;-----------------------------------------------------------------------
; verify if is ram or rom
inrom:
        nop
        lda inrom
        inx inrom
        cmp inrom
        bne isram
isrom:
        ; clear overflow bit
        clv       
        rts
isram:
        ; set overflow bit
setovr_:
        bit @ends
@ends:
        rts

where_me:
        jsr @oops
@oops:
        pla
        tay
        iny
        pla

; where I am, which 256 page.
where_i_am:
        jsr @oops
        tsx
        lda $100, x
        .byte $2C
@oops:   
        nop
        rts

; Z flag is zero in NMOS6502
nmos_:
        sed
        clc
        lda #$99
        adc #$01
        cld
        rts

messages:
        jsr display
        .asciiz "this message is for display"
        rts

display:
        pla 
        sta message + 0
        pla 
        sta message + 1
        ldy #1  ; stack is one less
@loop:
        lda (message), y
        inc message + 0
        bne @bnes
        inc message + 1
@bnes:
        ora #0
        beq @ends
        jsr putch
        bne @loop
@ends:
        lda message + 1
        pha
        lda message + 0
        pha
        rts

;-----------------------------------------------------------------------
;       beware, self modify, non relocable code
;       overhead (bytes/cycles) 2/2 + 2/2 + 3/4 + 3/4 == 10/12 
;       lower size by 17 * 4 = 68 less bytes
;-----------------------------------------------------------------------
; (( w1 w2 -- w1 OPC w2 )) 
def_word "OPC", "OPCo", 0
opc_a:
        sta opc_b
        sta opc_c
        lda 2, x
opc_b:
        and 0, x
        sta 2, x
        lda 3, x
opc_c:
        and 1, x
        sta 3, x
        inx
        inx
        jmp next_

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 AND w2 )) 
def_word "AND", "ANDo", 0
        lda #$35
        bne opc_a
        
;-----------------------------------------------------------------------
; (( w1 w2 -- w1 AND w2 )) 
def_word "OR", "ORo", 0
        lda #$15
        bne opc_a
        
;-----------------------------------------------------------------------
; (( w1 w2 -- w1 AND w2 )) 
def_word "EOR", "EORo", 0
        lda #$55
        bne opc_a
        
;-----------------------------------------------------------------------
; (( w1 w2 -- w1 AND w2 )) 
def_word "+", "PLUSo", 0
        lda #$75
        bne opc_a
        
;-----------------------------------------------------------------------
; (( w1 w2 -- w1 AND w2 )) 
def_word "-", "MINUSo", 0
        lda #$F5
        bne opc_a

;-----------------------------------------------------------------------

