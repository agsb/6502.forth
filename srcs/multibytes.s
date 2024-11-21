
;-----------------------------------------------------------------------
; ND is 0 1 2 3 from lsb to msb, allow multi bytes
; ( w1 w2 w3 w4 -- (w1 w2 + w3 w4) )) 
def_word "ND+", "NDPLUS", 0
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
