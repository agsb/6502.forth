; use of multibytes, tos is MSB deep is LSB 
;-----------------------------------------------------------------------
; ND is 0 1 2 3 from lsb to msb, allow multi bytes
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
