;-----------------------------------------------------------------------
; ( w1 w2 -- ) 
def_word "2DROP", "TWODROP", 0
    inc spi
    inc spi
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 w2 w1 w2 ) 
def_word "2DUP", "TWODUP", 0
    ldx spi
    dec spi
    dec spi
    lda sp0 + 0, x
    sta sp0 - 2, x
    lda sp0 + 0 + sps, x
    sta sp0 - 2 + sps, x
    lda sp0 + 1, x
    sta sp0 - 1, x
    lda sp0 + 1 + sps, x
    sta sp0 - 1 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 -- w3 w4 w1 w2 ) 
def_word "2SWAP", "TWOSWAP", 0
    ldx spi
    lda sp0 + 0, x
    sta sp0 - 2, x
    lda sp0 + 0 + sps, x
    sta sp0 - 2 + sps, x
    lda sp0 + 1, x
    sta sp0 - 1, x
    lda sp0 + 1 + sps, x
    sta sp0 - 1 + sps, x
    lda sp0 + 2, x
    sta sp0 + 0, x
    lda sp0 + 2 + sps, x
    sta sp0 + 0 + sps, x
    lda sp0 + 3, x
    sta sp0 + 1, x
    lda sp0 + 3 + sps, x
    sta sp0 + 1 + sps, x
    lda sp0 - 1, x
    sta sp0 + 3, x
    lda sp0 - 1 + sps, x
    sta sp0 + 3 + sps, x
    lda sp0 - 2, x
    sta sp0 + 2, x
    lda sp0 - 2 + sps, x
    sta sp0 + 2 + sps, x
    jmp next_

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 -- w3 w4 w1 w2 w3 w4 ) 
def_word "2OVER", "TWOOVER", 0
    ldx spi
    dec spi
    dec spi
    lda sp0 + 2, x
    sta sp0 - 2, x
    lda sp0 + 2 + sps, x
    sta sp0 - 2 + sps, x
    lda sp0 + 3, x
    sta sp0 - 1, x
    lda sp0 + 3 + sps, x
    sta sp0 - 1 + sps, x
    jmp next_

