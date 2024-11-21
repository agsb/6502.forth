
;-----------------------------------------------------------------------
;   COMPILED WORDS
;-----------------------------------------------------------------------
init_of_compiled:

;-----------------------------------------------------------------------
;       core stuff
;-----------------------------------------------------------------------
; ( w1 w2 -- w1 < w2 ) 
def_word "<", "LTH", 0
        .word MINUS, ZLT
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 -- w1 > w2 ) 
def_word ">", "GTH", 0
        .word SWAP, LTH
	.word EXIT 

;-----------------------------------------------------------------------
; ( w --  w + CELL )     
def_word "CELL+", "CELLPLUS", 0
        ; cells is two 
        .word TWOPLUS
	.word EXIT

;-----------------------------------------------------------------------
; ( w -- w * 2 )     
def_word "CELLS", "CELLS", 0
        ; cells is two 
        .word ASFL
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 -- w2 w1 w2 )     
def_word "TUCK", "TUCK", 0
        .word SWAP, OVER
	.word EXIT

;-----------------------------------------------------------------------
;       dword stuff
;-----------------------------------------------------------------------
; ( w1 w2 -- w1 w2 )     
def_word "2@", "DAT", 0
        .word DUP, CELL, PLUS, FETCH, SWAP, FETCH
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 --  )     
def_word "2!", "DTO", 0
        .word SWAP, OVER, STORE, CELL, PLUS, STORE
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 --  )     
def_word "2R>", "DRTO", 0
        .word RTO, RTO, SWAP
	.word EXIT

;-----------------------------------------------------------------------
; ( -- w1 w2 )     
def_word "2>R", "DTOR", 0
        .word SWAP, TOR, TOR
	.word EXIT

;-----------------------------------------------------------------------
; ( -- w1 w2 ) (R: w1 w2 -- w1 w2 )    
def_word "2R@", "DRAT", 0
        .word RTO, RTO, DDUP, DRTO
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 -- )     
def_word "2DROP", "DDROP", 0
        .word DROP, DROP
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2  -- w1 w2 w1 w2 )     
def_word "2DUP", "DDUP", 0
        .word OVER, OVER
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 -- w1 w2 w3 w4 w1 w2 )     
def_word "2OVER", "DOVER", 0
        .word DTOR, DDUP, DRTO, DSWAP
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 -- w3 w4 w1 w2 )     
def_word "2SWAP", "DSWAP", 0
        .word ROT, TOR, ROT, RTO
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 w5 w6 -- w5 w6 w1 w2 w3 w4 )     
def_word "2ROT", "DROTF", 0
        .word TOR, SWAP, RTO, SWAP
	.word EXIT

;-----------------------------------------------------------------------
; ( w1 w2 w3 w4 w5 w6 -- w3 w4 w5 w6 w1 w2 )     
def_word "-2ROT", "DBROT", 0
        .word SWAP, TOR, SWAP, RTO
	.word EXIT

;-----------------------------------------------------------------------
; dictionary stuff
;-----------------------------------------------------------------------

; ( -- w )      ; zzzz
def_word "BLOCK", "BLOCK", 0 
        .word TIB
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )      
def_word "VARIABLE", "VARIABLE", IMMEDIATE
        .word CREATE, LIT, DOVAR, COMMA, LIT, ZERO, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; ( w -- )      
def_word "CONSTANT", "CONSTANT", IMMEDIATE
        .word CREATE, LIT, DOCON, COMMA, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; ( -- a )      
def_word "HERE", "HERE", 0
        .word DP, FETCH
	.word EXIT 

;-----------------------------------------------------------------------
; ( u -- )      
def_word "ALLOT", "ALLOT", 0
        .word DP, PLUSTO
	.word EXIT

;-----------------------------------------------------------------------
; ( c -- )   
def_word "C,", "CCOMMA", 0
        .word HERE, CSTORE, ONE, ALLOT
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )   
def_word ",", "COMMA", 0
        .word HERE, STORE, TWO, ALLOT
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )   
def_word "'", "TICK", 0
        .word FINDF
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )   easy way 
def_word "POSTPONE", "POSTPONE", IMMEDIATE
        .word TICK, COMMA
	.word EXIT

;----------------------------------------------------------------------
; ( -- )  state mode compile
def_word "[", "LBRAC", 0
        .word ZERO, STATE, STORE
	.word EXIT

;----------------------------------------------------------------------
; ( -- )  state mode interprete
def_word "]", "RBRAC", 0
        .word ONE, STATE, STORE
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))    find a word in dictionary, return CFA or FALSE (0x0))  
def_word "FINDF", "FINDF", 0
        .word DROP
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))    find a word in dictionary, return CFA or FALSE (0x0))  
def_word "PFIND", "PFIND", 0
        .word DROP
	.word EXIT

;----------------------------------------------------------------------
; (( c --  )) classic, c delimiter 
def_word "WORD", "WORD", 0
        .word BLK, FETCH
        .word QBRANCH, 10, BLK, FETCH, BLOCK
        .word BRANCH, 6, TIB, FETCH
        .word TOIN, FETCH, PLUS, SWAP, ENCLOSE
        .word TOIN, PLUSTO, MINUS, TOR
        ; header
        .word HERE, LAST, STORE
        .word LATEST, COMMA 
        .word RAT, HERE, CSTORE
        .word HERE, ONEPLUS, RTO
        .word CMOVE
	.word EXIT

;----------------------------------------------------------------------
; (( ???? --  ))
; to review
def_word "CREATE", "CREATE", 0
        .word BL, WORD, HERE, CMOVE 
        .word EXIT

;----------------------------------------------------------------------
; (( -- ))  ZZZZ
def_word "ACCEPT", "ACCEPT", 0
        .word EXIT

;----------------------------------------------------------------------
; ( -- )  compile a word
def_word ":", "COLON", 0
	.word HERE, LAST, STORE
	.word LATEST, FETCH, COMMA
        .word CREATE, RBRAC
        .word EXIT

;----------------------------------------------------------------------
; ( -- )  ends a compile word
def_word ";", "SEMIS", 0
	.word DOCON, EXIT, COMMA
        .word LAST, FETCH, LATEST, STORE, LBRAC
        .word EXIT

;-----------------------------------------------------------------------
;       old school, offset branches
;-----------------------------------------------------------------------
; ( -- )     
def_word "BEGIN", "BEGIN", IMMEDIATE
        .word HERE
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )     
def_word "AGAIN", "AGAIN", IMMEDIATE
        .word LIT, BRANCH, COMMA
        .word HERE, MINUS, COMMA
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))     
def_word "UNTIL", "UNTIL", IMMEDIATE
        .word LIT, QBRANCH, COMMA 
        .word HERE, MINUS, COMMA
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))  eForth AHEAD ???? offset 
def_word "GOTO", "GOTO", IMMEDIATE
        .word LIT, BRANCH, COMMA
        .word HERE, ZERO, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))  eForth AFT ???? offset
def_word "AFT", "AFT", IMMEDIATE
        .word DROP 
        .word LIT, GOTO, COMMA
        .word LIT, BEGIN, COMMA
        .word SWAP
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "IF", "IF", IMMEDIATE
        .word LIT, QBRANCH, COMMA
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
        .word THEN
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "ELSE", "ELSE", IMMEDIATE
        .word LIT, BRANCH, COMMA
        .word HERE, ZERO, COMMA
        .word SWAP, THEN
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "END", "END", IMMEDIATE
        .word UNTIL
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "WHILE", "WHILE", IMMEDIATE
        .word IF, TWOPLUS
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- ))     
def_word "REPEAT", "REPEAT", IMMEDIATE
        .word LIT, AGAIN, COMMA
        .word HERE, SWAP, STORE
	.word EXIT 

;-----------------------------------------------------------------------
; (( -- )) counts down 
def_word "FOR", "FOR", IMMEDIATE
        .word LIT, TOR, COMMA, HERE
	.word EXIT

;-----------------------------------------------------------------------
; (( -- )) until zero     
def_word "NEXT", "NEXT", IMMEDIATE
        .word LIT, DONEXT, COMMA
        .word LIT, UNTIL, COMMA
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
; (( -- ))  
def_word "DOES", "DOES", 0
        ; zzzz
        .word EXIT

;-----------------------------------------------------------------------
; (( -- ))  
def_word "VALUE", "VALUE", 0
        .word CREATE, LIT, DOCON, COMMA 
        .word EXIT         

;-----------------------------------------------------------------------
; (( -- ))  
def_word "TO", "TO", 0
        .word TICK, TWOPLUS, COMMA 
        .word EXIT         

;-----------------------------------------------------------------------
; (( -- ))  
def_word "DEFER", "DEFER", IMMEDIATE
        .word CREATE 
        .word LIT, NOOP, COMMA 
        .word LIT, EXIT, COMMA 
        .word EXIT         

;-----------------------------------------------------------------------
; (( -- ))  
def_word "IS", "IS", IMMEDIATE
        .word POSTPONE
        .word EXIT         

;-----------------------------------------------------------------------
; (( w1 -- w2 w3 ))     
def_word "?", "QST", 0
        .word FETCH, DOT
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 -- w1 ))     
def_word "NOOP", "NOOP", 0
        .word FALSE
	.word EXIT

;-----------------------------------------------------------------------
def_word "USTRING", "USTRING", 0
        .word EXIT

;-----------------------------------------------------------------------
def_word "BUBUF", "BUBUF", 0
        .word EXIT

;-----------------------------------------------------------------------
def_word "XTIB", "XTIB", 0
        .word EXIT

;-----------------------------------------------------------------------
def_word "UMAX", "UMAX", 0
        .word EXIT

;-----------------------------------------------------------------------
def_word "UMIN", "UMIN", 0
        .word EXIT

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
def_word "STREAM", "STREAM", 0
        .word BLK, FETCH, QDUP
        .word QBRANCH, 6, BLOCK, BUBUF
        .word BRANCH, 8, TIB, XTIB, FETCH
        .word TOIN, FETCH, OVER, UMIN, USTRING 
        .word EXIT

;-----------------------------------------------------------------------
; error messages
erro1:  .asciiz "unmatched DEFER"

;-----------------------------------------------------------------------
end_of_compiled:
;-----------------------------------------------------------------------

.ifdef none
;-----------------------------------------------------------------------
;       controls alike 
;-----------------------------------------------------------------------

;def_word ">MARK", "TOMARK", 0
;        .word HERE, ZERO, COMMA
;        .word EXIT

;def_word "<MARK", "ATMARK", 0
;        .word HERE
;        .word EXIT

;def_word ">RESOLVE", "TORESOLVE", 0
;        .word HERE, SWAP, COMMA
;        .word EXIT

;def_word "<RESOLVE", "TORESOLVE", 0
;        .word COMMA
;        .word EXIT

;: >MARK ( -- addr) here 0 , ;
;: <MARK ( -- addr) here ;
;: >RESOLVE ( addr -- ) here swap (forth) ! ;
;: <RESOLVE ( -- addr) , ;

;: IF ( flag -- ) COMPILE (?BRANCH) >MARK ; immediate
;: ELSE ( -- ) COMPILE (BRANCH) >MARK swap >RESOLVE ; immediate
;: THEN ( -- ) >RESOLVE ; immediate
;: BEGIN ( -- ) <MARK ; immediate
;: UNTIL ( flag -- ) COMPILE (?BRANCH) <RESOLVE ; immediate
;: AGAIN ( flag -- ) COMPILE (BRANCH)  <RESOLVE ; immediate
;: WHILE ( flag -- ) COMPILE (?BRANCH) >MARK ; immediate
;: REPEAT ( -- ) COMPILE (BRANCH) swap <RESOLVE >RESOLVE ; immediate

.endif
