
;-----------------------------------------------------------------------
;   COMPILED WORDS
;-----------------------------------------------------------------------
init_of_compiled:

;-----------------------------------------------------------------------
;       core stuff
;-----------------------------------------------------------------------
; (( w1 w2 -- ((w1 < w2)) )) 
def_word "<", "LTH", 0
        .word MINUS, ZLT
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 -- ((w1 > w2)) )) 
def_word ">", "GTH", 0
        .word SWAP, LTH
	.word EXIT 

;-----------------------------------------------------------------------
; (( w1 --  w1 + CELL ))     
def_word "CELL+", "CELLPLUS", 0
        ; cells is two 
        .word TWOPLUS
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 -- w1 * 2 ))     
def_word "CELLS", "CELLS", 0
        ; cells is two 
        .word ASFL
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 w2 w1 ))     
def_word "TUCK", "TUCK", 0
        .word SWAP, OVER
	.word EXIT

;-----------------------------------------------------------------------
;       dword stuff
;-----------------------------------------------------------------------
; (( w1 w2 -- (w1) (w2) ))     
def_word "2@", "TWOAT", 0
        .word DUP, CELL, PLUS, FETCH, SWAP, FETCH
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 --  ))     
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
; (( w1 w2  -- w1 w2 w1 w2 ))     
def_word "2DUP", "TWODUP", 0
        .word OVER, OVER
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 -- w1 w2 w1 w2 ))  ????   
def_word "2OVER", "TWOOVER", 0
        .word TWOTOR, TWODUP, TWORTO, TWOSWAP
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 -- w3 w4 w1 w2 ))     
def_word "2SWAP", "TWOSWAP", 0
        .word ROT, TOR, ROT, RTO
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 w5 w6 -- ???? ))     
def_word "ROT2", "ROTF2", 0
        .word TOR, SWAP, RTO, SWAP
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 w2 w3 w4 w5 w6 -- ???? ))     
def_word "-ROT2", "ROTB2", 0
        .word SWAP, TOR, SWAP, RTO
	.word EXIT

;-----------------------------------------------------------------------
; dictionary stuff
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; (( -- ))      ; zzzz
def_word "VARIABLE", "VARIABLE", IMMEDIATE
        .word CREATE, DOVAR, DOVAR, COMMA, DOVAR, ZERO, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; (( w1 -- ))      ; zzzz
def_word "CONSTANT", "CONSTANT", IMMEDIATE
        .word CREATE, DOVAR, DOCON, COMMA, COMMA
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))    find a word in dictionary, return CFA or FALSE (0x0))  
; zzzz
def_word "FIND", "FINDF", 0
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
; (( -- ))   easy way 
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
; (( a1 -- n a2 ))  word from buffer, 
;       return size and address, or zero, update toin 
;
;       DOES NOT HANDLE END OF LINE
;       classic, copy from TOIN to TIB, 
;       tib is a internal 32 byte buffer
;       WORD putw in TIB or HERE ???
;       toin is a external >80 bytes buffer
;       when empty refill, or accept, or expect

def_word "WORD", "WORD", 0
        .word TOIN, FETCH 
        .word DUP, BL, CSKIP, PLUS   
        .word DUP, BL, CSCAN, OVER, OVER 
        .word PLUS, TOIN, STORE 
	.word EXIT

;----------------------------------------------------------------------
; (( -- a ))  ZZZZ
def_word "CREATE", "CREATE", 0
        .word BL, WORD, HERE, CMOVE 
        .word EXIT

;----------------------------------------------------------------------
; (( -- ))  ZZZZ
def_word "ACCEPT", "ACCEPT", 0
        .word DUP
        .word EXIT
;----------------------------------------------------------------------
; (( -- ))  compile a word
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
;       controls alike, all offsets 
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

;-----------------------------------------------------------------------
;       old school, offset branches
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
def_word "ELSE", "ELSE", IMMEDIATE
        .word DOCON, BRANCH, COMMA
        .word HERE, ZERO, COMMA
        .word SWAP, THEN
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))     
def_word "END", "END", IMMEDIATE
        .word DOCON, UNTIL, COMMA
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
; (( -- ))  
def_word "DOES", "DOES", 0
        ; zzzz
        .word EXIT

;-----------------------------------------------------------------------
; (( -- ))  
def_word "VALUE", "VALUE", 0
        .word CREATE, DOCON, DOCON, COMMA 
        .WORD DOES, DOVAR, ZERO, COMMA
        .word EXIT         

;-----------------------------------------------------------------------
; (( -- ))  
def_word "TO", "TO", 0
        .word TICK, TWOPLUS, COMMA 
        .word EXIT         

;-----------------------------------------------------------------------
; (( -- ))  
def_word "DEFER", "DEFER", 0
        .word CREATE, DOCON, NOOP, COMMA 
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
; (( w1 -- w2 w3 ))     
def_word "NOOP", "NOOP", 0
        .word FALSE
	.word EXIT

;-----------------------------------------------------------------------
; error messages
erro1:  .asciiz "unmatched DEFER"

;-----------------------------------------------------------------------
end_of_compiled:
;-----------------------------------------------------------------------

