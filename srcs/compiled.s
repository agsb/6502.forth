
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
def_word "CELL-", "CELLMINUS", 0
        ; cells is two 
        .word TWOMINUS
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
        .word SWAP, OVER, TO, CELL, PLUS, TO
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
;
;-----------------------------------------------------------------------
; ( xt -- exception# | 0 \ return addr on stack,  as Forth-2012
def_word "CATCH", "CATCH", 0
        .word SPAT, TOR, HANDLER, FETCH, TOR
	.word RPAT, HANDLER, TO
	.word EXECUTE
	.word RTO, HANDLER, TO
	.word RTO, DROP, ZERO
	.word EXIT
 
;-----------------------------------------------------------------------
; ( ??? exception# -- ??? exception# ),  as Forth-2012
def_word "THROW", "THROW", 0
	.word QDUP, QBRANCH, 13
	.word HANDLER, FETCH, RPTO
	.word RTO, HANDLER, TO
	.word RTO, SWAP, TOR
	.word SPTO, DROP, RTO
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
        .word HERE, CTO, ONE, ALLOT
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )   
def_word ",", "COMMA", 0
        .word HERE, TO, TWO, ALLOT
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )   
def_word "SOURCE", "SOURCE", 0
        .word BLK, AT, QDUP
        .word QBRANCH, 10
        .word BLOCK, BVBUF
        .word BRANCH, 08
        .word TIB, TIBZ, AT
        .word EXIT

;-----------------------------------------------------------------------
; ( -- )   
def_word "'", "TICK", 0
        .word FINDF
	.word EXIT

;-----------------------------------------------------------------------
; ( -- )   easy way 
def_word "COMPILE!", "COMPILE!", IMMEDIATE
        .word TICK, COMMA
	.word EXIT

.if 0

; https://stackoverflow.com/questions/31636929/\
; how-to-properly-implement-postpone-in-a-forth-system
; https://github.com/ForthHub/discussion/discussions/105

: state-on  ( -- )  1 state ! ;
: state-off ( -- )  0 state ! ;

: execute-compiling ( i*x xt --j*x )
  state @ if  execute  exit  then
  state-on  execute  state-off
  ;
 
: postpone ( "name" -- )
  bl word find dup 0= -13 and throw 1 = ( xt flag-special )
  swap lit, if ['] execute-compiling else ['] compile, then compile,
  ; immediate

.endif 

;----------------------------------------------------------------------
; ( -- )  state mode compile
def_word "[", "LBRAC", 0
        .word ZERO, STATE, TO
	.word EXIT

;----------------------------------------------------------------------
; ( -- )  state mode interprete
def_word "]", "RBRAC", 0
        .word ONE, STATE, TO
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))    find a word in dictionary, return CFA or FALSE (0x0))  
def_word "FINDF", "FINDF", 0
	.word EXIT

;-----------------------------------------------------------------------
; (( -- ))    find a word in dictionary, return CFA or FALSE (0x0))  
def_word "PFIND", "PFIND", 0
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
        .word HERE, LAST, TO
        .word LATEST, COMMA 
        .word RAT, HERE, CTO
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
; ( -- )  compile a word, zzzz
def_word ":", "COLON", 0
	.word HERE, LAST, TO
        .word CURRENT, FETCH, CONTEXT, TO
        .word CREATE, LATEST, TRUE, OVER, 
        .word RBRAC
        .word EXIT

;----------------------------------------------------------------------
; ( -- )  ends a compile word, zzzz
def_word ";", "SEMIS", 0
	.word LIT, EXIT, COMMA
        .word LAST, FETCH, LATEST, TO, 
        .word LBRAC
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
        .word HERE, OVER, MINUS, SWAP, TO
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
        .word HERE, SWAP, TO
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
; (( w1 -- w1 ))     
def_word "NOOP", "NOOP", 0
	.word EXIT

;----------------------------------------------------------------------

