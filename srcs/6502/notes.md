## Notes 

07/03/2025

    Reworking for review of functions and sequences 

    review of: < D< U<

    includes words: R0 S0 

12/12/2024

    This forth is for a toy, using a 8-bit CPU, 
    there is no need of offsets in BRANCH, then
    review for use absolute address.
    
    Pre-compiled words uses ( * + cells ).

    All Forth code goes from $1000

26/11/2024

    Review of functions in lowercase and words in uppercase
    Going for a internal REPL 
    split forths.s into 

        minimal.s (with minimal core), 
        native.s (core words in native code),
        compiled.s (core words in compiled code)

21/11/2024

    Using ideas from 6502.org forum "Fleet Forth design considerations"

18/11/2025

    Review of code for use 65C02 extra opcodes

15/11/2024
    
    Reviews of standarts F79, F83, F94 for words about 
    
    terminal input buffer and outer interpreter.
    
    REFILL, ACCCEPT, EXPECT, QUERY
    PARSE-WORD, PARSE, WORD
    SCR, BLK, TONI 
    BLOCK, BUFFER

    obsolets: QUERY, WORD, TIB
    
14/11/2024
    
    no user offset in forth variables
    user variable points to start of Forth variables, see source.
    review of most compiled words
    grouping compiled words by functional scope

09/11/2024 
    
    review bios at 6502.toy, 
    defined $040, $140, $400, $1000 limits 
    S0 at $0FF, R0 at $1FF, U0 at $1000+4
    TIB at $0200, with 82 bytes,
 
06/11/2024 
    
    house clean 
    rewrite for not splited MSB LSB stacks. 
    wise keep Y=0 in next, 
    keep X as dedicated data stack index. 
    keep return stack at hard stack 
 
05/11/2024 
    
    rewrite for use stacks at pages zero and one 
    due overhead at @ and ! 
 
03/11/2024 
    
    using absolute splited stacks. 
    interrupt service from R65F11, 
    also http://wilsonminesco.com/6502primer/IRQconx.html 
 
## Todo
    
    still NO multi-user and NO multi-task
    still NO vocabularies, but will be a linked-list not a array.
    need DOES>, CREATE, ACCEPT, WORD,   
    

## Read me

;-----------------------------------------------------------------------
;
;	A((nother)) Forth for 65C02 with
;	data stack in page zero, return stack in page one
;	and using minimal thread code, 
;	
;   Inspired on eForth and Fig-Forth, alike R65F11.
;	
;	FALSE is $0000 (0)) TRUE is $FFFF ((-1))
;
;	SP0 and RP0 uses $FF as botton
;	
;	stacks grows backwards, as -2 -1 0 1 2 3 ...
;	0 is TOS, 1 is NOS, 2 is SOS, 3 is FORGET.
;	
;       Why another Forth? To learn how to.
;
;       for use in the 65C02.toy SBC
;
;   order in stack:
;
;       as words order, lsb 0 1 msb
;
;       as double, order TOS lsb 2 3 0 1 msb // NOS lsb 6 7 4 5 msb
;
;       as multibyte, count 0 1 lsb 2 3 4 5 6 7 8 9 msb etc
;
;-----------------------------------------------------------------------
;
; ABOUT Coding
;
;       It's for a SBC toy, no strict ANSI Forth.
;	    
;       BIOS loads into SRAM and jumps to $1000.
;
;       Non multitask, non multiuser and non-vocabulary Forth.
;
;       Non-rellocable, non self-modified and non-optimized code.
;
;	    uses X as data stack index, SP as return stack index,
;           A is generic and Y as keeped zero at next.
;	
;       internal functions are lowercase, words are uppercase.
;
;       Code must flow.
;
;-----------------------------------------------------------------------
;
; ABOUT Minimal Thread Code
;
;	1. Mixed header and code dictionary, no need CFA
;
;       the next cell after c-name is always a CFA
;	
;       header:
;               link:   .word
;               byte:   .lenght+flags
;               name:   .byte * lenght
;       code:   
;               all bytes following name
;               are references or native code
;
;	2. The first reference of a word could be any.
;
;       No need of DOCOL at first.
;
;       all references above a limit address always are compond words
;       and are pushed into return stack (nest) and must ends with 
;       pulled from return stack
;
;       3. Only primitives are executed.
;
;       When a primitive is executed, wk is a self reference and
;       ip is the following reference on current word.
;
;-----------------------------------------------------------------------
;
; ABOUT Stack Notation
;
; __The top of the stack is to the right.__, 
;       Forth 2012, Standart.
;
; Sorry. Warning.
;   
;   Any double comments (( -- )) shows stack order from left to right.
;
; Why ? Makes easy counts of cells and offsets.
;
;-----------------------------------------------------------------------

