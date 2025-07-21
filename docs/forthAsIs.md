# Forth as is

## group minimal forth words

unest, next, pick, nest, jump  ; heart beat of thread forth system
!, @, !C, @C  ; memory store and fetch
SP@, RP@, SP!, RP!  ; setup of stacks
>R, R>, EXIT, EXEC ; return stack operations

DUP, SWAP, ROT, DROP  ; stack operators
AND, OR, XOR, NOT  ; logical operations 
+, -, *, /  ; aritmetic operations

IF, ELSE, THEN  ; decision 

DO, LOOP, +LOOP, -LOOP  ; repetions
FOR, NEXT, CONTINUE, BREAK  ; repetitions
BEGIN, WHILE, REPEAT, AGAIN  ; repetitions

KEY, EMIT, SOURCE, GETLINE, TOKEN ; I/O

COMMA, TICK, FIND, NUMBER ; interactives
