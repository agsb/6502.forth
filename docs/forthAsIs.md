# Forth as is

## group minimal forth words

    SP, RP, IP, WP, TOS, TOR, HP, DP    ; internal variables, never changes directly 
    unest, next, pick, nest, jump  ; heart beat of thread forth system

    
    !, @, !C, @C  ; memory store and fetch
    SP@, RP@, SP!, RP!  ; setup of stacks
    >R, R>, EXIT, EXEC ; return stack operations

    DUP, SWAP, ROT, DROP  ; stack operators
    AND, OR, XOR, NOT  ; logical operations 
    +, -, *, /  ; aritmetic operations

    BRANCH, QBRANCH  ; jump 
    IF, ELSE, THEN  ; decision 

    DO, LOOP, +LOOP, -LOOP  ; repetions
    FOR, NEXT, CONTINUE, BREAK  ; repetitions
    BEGIN, WHILE, REPEAT, AGAIN  ; repetitions

    KEY, EMIT, SOURCE, GETLINE, TOKEN ; I/O

    COMMA, TICK, FIND, NUMBER ; interactives
