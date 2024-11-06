
## D. Ting eForth snapshoot

: COMPILE ( -- ) R> DUP @ , CELL+ >R ; COMPILE-ONLY

: [COMPILE] ( -- \ <string> ) ' , ; IMMEDIATE

: <MARK ( -- a ) HERE ;

: >MARK ( -- A ) HERE 0 , ;

: <RESOLVE ( a -- ) , ;

: >RESOLVE ( A -- ) <MARK SWAP ! ;

: BEGIN ( -- a ) \ Start an infinite or indefinite loop structure
    <MARK ; IMMEDIATE

: FOR ( -- a ) \ Start a FOR-NEXT loop structure
    COMPILE >R <MARK ; IMMEDIATE

: AHEAD ( -- A ) \ Compile a forward branch instruction
    COMPILE branch >MARK ; IMMEDIATE

: AFT ( a -- a A ) \ Jump to THEN in FOR-AFT-THEN-NEXT loop 1st time through
    DROP [COMPILE] AHEAD [COMPILE] BEGIN SWAP ; IMMEDIATE

: THEN ( A -- ) \ Terminate a conditional branch structure
    >RESOLVE ; IMMEDIATE

: NEXT ( a -- ) \ Terminate a FOR-NEXT loop structure
    COMPILE next <RESOLVE ; IMMEDIATE

: _TYPE ( b u -- ) \ emit u chars from b address
    FOR AFT DUP C@ >CHAR EMIT 1 + THEN NEXT DROP ;

: .S ( -- ) \ shows return stack
    CR DEPTH FOR AFT R@ PICK . THEN NEXT ." <sp" ;

