
# Another Forth

## Why

   This Forth is for use with my 6502.toy project, a SBC with 6502 
   family and I2C EEPROMS and TTL glue. A single full Forth computer, 
   inspired in R65F11 but with new devices as I2C, SPI, etc.

   In my previous projects, Forth was just an application, under an 
   Operational Scheduler and Basic Input Output systems. So I tried to 
   minimize the use of page zero and page one as privileged resources. 

   In this version, the entire concept and design is for the use of 
   Forth as the host and the main agent of processes and event, 
   thus, setting full use of the data stack on page zero and the 
   return stack on the dedicated CPU stack, as done in the original 
   Fig-Forth code.

   Fig-Forth code turns out to be brilliant, a feat of ingenuity and
   careful consideration of options, given the restricted and specific 
   set of opcodes of the 6502. 

   With eForth concepts of minimal number of primitives.

   The Fig-Forth code, by W. B. Ragsdale and Robert Selzer, was also
   used as the basis for the R65F11, manufactured by Rockwell, 
   which contained Forth in ROMs. 

## How
    
    Some goals:

    1.  Minimal boot Rom with Forth as bare bones boot;
    2.  Almost relocable compiled code;
    3.  Interrups threads in Forth;
    4.  Use of blocks and screens for keep simple;

 ## Forth

    1. reserves $00 to $3F of page zero for MONITOR, SWEET-16, etc
    2. pleny of full 64k (sic) RAM, use interleaved dictionary
    3. uses Minimal Thread Code.
    4. mixed eForth and Fig-Forth references.
    5. 

 ## Uniques

    1. No DO LOOP, only FOR NEXT that exits at 0
    2. 

 ## Words

    unnest  next  pick  nest  pick  jump 
    find  comma  number  parse  word  accept 
    push  putw  select  
    key  emit  ?key  ?emit

    C@  C!  @  !  R@  R>  >R  SP@  RP@  SP!  RP! 
    DROP  DUP  SWAP  OVER 0= 0< U<  +  U+
    :  ;  ,  ' ;S  EXIT  EXECUTE  FOR  NEXT . ?

    1+  1-  2+  2-  0  1  2  3  CELL  CELLS  CR  BL
    ROT  -ROT  DIP  2/  2*  NEGATE  INVERT  +  +!  

    BRANCH  QBRANCH  DP  LATEST  HERE 
    IF  ELSE  THEN  BEGIN  AGAIN  UNTIL  WHILE  REPEAT
    CASE OF ENDOF ENDCASE
    
 ## Rationale

    the most used words in Forth executions are:

        inner  CALL  EXIT  LIT  
        @  !  0=  0<  +
        QBRANCH  BRANCH  DROP  PUTW



