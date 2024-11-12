
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
   Fig-Forth and eForth codes.

   the code of eForth and Fig-Forth, turns out to be brilliant, 
   a feat of ingenuity and careful consideration of options, 
   given the restricted and specific set of opcodes of the 6502. 

   The Fig-Forth code, by W. B. Ragsdale and Robert Selzer, was also
   used as the basis for the R65F11, manufactured by Rockwell, 
   which contained Forth in ROMs. 

   With eForth concepts of minimal number of primitives.

## How
    
    Some goals:

    1.  Minimal boot Rom with Forth as bare bones boot;
    2.  Almost relocable compiled code;
    3.  Interrups threads in Forth;
    4.  Use of blocks and screens for keep simple;
    5.  Use forth file system, [ffs](https://github.com/howerj/ffs)


 ## Forth

    BIOS reserves $000-$03F, $0100-$013F, $0300-03FF, $0400-$04FF

    pleny of full 64k (sic) RAM, use interleaved dictionary;
    
    uses Minimal Thread Code;
    
    mixed eForth and Fig-Forth references;
     

 ## Uniques

    1. No DO LOOP, only FOR NEXT that counts down and exits at 0.
    2. no internal test for $XXFF, grep for it at end of compilation 
    3. tips from "EOR #$FF" https://ia804606.us.archive.org/26/items/eor6502/eorBookV1.0.1_ebookRelease.pdf

 ## Words

    unnest  next  look  pick  nest  pick  jump  hang 
    push  putw  select  
    key  emit  ?key  ?emit

    C@  C!  @  !  R@  R>  >R  SP@  RP@  SP!  RP! 
    DROP  DUP  SWAP  OVER  0=  0<  U<  +  U+  -  U-
    BRANCH  QBRANCH  EXIT  EXEC  FOR  NEXT  
    .  ?  COLD  WARM  BOOT  TURN  REST  BYE

    1+  1-  2+  2-  0  1  2  3  CELL  CELLS  CR  BL
    ROT  -ROT  DIP  2/  2*  NEGATE  INVERT  +  +!  

    create  does  find  comma  
    number  parse  word  accept 

    DP  LATEST  HERE  CMOVE  MOVE CSCAN CSKIP 
    CREATE  :  ;  ,  '  ;S
    IF  ELSE  THEN  BEGIN  AGAIN  UNTIL  WHILE  REPEAT
    CASE OF ENDOF ENDCASE
    QUIT EVAL
    
    Notes:

        next includes an interrupt handler to hang ;)

        EXEC does nest, EXIT does unnest

 ## Rationale

    the most used words in Forth executions are:

        inner  CALL  EXIT  LIT  
        @  !  0=  0<  +
        QBRANCH  BRANCH  DROP



