# The Stacks

_this is still a stub_

_Charles Moore says 22 levels is enough for Forth._

## Back Stacks

The 6502 have two peculiar pages, the zero page and stack page, both unique and with 256 bytes. All sub-routines calls (JSR) and returns (RTS) uses the stack page for 16-bit pointers, also the indirect indexed and indexed indirect modes uses page zero. Those are valuable resources.

An good revew of 6502 addressing modes at [emulators](http://www.emulator101.com/6502-addressing-modes.html).

In 6502 code, to pass a byte between memory, always need use LDA and STA, there are exotic alternatives, but all uses the accumulator.

Almost 6502 typical stack implementations does as standart: 
      
      1. Allow till 128 words deep stack; 
      2. Any operation with values at stack must do pushs and pulls. 
      3. Any multitask or multiuser system must split or copy the stack.
      4. Stack of 128 words are round-robin, else could need check limits.
      
If using page zero or stack page, the 128 words must be split in 3 parts: One for generic code use ( < 84 words ); One for data stack ( > 22 words ); One for return stack ( > 22 words ). For multitask or multiuser, could be 2 tasks with 2 stacks of 24 words and a generic stack of 32.
      
These are most commom, using index, pointer, LSB, MSB in zero page: 

### hardware stack SP

Uses the hardware stack, must split. Each stack uses cycles ~66 cc, 40 bytes of code and 4 bytes. _Could not use JSR/RTS inside_.

      .macro push_sp index, value 
            LDA \index;
            TSX; 
            STX \index; 
            TAX; 
            TXS;      
            LDA \value + 0; 
            PHA; 
            LDA \value + 1; 
            PHA;          
            LDA \index; 
            TSX; 
            STX \index; 
            TAX; 
            TXS;      
      .endmacro ; 
      
      .macro pull_sp index, value
            LDA \index;
            TSX;
            STX \index;
            TAX;
            TXS;     
            PLA;
            STA \value + 1;
            PLA;
            STA \value + 0;           
            LDA \index;
            TSX;
            STX \index;
            TAX;
            TXS;     
      .endmacro ;  


### page zero indexed by X
      
Uses the page zero as stack, could be splited. Each stack uses cycles ~48 cc, 28 bytes of code and 4 bytes at zero page;

      .macro push_zx index, pointer, value 
            LDX \index; 
            DEX; 
            LDA \value + 1; 
            STA \pointer, X;
            DEX; 
            LDA \value + 0;
            STA \pointer, X;
            STX \index;
      .endmacro     
      
      .macro pull_zx index, pointer, value 
            LDX \index;
            LDA \pointer, X;
            STA \value + 1;
            INX;
            LDA \pointer, X;
            STA \value + 0;
            INX;
            STX \index;
      .endmacro


### page zero indirect indexed by Y

Uses the a pointer in page zero to anywhere in memory. Stacks with up to 128 cells. Each stack uses ~50 cc, 28 bytes of code and 4 bytes at zero page. _Multiuser and Multitask systems can change the pointers anytime._ 

      .macro push_iy index, pointer, value 
            LDY \index;
            DEY;
            LDA \value + 1;
            STA (\pointer), Y;
            DEY;
            LDA \value + 0;
            STA (\pointer), Y;
            STY \index;
      .endmacro      
      
      .macro pull_iy index, pointer, value 
            LDY \index;
            LDA (\pointer), Y;
            STA \value + 1;
            INY;
            LDA (\pointer), Y;
            STA \value + 0;
            INY;
            STY \index;
      .endmacro


### absolute address indexed by X or Y, not splited
      
Uses one absolute pointer _pointer_ to memory. Stacks with up to 128 cells. Each stack uses ~52 cc, 32 bytes of code and 2 bytes at zero page. _Any operation with values at stack could be at direct offset, no need use pulls and pushs_

      .macro push_ax index, value 
            LDX \index;
            LDA \value + 1;
            STA pointer - 1, X;
            LDA \value + 0;
            STA pointer - 2, X;
            DEX;
            DEX;
            STX \index;
      .endmacro    
      
      .macro pull_ax index, value 
            LDX \index;
            LDA pointer + 0, X;
            STA \value + 0;
            LDA pointer + 1, X;
            STA \value + 1;;
            INX;
            INX;
            STX \index; 
      .endmacro


### split absolute address indexed by X or Y
      
Uses two absolute pointers _pointer_lo_ and _pointer_hi_ to memory. Stacks with up to 256 cells, splited in two parts. Each stack uses ~48 cc, 30 bytes of code and 2 bytes at zero page.  _Any operations with values at stack could be at direct offset, no need pulls and pushs_

      .macro push_axs index, value 
            LDX \index;
            LDA \value + 1;
            STA pointer_lo - 1, X;
            LDA \value + 0;
            STA pointer_hi - 1, X;
            DEX;
            STX \index;
      .endmacro    
      
      .macro pull_axs index, value 
            LDX \index;  
            LDA pointer_lo + 0, X;
            STA \value + 0;
            LDA pointer_hi + 0, X;
            STA \value + 1;
            INX;
            STX \index;
      .endmacro


### direct address with indirect access by Y

Uses an absolute pointer _pointer_ to memory. _Stacks with up to any size_. Each stack uses ~96 cc, 58 bytes of code and 2 bytes at page zero. 

      .macro push_di pointer, value 
            LDY #0; 
            LDA \value + 1;
            STA (pointer), Y; 
            INC pointer + 0;
            BNE :+ ;
            INC pointer + 1; 
            : ;
            LDA \value + 0;
            STA (pointer), Y; 
            INC pointer + 0;
            BNE :+ ;
            INC pointer + 1; 
            : ;
       .endmacro    
      
      .macro pull_di pointer, value 
            LDY #0; 
            LDA pointer + 0;
            BNE :+ ;
            DEC pointer + 1;
            : ;
            DEC pointer + 0; 
            LDA (pointer), Y;
            STA \value + 1; 
            LDA pointer + 0;
            BNE :+ ;
            DEC pointer + 1;
            : ;
            DEC pointer + 0; 
            LDA (pointer), Y;
            STA \value + 0;
      .endmacro


### Comparasion

| type | code size | cycles | cells  | notes |
| -- | -- | -- | -- | -- | 
| hardware stack SP | 40 | 66 | 128 | must split*, must use push and pull | 
| page zero indexed by X | 28 | 48 | 128 | must split*, must use push and pull |
| indirect indexed by Y | 28 | 50 | 128 | must split*, must use push and pull |
| absolute address indexed | 32 | 52 | 128 | any operation at direct offset, no need pull and push |
| split absolute addres indexed | 30 | 48 | 256 | any operation at direct offset, no need pull and push |
| direct address with indirect access | 58 | 96 | any size | must use push and pull | 

\* a least 22 cells of each stack and rest for inline code
  
### What Do 

Consider for no multitask, no multiuser, just 128 deep stacks, reduce overhead, direct memoy access and good timing, then

_Using absolute address indexed access for stacks_ 

It provides the most fast overall implementation because does not need use push and pull. 

All operations DROP, DUP, OVER, SWAP, ROT, AND, OR, XOR, NEG, INV, ADD, SUB, INC, DEC, EQ, LT, GT, SHL, SHR, AT (Fetch), TO (Store), are done using offsets (table 2).
  
  | _table 2_ | memory layout|
  | --- | --- | 
  | low | address |
  | -4  | LSB *COS*|
  | -3  | MSB |
  | -2  | LSB *BOS*|
  | -1  | MSB |
  |  0  | LSB **TOS** |
  | +1  | MSB |
  | +2  | LSB *NOS* |
  | +3  | MSB |
  | +4  | LSB *MOS* |
  | +5  | MSB |
  | +6  | LSB *POS* |
  | +7  | MSB |
  | high | address |

      - Odd address are always MSB, even address are always LSB
      - TOS, top in stack; NOS, next in stack; 
      - MOS, POS, next in sequence, BOS, COS back in sequence, for easy macros 

### Best

**For common "alone" applications _zero page indexed by X_ with 24 words per stack and 32 words shared could be faster.** 

Note that all modes needs read and write stack indexes from/into somewhere, then _lda, sta, inc, dec_ are always used.

     
     
