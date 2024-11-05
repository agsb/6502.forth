
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

   Some eForth concepts of minimal number of primitives.

   The Fig-Forth code, by W. B. Ragsdale and Robert Selzer, was also
   used as the basis for the R65F11, manufactured by Rockwell, 
   which contained Forth in ROMs. 

## How
    
    Some goals:

    1.  Minimal boot rom with Forth bare bones;
    2.  Almost relocable compiled code;
    3.  Use of blocks and screens for keep simple;
    4.  

 ## Forth

    goals and marks.

    . reserves $00 to $24 of page zero for SWEET-16 use
    . pleny of full 64k (sic) RAM, use interleaved dictionary
    . uses Minimal Thread Code.

 ## Hardware
    
    . minimal 2 kb (256 bytes for 16 devices mapper) boot EPROM
    . full 64 kb RAM system
    . copy main FORTH from EEPROM to RAM
    . use I2C EEPROMs as Hard Disk

    1   MOS6502 (or W65C02)
    1   N6551       VIA
    2   M6522       PIA
    2   HM65256     32k RAM
    1   AT28C16     2k EEPROM
    8   AT24C256    32k I2C EEPROM
    1   Xtal        1.8 MHz
    1   74HC74
    N   74HC00
    
   The 6502's default frequency of about 1 MHz is merely illustrative, 
   compared to the 3.6 GHz of current CPUs.
   
   the USART is at 9600 bps, 8-N-1, duplex. fixed, no changes.

   the I2C EEPROM used as Hard Disk, 1k block, 64 bytes (screens).

 ## BIOS

### Primitives

```
    inner core: 

        unnest, next, pick, nest, link, jump,
        colon, semmis, comma, tick, word, parse, expect,
        docon, dovar, dodoes, pscode, execute,
        push, pull, put,
        drop, dup, swap, over, rot, -rot,
        and, or, xor, +, -, 2/, 2*, 2+, 2-, 1+, 1-,
        r>, >r, r@, sp@, rp@, sp!, rp!,
        for, next, setup, 
        0, 1, 2, 3, 4, BL, CR,
        DP, here, alloc, 
        interrupt,

   the 'bare bones' BIOS must have:

       millis,     return milliseconds
       getch,      get a byte from USART/COMM port
       putch,      put a byte into USART/COMM port
       qgetch,     verify if get a byte from USART/COMM port
       qputch,     verify if put a byte into USART/COMM port
       getio,      get a byte from a GPIO/DEVICE
       putio,      put a byte into a GPIO/DEVICE
    
    i2c core:
       
```
 ## References

   The history of Fig-Forth roots is at [Through The Forth
   Jungle](https://www.forth.org/svfig/kk/10-2021-Ragsdale.pdf)

    The [6502 forum](https://6502.org) 
   
