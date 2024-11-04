;
;   the 'bare bones' BIOS must have:
;       millis,     return milliseconds
;       getch,      get a byte from USART/COMM port
;       putch,      put a byte into USART/COMM port
;       qgetch,     verify if get a byte from USART/COMM port
;       qputch,     verify if put a byte into USART/COMM port
;       getio,      get a byte from a GPIO/DEVICE
;       putio,      put a byte into a GPIO/DEVICE
;       
;   for use with 6502.toy a SBC with 6502 family and TTL glue
;   a single Forth based computer, inspired in R65F11 but with
;   USART, I2C, SPI, etc.
;
;   In later designs Forth was only an application carried in/out
;   by the BIOS, then I tried to minimize the use of page zero and
;   page one. In this version, all concept and design is for use of
;   Forth.
;
