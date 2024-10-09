
;--------------------------------------------------------
;
;   specifc for ca65 assembler
;
;--------------------------------------------------------

; identifiers

.case +

; enable features

.feature c_comments

.feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment

; enable 6502 mode

.p02

;--------------------------------------------------------

.segment "ZERO"

* = $0000

ptz:    .byte $0
ivi:    .byte $0

lsb:    .byte $0
msb:    .byte $0

ind:    .byte $0
lmt:    .byte $0

; for stack in page zero
.res    $20, $0
pts:

;--------------------------------------------------------

.segment "CODE"

ptr:    .word $0

ptr_lo: .word $0

ptr_hi: .word $0

;--------------------------------------------------------
.segment "VECTORS"

.word init
.word init
.word init

;--------------------------------------------------------

.segment "ONCE"

; ### at hardware stack

push_SP:
  LDA ind
  TSX
  STX ind
  TAX
  TXS
  LDA lsb
  PHA
  LDA msb
  PHA
  LDA ind
  TSX
  STX ind
  TAX
  TXS
  RTS

pull_SP:
  LDA ind
  TSX
  STX ind
  TAX
  TXS
  PLA
  STA msb
  PLA
  STA lsb
  LDA ind
  TSX
  STX ind
  TAX
  TXS
  RTS

;## at page zero indexed by X

push_ZX:
  LDX ind
  LDA msb
  STA pts - 2, X
  LDA lsb
  STA pts - 1, X
  DEX
  DEX
  STX ind
  RTS

pull_ZX:
  LDX ind
  LDA ptz + 0, X
  STA msb
  LDA ptz + 1, X
  STA lsb
  INX
  INX
  STX ind
  RTS

;## indirect by page zero indexed by Y

push_IY:
  LDY ind
  DEY
  LDA msb
  STA (ptz), Y
  DEY
  LDA lsb
  STA (ptz), Y
  STY ind
  RTS

pull_IY:
  LDY ind
  LDA (ptz), Y
  STA msb
  INY
  LDA (ptz), Y
  STA lsb
  INY
  STY ind
  RTS

;## absolute address indexed by X or Y

push_AX:
  LDX ind
  LDA msb
  STA ptr - 1, X
  LDA lsb
  STA ptr - 2, X
  DEX
  DEX
  STX ind
  RTS

pull_AX:
  LDX ind
  LDA ptr + 0, X
  STA lsb
  LDA ptr + 1, X
  STA msb
  INX
  INX
  STX ind
  RTS

;## split absolute addres indexed by X or Y

push_AXS: 
  LDX ind
  LDA msb
  STA ptr_lo - 1, X
  LDA lsb
  STA ptr_hi - 1, X
  DEX
  STX ind
  RTS

pull_AXS: 
  LDX ind
  LDA ptr_lo + 0, X
  STA lsb
  LDA ptr_hi + 0, X
  STA msb
  INX
  STX ind
  RTS

;## direct address with indirect access

push_DI:
  LDY #0
  LDA msb
  STA (ptz), Y
  INC ptz + 0
  BNE :+ 
  INC ptz + 1
: 
  LDA lsb
  STA (ptz), Y
  INC ptz + 0
  BNE :+ 
  INC ptz + 1
  RTS

pull_DI:
  LDY #0
  LDA ptz + 0
  BNE :+ 
  DEC ptz + 1
: 
  DEC ptz + 0
  LDA (ptz), Y
  STA msb
  LDA ptz + 0
  BNE :+ 
  DEC ptz + 1
: 
  DEC ptz + 0
  LDA (ptz), Y
  STA lsb
  RTS

init:
    jmp init
