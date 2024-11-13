	
(http://forum.6502.org/viewtopic.php?p=3331#p3331 @13/11/2024, edited)
( by dclxvi, optimized word operations )
	
ONEMINUS:
	LDA 0, X
	BNE ONEM1
	DEC 1, X
ONEM1:
	DEC 0, X
	RTS
ABS:
	LDA 1, X
	BNE ABS1
NEGATE:
	JSR ONEMINUS
INVERT: 
	JSR INV1
INV1:
	LDA #$FF
	EOR 0, X
	STA 0, X
	INX
ABS1:
	RTS

( self modify for opcode )
AND:	
	LDA #$35
	BNE MATH
OR:
	LDA #$15
	BNE MATH
XOR:
	LDA #$55
	BNE MATH
MINUS:
	JSR NEGATE
PLUS:
	CLC
	LDA #$75
MATH:
	STA MATH2
	JSR MATH1
MATH1:
	LDA 2, X
MATH2:
	ADC 0, X
	STA 2, X
	INX
	RTS
