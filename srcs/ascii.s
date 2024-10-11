
; @https://cx16.dk/6502/algorithms.html

; Constants describing the role of each classification bit
_CTL	EQU $80
_PRN	EQU $40
_WSP	EQU $20
_PCT	EQU $10
_UPR	EQU $08
_LWR	EQU $04
_DGT	EQU $02
_HEX	EQU $01

; The lookup table of character descriptions
ASCIIS:
	DB  _CTL		; NUL
	DB  _CTL		; SOH
	DB  _CTL		; STX
	DB  _CTL		; ETX
	DB  _CTL		; EOT
	DB  _CTL		; ENQ
	DB  _CTL		; ACK
	DB  _CTL		; BEL
	DB  _CTL		; BS
	DB  _CTL|_WSP		; TAB
	DB  _CTL|_WSP		; LF
	DB  _CTL|_WSP		; VT
	DB  _CTL|_WSP		; FF
	DB  _CTL|_WSP		; CR
	DB  _CTL		; SO
	DB  _CTL		; SI
	DB  _CTL		; DLE
	DB  _CTL		; DC1
	DB  _CTL		; DC2
	DB  _CTL		; DC3
	DB  _CTL		; DC4
	DB  _CTL		; NAK
	DB  _CTL		; SYN
	DB  _CTL		; ETB
	DB  _CTL		; CAN
	DB  _CTL		; EM
	DB  _CTL		; SUB
	DB  _CTL		; ESC
	DB  _CTL		; FS
	DB  _CTL		; GS
	DB  _CTL		; RS
	DB  _CTL		; US
	DB  _PRN|_WSP		; SPACE
	DB  _PRN|_PCT		; !
	DB  _PRN|_PCT		; "
	DB  _PRN|_PCT		; #
	DB  _PRN|_PCT		; $
	DB  _PRN|_PCT		; %
	DB  _PRN|_PCT		; &
	DB  _PRN|_PCT		; '
	DB  _PRN|_PCT		; (
	DB  _PRN|_PCT		; )
	DB  _PRN|_PCT		; *
	DB  _PRN|_PCT		; +
	DB  _PRN|_PCT		; ,
	DB  _PRN|_PCT		; -
	DB  _PRN|_PCT		; .
	DB  _PRN|_PCT		; /
	DB  _PRN|_DGT|_HEX	; 0
	DB  _PRN|_DGT|_HEX	; 1
	DB  _PRN|_DGT|_HEX	; 2
	DB  _PRN|_DGT|_HEX	; 3
	DB  _PRN|_DGT|_HEX	; 4
	DB  _PRN|_DGT|_HEX	; 5
	DB  _PRN|_DGT|_HEX	; 6
	DB  _PRN|_DGT|_HEX	; 7
	DB  _PRN|_DGT|_HEX	; 8
	DB  _PRN|_DGT|_HEX	; 9
	DB  _PRN|_PCT		; :
	DB  _PRN|_PCT		; ;
	DB  _PRN|_PCT		; <
	DB  _PRN|_PCT		; =
	DB  _PRN|_PCT		; >
	DB  _PRN|_PCT		; ?
	DB  _PRN|_PCT		; @
	DB  _PRN|_UPR|_HEX	; A
	DB  _PRN|_UPR|_HEX	; B
	DB  _PRN|_UPR|_HEX	; C
	DB  _PRN|_UPR|_HEX	; D
	DB  _PRN|_UPR|_HEX	; E
	DB  _PRN|_UPR|_HEX	; F
	DB  _PRN|_UPR		; G
	DB  _PRN|_UPR		; H
	DB  _PRN|_UPR		; I
	DB  _PRN|_UPR		; J
	DB  _PRN|_UPR		; K
	DB  _PRN|_UPR		; L
	DB  _PRN|_UPR		; M
	DB  _PRN|_UPR		; N
	DB  _PRN|_UPR		; O
	DB  _PRN|_UPR		; P
	DB  _PRN|_UPR		; Q
	DB  _PRN|_UPR		; R
	DB  _PRN|_UPR		; S
	DB  _PRN|_UPR		; T
	DB  _PRN|_UPR		; U
	DB  _PRN|_UPR		; V
	DB  _PRN|_UPR		; W
	DB  _PRN|_UPR		; X
	DB  _PRN|_UPR		; Y
	DB  _PRN|_UPR		; Z
	DB  _PRN|_PCT		; [
	DB  _PRN|_PCT		; \
	DB  _PRN|_PCT		; ]
	DB  _PRN|_PCT		; ^
	DB  _PRN|_PCT		; _
	DB  _PRN|_PCT		; `
	DB  _PRN|_LWR|_HEX	; a
	DB  _PRN|_LWR|_HEX	; b
	DB  _PRN|_LWR|_HEX	; c
	DB  _PRN|_LWR|_HEX	; d
	DB  _PRN|_LWR|_HEX	; e
	DB  _PRN|_LWR|_HEX	; f
	DB  _PRN|_LWR		; g
	DB  _PRN|_LWR		; h
	DB  _PRN|_LWR		; i
	DB  _PRN|_LWR		; j
	DB  _PRN|_LWR		; k
	DB  _PRN|_LWR		; l
	DB  _PRN|_LWR		; m
	DB  _PRN|_LWR		; n
	DB  _PRN|_LWR		; o
	DB  _PRN|_LWR		; p
	DB  _PRN|_LWR		; q
	DB  _PRN|_LWR		; r
	DB  _PRN|_LWR		; s
	DB  _PRN|_LWR		; t
	DB  _PRN|_LWR		; u
	DB  _PRN|_LWR		; v
	DB  _PRN|_LWR		; w
	DB  _PRN|_LWR		; x
	DB  _PRN|_LWR		; y
	DB  _PRN|_LWR		; z
	DB  _PRN|_PCT		; {
	DB  _PRN|_PCT		; |
	DB  _PRN|_PCT		; }
	DB  _PRN|_PCT		; ~
	DB  _CTL			; DEL

