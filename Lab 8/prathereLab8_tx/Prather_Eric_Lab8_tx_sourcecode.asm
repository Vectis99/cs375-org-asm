;***********************************************************
;*
;*	Prather_Eric_Lab8_tx_sourcecode.asm
;*
;*	Remote Controller
;*
;***********************************************************
;*
;*	 Author: Eric Prather
;*	   Date: February 27, 2020
;*
;***********************************************************

.include "m128def.inc"			; Include definition file


;***********************************************************
;* Shared Macros
;* 
;* These macros are exactly the same in all files
;* Uncomment desired BAUD number, comment undesired.
;* 
;***********************************************************

.equ ADDRESS = 69 ; Nice.

; Baud rate should be 2400
; Baud # (single): 416
;.equ BAUD_LOW = 0b10100000
;.equ BAUD_HIGH = 0b00000001
; Baud # (double): 832
.equ BAUD_LOW = 0b01000000
.equ BAUD_HIGH = 0b00000011

; Flag descriptions in lab report
.equ TX_A = (1 << U2X1)
.equ RX_A = (1 << U2X1)
.equ TX_B = (1<<TXEN1) ; Interrupts: TXCIE1 and UDRE1
.equ RX_B = (1<<RXEN1) 
.equ TX_C =  (1<<USBS1) | (1<<UCSZ11) | (1<<UCSZ10) | (1<<UCPOL1) ;|(1<<UMSEL1)
.equ RX_C =  (1<<USBS1) | (1<<UCSZ11) | (1<<UCSZ10);| (1<<UCPOL1) ;|(1<<UMSEL1)

.equ UDRE_INTERRUPT_INVERSE_MASK = 0b10111111
.equ TXC_INTERRUPT_INVERSE_MASK = 0b11011111
.equ EXI_MASK = 0b11110011  ; Push buttons

.equ MOVE_FORWARD = 0b10110000
.equ MOVE_BACKWARD = 0b10000000
.equ TURN_RIGHT = 0b10100000
.equ TURN_LEFT = 0b10010000
.equ HALTSIG = 0b11001000
.equ FREEZE_CMD = 0b11111000

.equ FREEZE_BROADCAST = 0b01010101

;***********************************************************


;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multi-Purpose Register
.def	mpr2 = r17				; RESERVED MP-register

.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit
; Use these action codes between the remote and robot
; MSB = 1 thus:
; control signals are shifted right by one and ORed with 0b10000000 = $80
.equ	MovFwd =  ($80|1<<(EngDirR-1)|1<<(EngDirL-1))	;0b10110000 Move Forward Action Code
.equ	MovBck =  ($80|$00)								;0b10000000 Move Backward Action Code
.equ	TurnR =   ($80|1<<(EngDirL-1))					;0b10100000 Turn Right Action Code
.equ	TurnL =   ($80|1<<(EngDirR-1))					;0b10010000 Turn Left Action Code
.equ	Halt =    ($80|1<<(EngEnR-1)|1<<(EngEnL-1))		;0b11001000 Halt Action Code

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt


.org $0002;INT0addr
		rjmp TS_0

.org $0004;INT1addr
		rjmp TS_1

.org $000A;INT4addr
		rjmp TS_4

.org $000C;INT5addr
		rjmp TS_5

.org $000E;INT6addr
		rjmp TS_6

.org $0010;INT7addr
		rjmp TS_7


; USART Transmit Complete
.org $0040
	rcall ResetInterruptsOnTransmissionComplete
	reti

; USART Recv Complete
;.org $003C
;  nop

; USART Data register Empty
.org $003E
	rcall TransmitMPR2
	reti

; See page 171 of datasheet for interrupt execution order

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
	;Stack Pointer (VERY IMPORTANT!!!!) (Copied from Lab 1)
	ldi	mpr, low(RAMEND)
	out	SPL, mpr		; Load SPL with low byte of RAMEND
	ldi	mpr, high(RAMEND)
	out	SPH, mpr		; Load SPH with high byte of RAMEND
	;I/O Ports (Copied from Lab 1)
		    ; Initialize Port B for output
	ldi		mpr, $FF		; Set Port B Data Direction Register
	out		DDRB, mpr		; for output
	ldi		mpr, ADDRESS		; Initialize Port B Data Register
	out		PORTB, mpr		; so all Port B outputs are ADDRESS		
		; Initialize Port D for input
	ldi		mpr, $00		; Set Port D Data Direction Register
	out		DDRD, mpr		; for input
	ldi		mpr, $FF		; Initialize Port D Data Register
	out		PORTD, mpr		; so all Port D inputs are Tri-State


	;USART1
	;Set baudrate at 2400bps
	ldi mpr, BAUD_LOW
	sts UBRR1L, mpr
	ldi mpr, BAUD_HIGH
	sts UBRR1H, mpr

	;Enable transmitter
	;Set frame format: 8 data bits, 2 stop bits
	rcall BaudTransmit
	
	; Set interrupt flag
	sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		in mpr, PIND
		andi mpr, 1
		cpi mpr, 1
		breq POLL1
		rcall TS_0
POLL1:
		in mpr, PIND
		andi mpr, 2
		cpi mpr, 2
		breq POLL4
		rcall TS_1
POLL4:
		in mpr, PIND
		andi mpr, 16
		cpi mpr, 16
		breq POLL5
		rcall TS_4
POLL5:
		in mpr, PIND
		andi mpr, 32
		cpi mpr, 32
		breq POLL6
		rcall TS_5
POLL6:
		in mpr, PIND
		andi mpr, 64
		cpi mpr, 64
		breq POLL7
		rcall TS_6
POLL7:
		in mpr, PIND
		andi mpr, 128
		cpi mpr, 128
		breq MAIN
		rcall TS_7
		rjmp	MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

; TS = "Transmit Symbol" interrupt


TS_START:
	rjmp TS_END

TS_0:
	ldi mpr2, MOVE_FORWARD
	rjmp TS_END

TS_1:
	ldi mpr2, MOVE_BACKWARD
	rjmp TS_END

TS_4:
	ldi mpr2, TURN_RIGHT
	rjmp TS_END

TS_5:
	ldi mpr2, TURN_LEFT
	rjmp TS_END

TS_6:
	ldi mpr2, HALT
	rjmp TS_END

TS_7:
	ldi mpr2, FREEZE_CMD
	rjmp TS_END


TS_END:
	;Logic
	out PORTB, mpr2 ; Show message being transmitted
	ldi mpr, ADDRESS
	sts UDR1, mpr ; Transmit ADDRESS over USART1

	; Mask next Interrupts (Only listen for UDRE)
	ldi mpr, TX_B | (1<<UDRIE1) ; TXEN1 | UDRIE1
	sts UCSR1B, mpr
	;ldi mpr, 0 ; Ignore all external interrupt
	;out EIMSK, mpr
	; Cleanup
	; No cleanup to do
	
	ret



TransmitMPR2: ; Calls when UDRE interrupt triggers
	;enter stack frame
	push mpr
	in mpr, sreg
	push mpr
	;end enterstack frame

	sts UDR1, mpr2 ; Send the actual command
	; Mask next Interrupts (Only listen for TXC)
	ldi mpr, TX_B | (1<<TXCIE1)
	sts UCSR1B, mpr 
	;Cleanup
	ldi mpr, TX_A | (1<<TXC1)	; Clear transmission complete interrupt
	sts UCSR1A, mpr
	; exit stack frame
	pop mpr
	out sreg, mpr
	pop mpr
	ret
	; end exit stack frame

ResetInterruptsOnTransmissionComplete: ; calls on TXC1
	push mpr
	in mpr, sreg
	push mpr
	; Mask next Interrupts (Only Listen for external interrupts)
	ldi mpr, TX_B
	sts UCSR1B, mpr
	;Cleanup
	ldi mpr, TX_A | (1<<TXC1)	; Clear transmission complete interrupt
	sts UCSR1A, mpr
	
	ret
	
;***********************************************************
;* Shared functions
;* 
;* These functions are exactly the same in all files
;* 
;***********************************************************
BaudTransmit:
	; Begin stack frame
	push mpr
	in mpr, sreg
	push mpr
	; End begin stack frame

	; Configure Port D for output
	in mpr, DDRD
	ori mpr, 0b00001000
	out DDRD, mpr
	; Set baud control registers
	ldi mpr, TX_A
	sts UCSR1A, mpr
	ldi mpr, TX_B
	sts UCSR1B, mpr
	ldi mpr, TX_C
	sts UCSR1C, mpr
	; Cleanup

	; Exit stack frame
	pop mpr
	out sreg, mpr
	pop mpr
	ret
	; End exit stack frame

BaudRecv:
	; Begin stack frame
	push mpr
	in mpr, sreg
	push mpr
	; End begin stack frame

	; Configure Port D for input
	in mpr, DDRD
	andi mpr, 0b11111011
	out DDRD, mpr
	; Set baud control registers
	ldi mpr, RX_A
	sts UCSR1A, mpr
	ldi mpr, RX_B
	sts UCSR1B, mpr
	ldi mpr, RX_C
	sts UCSR1C, mpr
	; Cleanup
	; Exit stack frame
	pop mpr
	out sreg, mpr
	pop mpr
	ret
	; End exit stack frame

;***********************************************************


;***********************************************************
;*	Stored Program Data
;***********************************************************

;***********************************************************
;*	Additional Program Includes
;***********************************************************
