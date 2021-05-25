;***********************************************************
;*
;*	Prather_Eric_Lab8_rx_sourcecode.asm
;*
;*	Reciever robot
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
;************************************************************
;* Variable and Constant Declarations
;************************************************************
.def	mpr = r16				; Multi-Purpose Register
.def	last_msg = r20			; reserved
.def	waitcnt = r17				; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit


.equ	BotAddress = ADDRESS    ;(Enter your robot's address here (8 bits))

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////
.equ	MovFwd =  (1<<EngDirR|1<<EngDirL)	;0b01100000 Move Forward Action Code
.equ	MovBck =  $00						;0b00000000 Move Backward Action Code
.equ	TurnR =   (1<<EngDirL)				;0b01000000 Turn Right Action Code
.equ	TurnL =   (1<<EngDirR)				;0b00100000 Turn Left Action Code
.equ	Halt =    (1<<EngEnR|1<<EngEnL)		;0b10010000 Halt Action Code

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

;Should have Interrupt vectors for:
;- Left whisker
; Not using interrupt, just polling
;- Right whisker
; Not using interrupt, just polling
;- USART receive

; USART Transmit Complete
;.org $0040
;	rcall ResetInterruptsOnTransmissionComplete
;	reti

; USART Recv Complete
.org $003C
	rcall RECV_USART
	reti

; USART Data register Empty
;.org $003E
;	rcall TransmitMPR2
;	reti

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
	ldi		mpr, $00		; Initialize Port B Data Register
	out		PORTB, mpr		; so all Port B outputs are low		
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

	;Enable reciever
	;Set frame format: 8 data bits, 2 stop bits
	rcall BaudRecv
	
	; Set robot state
	ldi ZL, low(ROBOT_STATE)
	ldi ZH, high(ROBOT_STATE)
	ldi mpr, 0b00000000
	st Z, mpr

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
		rcall HitLeft
POLL1:
		in mpr, PIND
		andi mpr, 2
		cpi mpr, 2
		breq POLL5
		rcall HitRight
POLL5:
		in mpr, PIND
		andi mpr, 32
		cpi mpr, 32
		breq POLL6
		rcall FREEZE_SELF

POLL6:
		in mpr, PIND
		andi mpr, 64
		cpi mpr, 64
		breq POLL7
		rcall BROADCAST_FREEZE_DUMMY_TURN

POLL7:
		in mpr, PIND
		andi mpr, 128
		cpi mpr, 128
		breq MAIN
		rcall BROADCAST_FREEZE

		rjmp	MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

FREEZE_SELF:
	ldi last_msg, FREEZE_BROADCAST
	rjmp RECV_USART_SKIP_UDR

BROADCAST_FREEZE_DUMMY_TURN:
	; enter sf
	push mpr
	in mpr, sreg
	push mpr
	push ZL
	push ZH
	; end enter sf

	ldi last_msg, 0b10010000
	ld mpr, Z ; mpr = *Z
	ori mpr, 0b10000000 ; write bit 8 to 1
	st Z, mpr ; *Z = mpr
	
	rjmp BRANCH_IF_KNOWS_ADDRESS

INFINITE_LOOP:
	rjmp INFINITE_LOOP

RECV_USART:
	lds last_msg, UDR1
RECV_USART_SKIP_UDR:
	; enter sf
	push mpr
	in mpr, sreg
	push mpr
	push ZL
	push ZH
	; end enter sf

	ldi ZL, low(ROBOT_STATE)
	ldi ZH, high(ROBOT_STATE)

	; Freeze broadcast conditional
	cpi last_msg, FREEZE_BROADCAST
	brne BRANCH_IF_KNOWS_ADDRESS
	
	; Frozen
	in mpr, PINB
	push mpr
	ldi mpr, 0b10010000
	out PORTB, mpr
	ld mpr, Z
	andi mpr, 0b00000010
	cpi mpr, 0b00000010
	breq INFINITE_LOOP ; term on 3rd freeze
	ld mpr, Z
	inc mpr ; increase number of freezes
	st Z, mpr
	rcall Wait ; 1 sec
	rcall Wait ; 2 sec
	rcall Wait ; 3 sec
	rcall Wait ; 4 sec
	rcall Wait ; 5 sec
	pop mpr
	out PORTB, mpr
	rjmp CLEANUP


BRANCH_IF_KNOWS_ADDRESS: ; Guaranteed not to be frozen
	ld mpr, Z
	andi mpr, 0b10000000 ; check if address already recieved
	cpi mpr, 0b10000000
	breq EXEC_COMMAND
	; Now actually check our address
	cpi last_msg, ADDRESS
	brne CLEANUP ; was not meant for us
	; we heard our address
	ld mpr, Z
	ori mpr, 0b10000000 ; write bit 8 to 1
	st Z, mpr

	rjmp CLEANUP

EXEC_COMMAND: ; Do whatever the opcode tells
	
	; Flush address from memory
	ld mpr, Z
	andi mpr, 0b01111111 ; write bit 8 to 0
	st Z, mpr

	cpi last_msg, FREEZE_CMD
	brne NON_FREEZE_CMD
	rcall BROADCAST_FREEZE
	rjmp CLEANUP

NON_FREEZE_CMD:
	; last_msg is set
	mov mpr, last_msg ; mpr = last_msg
	lsl mpr
	out PORTB, mpr
	rjmp CLEANUP
	
CLEANUP:
	; exit sf
	pop ZH
	pop ZL
	pop mpr
	out sreg, mpr
	pop mpr
	ret
	; end exit sf

BROADCAST_FREEZE:
	; enter sf
	push mpr
	in mpr, sreg
	push mpr
	; end enter sf

	rcall BaudTransmit

	ldi mpr, FREEZE_BROADCAST
	sts UDR1, mpr ; Send the actual command

	; Mask next Interrupts (Only listen for TXC)
	ldi mpr, TX_B | (1<<TXCIE1)
	sts UCSR1B, mpr 
	
	;Cleanup
	ldi mpr, TX_A | (1<<TXC1)	; Clear transmission complete interrupt
	sts UCSR1A, mpr
	
	; exit sf
	pop mpr
	out sreg, mpr
	pop mpr
	ret
	; end exit sf

END_BROADCAST: ; TXC1 -> 1
	; enter sf
	push mpr
	in mpr, sreg
	push mpr
	; end enter sf
	ldi mpr, TX_A | (1<<TXC1)	; Clear transmission complete interrupt

	rcall BaudRecv
	
	; we don't care about udre, only txc

	; exit sf
	pop mpr
	out sreg, mpr
	pop mpr
	ret
	; end exit sf


; LAB 1 FUNCTIONS
;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
HitRight:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
Wait:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine


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
	ldi mpr, RX_B | (1<<RXCIE1)
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

.dseg
.org $0100
ROBOT_STATE:
	.byte 1

;***********************************************************
;*	Additional Program Includes
;***********************************************************

