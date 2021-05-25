;***********************************************************
;*
;*	Prather_Eric_Lab6_sourcecode.asm
;*
;*	Program which responds to capacitive button inputs and 
;*  counts the occurences via interrupts.
;***********************************************************
;*
;*	 Author: Enter your name
;*	   Date: Enter Date
;*
;***********************************************************

.include "m128def.inc"			; Include definition file
;.include "interrupts"

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 

;.equ	WskrR = 0				; Right Whisker Input Bit
;.equ	WskrL = 1				; Left Whisker Input Bit
;.equ INT0Addr = $0002			; These are predefined in 
								; m128def.inc so I can't
;.equ INT1Addr = $0004          ; redefine them here.


; The following is from lab 1
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

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt
		; Set up interrupt vectors for any interrupts being used
		; These are on page 59 of the datasheet.

		; This is just an example:
;.org	$002E					; Analog Comparator IV
;		rcall	HandleAC		; Call function to handle interrupt
;		reti					; Return from interrupt
.org INT0addr
		rjmp LeftBumperInterrupt
		reti
.org INT1addr
		rjmp RightBumperInterrupt
		reti

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr
		ldi		mpr, high(RAMEND)
		out		SPH, mpr
		
		;Port initalization copied from Lab 1 BasicBumpBot.asm
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

		; Initalize array
		rcall ClearBumperMemory
	


		; Initialize external interrupts
			; Set the Interrupt Sense Control to falling edge 
		; Instructions for how to do this provided by:
		; https://www.avrfreaks.net/forum/external-interrupts-using-assembly
		;ldi EICRA, (1<<ISC01) | (1<<ISC00)
		ldi mpr, 0b00001010 ;(1<<ISC01) | (1<<ISC10); (1<<0b00001010)
		sts EICRA, mpr ; Can't understand why we use sts when out is availible.

		
		ldi mpr, (1<<INTF0)
		out EIFR, mpr
		
		; Configure the External Interrupt Mask
		;ldi EIMSK, 1<<INT0
		ldi mpr, (1<<INT0) | (1<<INT1)
		out EIMSK, mpr					


		; Turn on interrupts
			; NOTE: This must be the last thing to do in the INIT function
		sei ; SHort for "set enable interrupt" (This is in the status register)
		; Inverse of sei is cli

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		ldi mpr, MovFwd
		out PORTB, mpr

		rjmp	MAIN			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the 
;	left whisker interrupt, one to handle the right whisker 
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;BumperMemoryState:
;	push ZL,
;	push ZR,
;	ldi ZL, low(BUMPER_MEMORY)
;	ldi ZH, high(BUMPER_MEMORY)
;
;	ret

; 0x20 = ASCII of ' '
; 0x?? = ASCII of 'L'
; 0x?? = ASCII of 'R'

ClearBumperMemory:
	push ZL
	push ZH
	push mpr
	ldi ZL, low(BUMPER_MEMORY)
	ldi ZH, high(BUMPER_MEMORY)
	ldi mpr, 0x20
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	st Z, mpr
	pop mpr
	pop ZH
	pop ZL
	ret

ShiftBumperMemoryLeft:
	push ZL
	push ZH
	push r16
	push r17
	ldi ZL, low(BUMPER_MEMORY)
	ldi ZH, high(BUMPER_MEMORY)
	inc ZL
	inc ZL
	inc ZL
	ldi r16, 0x20
	ld r17, Z
	st Z, r16
	dec ZL
	ld r16, Z
	st Z, r17
	dec ZL
	ld r17, Z
	st Z, r16
	dec ZL
	st Z, r17
	pop r17
	pop r16
	pop ZH
	pop ZL
	ret

;-----------------------------------------------------------
; Func: LeftBumperInterrupt
; Desc: Performs memory management and branching conditional
;       when the left bumper is hit
;-----------------------------------------------------------
LeftBumperInterrupt:
	cli
	;ldi r0, 0x00 ; // Conditional buffer
	ldi ZL, low(BUMPER_MEMORY)
	ldi ZH, high(BUMPER_MEMORY)
	ld mpr, Z 

	cpi mpr, 0x20 ; ' '
	breq LeftSimple
	inc ZL; Z -> BUMPER_MEMORY + 1
	cpi mpr, 0x4C ; 'L'
	breq LeftChain ;
	; No challenge code detected
	; Evaluate array item 2
LeftSimpleEval2:
	ld mpr, Z
	cpi mpr, 0x20
	breq LeftSimple
	inc ZL; Z -> BUMPER_MEMORY + 2
	cpi mpr, 0x4C 
	breq LeftChainNo180A
	; Evaluate array item 3
LeftSimpleEval3:
	ld mpr, Z 
	cpi mpr, 0x20
	breq LeftSimple
	inc ZL ; Z -> BUMPER_MEMORY + 3
	cpi mpr, 0x4C
	breq LeftChainNo180B
	

	; Evaluate array item 4 (different from 2,3)
LeftSimpleEval4:
	ld mpr, Z ; Z = BUMPER_MEMORY + 3
	cpi mpr, 0x20
	breq LeftSimple
	rcall ShiftBumperMemoryLeft ; Array is full, shift left
	cpi mpr, 0x4C
	breq LeftLong ; If L and last was L, big turn
	rjmp LeftSimple ; little turn



LeftChain:
	; Case 1: ['L', 'L', '?', '?'] (Illegal state!)
	ld r17, Z
	cpi r17, 0x4C
	breq LeftSimpleEval2
	; "Case 2: This is a regular 2x chain
	cpi r17, 0x20 ; [L, ' ', ' ', ' ']
	breq LeftLong
	; Case 3: Array is [L,R,L,R]
	cpi r17, 0x52 ; 0x4C + num = 0x52 = 'R'
	brne LeftSimpleEval2
	
	inc ZL; Z <- BUMPER_MEMORY + 2
	ld r17, Z 
	cpi r17, 0x4C 
	brne LeftSimpleEval3
	inc ZL; Z <- BUMPER_MEMORY + 3
	ld r17, Z 
	cpi r17, 0x52
	brne LeftSimpleEval4
	rjmp Left180

; we may have a special challenge code case:

; PRECONDITION: *BUMPER_MEMORY+1 == 'L'
LeftChainNo180A:
	; Case 1: Next is L, in which case we are double
	ld mpr, Z
	cpi mpr, 0x20; *BUMPER_MEMORY+2 == ' ' 
	breq LeftLong
	; Case 2: Next is R, in which case we are not a special case.
	rjmp LeftSimpleEval3

LeftChainNo180B:
	; Case 1: Next is L, in which case we are double
	ld mpr, Z
	cpi r16, 0x20 ; *BUMPER_MEMORY+3 == 'L'
	breq LeftLong
	; Case 2: Next is R, in which case we are not a special case.
	rjmp LeftSimpleEval4

;180:
Left180:
	; Clear memory array
	rcall ClearBumperMemory
	rcall Turn180 ; COMMENT OUT DURING DEBUG MODE, UNCOMMENT DURING BUILD
	rjmp LeftDone


LeftLong:
	; rewrite memory array => ['L',' ',' ',' ']
	ldi ZL, low(BUMPER_MEMORY)
	ldi ZH, high(BUMPER_MEMORY)
	ldi mpr, 0x4C ; 'L'
	st Z+, mpr
	ldi mpr, 0x20 ; ' '
	st Z+, mpr
	st Z+, mpr
	st Z, mpr
	rcall SuperTurnRight; COMMENT OUT DURING DEBUG MODE, UNCOMMENT DURING BUILD
	rjmp LeftDone

LeftSimple: ; Does "hitLeft" then stores 'L' to the array at the current index.
	ldi mpr, 0x4C
	st Z, mpr
	rcall HitLeft ; COMMENT OUT DURING DEBUG MODE, UNCOMMENT DURING BUILD
LeftDone:
	ldi mpr, 1;(1<<INTF0)
	out EIFR, mpr
	sei
	ret

;-----------------------------------------------------------
; Func: RightBumperInterrupt
; Desc: Performs memory management and branching conditional
;       when the left bumper is hit
;-----------------------------------------------------------
RightBumperInterrupt:
	cli
	;ldi r0, 0x00 ; // Conditional buffer
	ldi ZL, low(BUMPER_MEMORY)
	ldi ZH, high(BUMPER_MEMORY)
	ld mpr, Z 

	cpi mpr, 0x20 ; ' '
	breq RightSimple
	inc ZL; Z -> BUMPER_MEMORY + 1
	cpi mpr, 0x52 ; 'L'
	breq RightChain ;
	; No challenge code detected
	; Evaluate array item 2
RightSimpleEval2:
	ld mpr, Z 
	cpi mpr, 0x20
	breq RightSimple
	inc ZL ; Z -> BUMPER_MEMORY + 2
	cpi mpr, 0x52 
	breq RightChainNo180A
	; Evaluate array item 3
RightSimpleEval3:
	ld mpr, Z 
	cpi mpr, 0x20
	breq RightSimple
	inc ZL ; Z -> BUMPER_MEMORY + 3
	cpi mpr, 0x52
	breq RightChainNo180B
	

	; Evaluate array item 4 (different from 2,3)
RightSimpleEval4:
	ld mpr, Z ; Z = BUMPER_MEMORY + 3
	cpi mpr, 0x20
	breq RightSimple
	rcall ShiftBumperMemoryLeft ; Array is full, shift Right
	cpi mpr, 0x52
	breq RightLong ; If L and last was L, big turn
	rjmp RightSimple ; little turn



RightChain:
	; Case 1: ['L', 'L', '?', '?'] (Illegal state!)
	ld r17, Z
	cpi r17, 0x52
	breq RightSimpleEval2
	; "Case 2: This is a regular 2x chain
	cpi r17, 0x20 ; [L, ' ', ' ', ' ']
	breq RightLong
	; Case 3: Array is [L,R,L,R]
	cpi r17, 0x4C ; 0x52 + num = 0x4C = 'R'
	brne RightSimpleEval2
	
	inc ZL; Z <- BUMPER_MEMORY + 2
	ld r17, Z 
	cpi r17, 0x52 
	brne RightSimpleEval3
	inc ZL; Z <- BUMPER_MEMORY + 3
	ld r17, Z 
	cpi r17, 0x4C
	brne RightSimpleEval4
	rjmp Right180

; we may have a special challenge code case:
RightChainNo180A:
	; Case 1: Next is L, in which case we are double
	ld mpr, Z
	cpi mpr, 0x20; *BUMPER_MEMORY+2 == ' '
	breq RightLong
	; Case 2: Next is R, in which case we are not a special case.
	rjmp RightSimpleEval3

RightChainNo180B:
	; Case 1: Next is L, in which case we are double
	ld mpr, Z
	cpi r16, 0x20 ; *BUMPER_MEMORY+3 == ' '
	breq RightLong
	; Case 2: Next is R, in which case we are not a special case.
	rjmp RightSimpleEval4

;180:
Right180:
	; Clear memory array
	rcall ClearBumperMemory
	rcall Turn180 ; COMMENT OUT DURING DEBUG MODE, UNCOMMENT DURING BUILD
	rjmp RightDone


RightLong:
	; rewrite memory array => ['L',' ',' ',' ']
	ldi ZL, low(BUMPER_MEMORY)
	ldi ZH, high(BUMPER_MEMORY)
	ldi mpr, 0x52 ; 'L'
	st Z+, mpr
	ldi mpr, 0x20 ; ' '
	st Z+, mpr
	st Z+, mpr
	st Z, mpr
	rcall SuperTurnLeft; COMMENT OUT DURING DEBUG MODE, UNCOMMENT DURING BUILD
	rjmp RightDone

RightSimple: ; Does "hitRight" then stores 'L' to the array at the current index.
	ldi mpr, 0x52
	st Z, mpr
	rcall HitRight ; COMMENT OUT DURING DEBUG MODE, UNCOMMENT DURING BUILD
RightDone:
	ldi mpr, 1;(1<<INTF0)
	out EIFR, mpr
	sei
	ret


;-----------------------------------------------------------
; Func: SuperTurnLeft
; Desc: Turns left for twice as long as HitRight
;       Mostly copies lab 1 code
;-----------------------------------------------------------
SuperTurnLeft:
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

		; MY MODIFICATION HERE
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

;-----------------------------------------------------------
; Func: SuperTurnRight
; Desc: Turns right for twice as long as HitLeft
;       Mostly copies the code
;-----------------------------------------------------------
SuperTurnRight:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn right for a second TWO TIMES
		ldi		mpr, TurnR	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; MY MODIFICATION HERE:
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

;-----------------------------------------------------------
; Func: Turn180
; Desc: Turns 180 degrees. Achieves by turning in one direction.
;       This lasts for four seconds.
;-----------------------------------------------------------
Turn180:
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

		;MY MODIFICATION HERE
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function
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


;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------


;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label

		; Save variable by pushing them to the stack

		; Execute the function here
		
		; Restore variable by popping them from the stack in reverse order

		ret						; End a function with RET

;----------------------------------------------------------------
; LAB 1 ROUTINES
;----------------------------------------------------------------

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
;*	Stored Program Data
;***********************************************************
.dseg
.org $0100
BUMPER_MEMORY: ; Contiguous array of 'L', 'R', or ' '
	.byte 4

; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
