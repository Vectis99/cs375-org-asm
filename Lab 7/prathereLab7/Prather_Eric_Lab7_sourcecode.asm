;***********************************************************
;*
;*	Prather_Eric_Lab7_sourcecode
;*
;*  Drives the tekbot forward, but allows the speed to be
;*  configured via interrupts from PORTD. Enabling 
;*  technology is PWM mode of TCNT0.
;*
;***********************************************************
;*
;*	 Author: Eric Prather (prathere@oregonstate.edu)
;*	   Date: Feb 26, 2020
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	mpr2 = r17

.equ	EngEnR = 4				; right Engine Enable Bit
.equ	EngEnL = 7				; left Engine Enable Bit
.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit

; Macros for tekbot movement (From lab 1)
.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

		; place instructions in interrupt vectors here, if needed
.org INT0addr
		rjmp IncreaseSpeed
		;reti
.org INT1addr
		rjmp DecreaseSpeed
		;reti
.org INT2addr
		rjmp MaxSpeed
		;reti
.org INT3addr
		rjmp MinSpeed
		;reti
.org	$001E ; Timer 0 Compare Match (FALLING EDGE)
		rjmp ClockCompareInterrupt
		;reti
.org	$0020 ; Timer 0 Overflow (RISING EDGE)
		rjmp ClockOverflowInterrupt
		;reti


.org	$0046					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		;;; BEGIN LAB 1 CODE SEGMENT ;;;
		; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND

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

		; Initialize TekBot Forward Movement
		ldi		mpr, MovFwd		; Load Move Forward Command
		out		PORTB, mpr		; Send command to motors
		;;; END LAB 1 CODE SEGMENT ;;;


		; Configure External Interrupts, if needed
		; See lab 6 for more detail
		ldi mpr, 0b10101010 ; Falling edge for all 4 buttons
							; See datasheet page 89-90
		sts EICRA, mpr ; Can't understand why we use sts?
		; Fortunately, we do not have to use EICRB
		ldi mpr, 0b00001111 ; Initalize interrupts to clear flags?
							; I don't know if this is actually necessary,
							; but whatever, I'll bite.
		out EIFR, mpr
		out EIMSK, mpr      ; We want to enable the same interrupts as those
							; whose flags we just cleared.


		; Configure 8-bit Timer/Counters
		; See page 104 of datasheet for register descriptions
		; See pages 98-99 of datasheet for FastPWM description
		; Set TCCR0 to No prescaling, non-inverting fast PWM
		ldi mpr, 0b01001011 ; Prescaler bits (CS00,CS01,CS02 => clk_T0S:001), fast PWM
				            ; (WGM01:0=3) must be set. COM01:0 => Non inverting. Clear FOC.
		out TCCR0, mpr      ; Write to control register
		

		; Output compare register should be based on duty cycle.
		ldi mpr, 0x7F ; About speed 7
		out OCR0, mpr
		; Enable timer interrupts on falling and rising edge of PWM mode
		ldi mpr, 0b00000011 ; (1<<OCIE0)| (1<<OCIE1)  ; Mask
		out TIMSK, mpr
		ldi mpr, 0b00000011 ;(1<<TOV0) | (1<<OFC0) ; Flags
		out TIFR, mpr


		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL)
		; Set initial speed, display on Port B pins 3:0
		ldi ZL, low(SPEED)
		ldi ZH, high(SPEED)
		ldi mpr, 0b00000111 | MovFwd ; Initial speed = 7, for kicks, in the forward direction
		st Z, mpr
		out PORTB, mpr

		; Enable global interrupts (if any are used)
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		; poll Port D pushbuttons ~~(if needed)~~

								; if pressed, adjust speed
								; also, adjust speed indication

		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************
;-----------------------------------------------------------
; Func:	ResetOCR
; Desc:	Calculates a number [0..255] from the speed [0-15] 
;		to put in timer/counter 0’s output compare register,
;	    then puts it there. Must be called every time the 
;		speed is changed.
;-----------------------------------------------------------
ResetOCR:
	push mpr2
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ld mpr, Z ; mpr = SPEED
	andi mpr, 0b00001111 ; 0-15 speed number
	ldi mpr2, 0b00010001 ; 0d16

	; CONDITION 1: MAX SPEED
	cpi mpr, 0b00001111
	brne CON_2
	ldi mpr, 0b00000000 ; This is the only way to circumvent any compare=
	rjmp OCR_CALCULATED
CON_2:
	; CONDItiON 2: MIN SPEED
	cpi mpr, 0b00000000
	brne CON_3
	ldi mpr, 0b00000011 ; Very close to, but not quite, 0- see above
	rjmp OCR_CALCULATED
CON_3: 
	mul mpr, mpr2 ; Moves to R1:R0. We only care about least significant byte, since max = 255
	mov mpr, r0   ; Now contains # of active high duty cycles
OCR_CALCULATED:
	out OCR0, mpr ; Set next time to do the falling edge based on speed.
	
	; End Logical body; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	pop mpr2
	ret

;-----------------------------------------------------------
; Func:	ClockCompareInterrupt
; Desc:	Turns off the motor
;-----------------------------------------------------------
ClockCompareInterrupt: ; FALLING EDGE
	push mpr2
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ldi ZL, low(SPEED)
	ldi ZH, high(SPEED)
	
	; RESET OCR
	rcall ResetOCR
	
	; DISPLAY
	ld mpr, Z ; mpr = SPEED
	ori mpr, 0b10010000 ; Turn on motors
	st Z, mpr

	st Z, mpr
	rcall DoSpeed
	; Pre-Cleanup (Flush flags)
	ldi mpr, 0b00000011 ; (1<<OFC0) | (1<<TOV0) ; Flags
	out TIFR, mpr
	; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	pop mpr2
	reti

;-----------------------------------------------------------
; Func:	ClockOverflowInterrupt
; Desc:	Turns the motor back on
;-----------------------------------------------------------
ClockOverflowInterrupt: ; RISING EDGE
	push mpr2
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ldi ZL, low(SPEED)
	ldi ZH, high(SPEED)
	
	; DISPLAY
	ld mpr, Z ; mpr = SPEED
	andi mpr, 0b01101111 ; Turn off motors
	st Z, mpr

	st Z, mpr
	rcall DoSpeed
	; Pre-Cleanup (Flush flags)
	ldi mpr, 0b00000011 ;(1<<TOV0) | (1<<OFC0); Flags
	out TIFR, mpr
	; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	pop mpr2
	reti

;-----------------------------------------------------------
; Func:	IncreaseSpeed
; Desc:	Increments the lower nibble of SPEED by 1, but not 
;		past 15
;-----------------------------------------------------------
IncreaseSpeed:
	push mpr2
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ldi ZL, low(SPEED)
	ldi ZH, high(SPEED)
	ld mpr, Z
	mov mpr2, mpr ; Split tekbot nibble and number nibble
	andi mpr2, 0b11110000 ; Mask to just tekbot output
	andi mpr, 0b00001111 ; Mask to just a 4 bit number
	cpi mpr, 0b00001111
	brge AT_MAX_SPEED ; Don't go out of bounds
	inc mpr
	or mpr, mpr2 ; Merge nibbles
	st Z, mpr
AT_MAX_SPEED:
	rcall ResetOCR
	; Pre-cleanup
	ldi mpr, 0b00001111
	out EIFR, mpr
	; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	pop mpr2
	reti

;-----------------------------------------------------------
; Func:	DecreaseSpeed
; Desc:	Decrements the lower nibble of SPEED by 1, but not
;		below 0.
;-----------------------------------------------------------
DecreaseSpeed:
	push mpr2
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ldi ZL, low(SPEED)
	ldi ZH, high(SPEED)
	ld mpr, Z
	mov mpr2, mpr ; Split tekbot nibble and number nibble
	andi mpr2, 0b11110000 ; Mask to just tekbot output
	andi mpr, 0b00001111 ; Mask to just a 4 bit number
	cpi mpr, 0b00000001
	brlt AT_MIN_SPEED ; Don't go out of bounds
	dec mpr
	or mpr, mpr2 ; Merge nibbles
	st Z, mpr
AT_MIN_SPEED:
	rcall ResetOCR
	; Pre-cleanup
	ldi mpr, 0b00001111
	out EIFR, mpr
	; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	pop mpr2
	reti

;-----------------------------------------------------------
; Func:	MaxSpeed
; Desc:	Sets the lower nibble of SPEED to 0b1111
;-----------------------------------------------------------
MaxSpeed:
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ldi ZL, low(SPEED)
	ldi ZH, high(SPEED)
	ld mpr, Z
	ori mpr, 0b00001111
	st Z, mpr
	
	rcall ResetOCR
	; Pre-cleanup
	ldi mpr, 0b00001111
	out EIFR, mpr
	; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	reti

;-----------------------------------------------------------
; Func:	MinSpeed
; Desc:	Sets the lower nibble of SPEED to 0b0000
;-----------------------------------------------------------
MinSpeed:
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ldi ZL, low(SPEED)
	ldi ZH, high(SPEED)
	ld mpr, Z
	andi mpr, 0b11110000
	st Z, mpr
	
	rcall ResetOCR
	; Pre-cleanup
	ldi mpr, 0b00001111
	out EIFR, mpr
	; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	reti

;-----------------------------------------------------------
; Func:	DoSpeed
; Desc:	Displays SPEED to PORTB.
;-----------------------------------------------------------
DoSpeed:
	push ZL
	push ZH
	push mpr
	in mpr, sreg
	push mpr
	; Logical body
	ldi ZL, low(SPEED)
	ldi ZH, high(SPEED)
	ld mpr, Z
	out PORTB, mpr
	; Cleanup
	pop mpr
	out sreg, mpr
	pop  mpr
	pop ZH
	pop ZL
	ret

;***********************************************************
;*	Stored Program Data
;***********************************************************
		; Enter any stored data you might need here
.dseg
.org $0100
SPEED: ; The speed (0 - 15) 
	.byte 1

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program
