; Eric Prather (prathere 932580666)
; January 27, 2020
;***********************************************************
;*
;*	Prather_Eric_Lab4_sourcecode.asm
;*
;*	This is Lab 4 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Eric Prather (prathere@oregonstate.edu)
;*           (932580666)
;*	   Date: January 27, 2020
;*
;***********************************************************

.include "m128def.inc"			; Include definition file
;.include "LCDDriver.asm"        ; Include Driver

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register is
								; required for LCD Driver
.def	mpr2 = r23				; I need one too!
.def	loop_count = r24		;
.def	init_loop_count = r25
;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer (Code from Lab 1)
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND
		
		; Initialize LCD Display;
		rcall LCDInit ; In LCDDriver.asm

		; Initialize Port D for input (Code from Lab 1)
		; Unclear if this is handled by LCDInit as well, 
		; but I think it's best to be safe and double-check
		; it all here.
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Move strings from Program Memory to Data Memory
		; STRING_NAME - STRING_NAME_END
		; STRING_HW - STRING_HW_END
		; Haven't figured out how to do this yet.
		
		
		; Do not use the LED outputs because they are reserved 
		; by the LED drivers.

		; rjump MAIN
		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
; $0100 = line 1
; $0110 = line 2
; Y register points to data memory
; Z register points to program memory
;***********************************************************
MAIN:							; The Main program
		rcall LCDClr
		ldi init_loop_count, 6
MAIN_LOOP:
		rcall READ_D ; Used to be more in this function but now it's pretty desolate
		andi mpr2, 0b00000001
		cpi mpr2, 0b00000001 ; only care abt one bit for conditional
		breq SKIP1

		; Case 1: Pressed 0
		ldi YL, low($0100)
		ldi YH, high($0100)
		ldi ZL, low(STRING_NAME << 1)
		ldi ZH, high(STRING_NAME << 1)
		rcall WRITE_Z_Y
		ldi YL, low($0110)
		ldi YH, high($0110)
		ldi ZL, low(STRING_HW << 1)
		ldi ZH, high(STRING_HW << 1)
		rcall WRITE_Z_Y
		rcall LCDWrite
SKIP1:
		rcall READ_D
		andi mpr2, 0b00000010
		cpi mpr2, 0b00000010
		breq SKIP2

		; Case 1: Pressed 0
		ldi YL, low($0110) ; note this is swapped from case 1
		ldi YH, high($0110)
		ldi ZL, low(STRING_NAME << 1)
		ldi ZH, high(STRING_NAME << 1)
		rcall WRITE_Z_Y
		ldi YL, low($0100) ; note this is swapped from case 1
		ldi YH, high($0100)
		ldi ZL, low(STRING_HW << 1)
		ldi ZH, high(STRING_HW << 1)
		rcall WRITE_Z_Y
		rcall LCDWrite
SKIP2:
		rcall READ_D
		andi mpr2, 0b10000000
		cpi mpr2, 0b10000000
		breq MAIN_LOOP

		; Case 3: Pressed 7 (clear)
		rcall LCDClr

		rjmp	MAIN_LOOP		; jump back to main and create an infinite
								; while loop.  Generally, every main program is an
								; infinite while loop, never let the main program
								; just run off

		rjmp	MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************
;-----------------------------------------------------------
; Func: WRITE_Y_Z
; Desc: Writes the contents of Y (in data memory) from Z
; (in program memory). Uses X as a buffer.
;-----------------------------------------------------------
WRITE_Z_Y:
	mov loop_count, init_loop_count
LOOP_WRITE_Z_Y:
	;lpm r27, Z+ ; XL = r27
	;lpm r26, Z+ ; XH = r26
	;st Y+, r27
	;st Y+, r26
	;inc ZL

	; IMPORTANT: Multiply ALL PROGRAM MEMORY ADRESSES BY TWO when assigning.
	; Following four lines are ARGUMENTS: they are set by the caller
	; before responsibility is passed to this function.
	; ldi start_of_string_low addr ZL
	; ldi start_of_string_high addr ZH
	; ldi data_memory_target_low YL
	; ldi data_memory_target_high YH

	; Following lines depend on Y and Z being initalized as above
	; Read 2 byte from program memory, write 2 byte to data memory.
	lpm r26, Z+;
	lpm r27, Z+;
	st Y+, r26;
	st Y+, r27;

	;inc ZL ; Point to next byte in program memory
	;inc YL ; Point to next byte in data memory


	; Loop until word is done.
	cpi loop_count, 0
	dec loop_count
	brne LOOP_WRITE_Z_Y
	ret

;-----------------------------------------------------------
; Func: Read D
; Desc: Puts PIND values as required into mpr2
;-----------------------------------------------------------
READ_D:
		in mpr2, PIND
		;andi mpr2, (1<<0|1<<1|1<<7) ; set all unused bits to 0?
		ret
;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variables by pushing them to the stack

		; Execute the function here
		
		; Restore variables by popping them from the stack,
		; in reverse order

		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING_BEG:
.DB		"My Test String"		; Declaring data in ProgMem
STRING_END:

STRING_NAME:
.DB		"Eric Prather"
STRING_NAME_END:

STRING_HW:
.DB		"Hello World!"
WTRING_HW_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver


