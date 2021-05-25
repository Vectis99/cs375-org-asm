;***********************************************************
;*	This is the final exam template for ECE375 Winter 2020
;***********************************************************
;*	 Author: Youngbin Jin
;*   Date: March 13th, 2020
;***********************************************************
.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;***********************************************************
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable
.def	mpr = r16				; Multipurpose register 
.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter
.def	dataptr = r19			; data ptr

;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0100						; data memory allocation for operands
operand1:		.byte 2				; allocate 2 bytes for a variable named op1


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment
;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt
.org	$0046					; End of Interrupt Vectors
;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:	; The initialization routine
		clr		zero

; To do
; your code is here
	
		jmp	Grading

;***********************************************************
;*	Procedures and Subroutines
;***********************************************************
; your code can go here as well

;***end of your code***end of your code***end of your code***end of your code***end of your code***
;******************************* Do not change below this point************************************
;******************************* Do not change below this point************************************
;******************************* Do not change below this point************************************

Grading:
		nop					; Check the results and number of cycles (The TA will set a breakpoint here)
rjmp Grading


;***********************************************************
;*	Stored Program Data
;***********************************************************

; Contents of program memory will be changed during testing
; The label names (Treasure1, Treasure2, etc) are not changed
Treasure1:	.DB	0xF9, 0xFD				; X, Y coordinate of treasure 1 (-7 in decimal), (-3 in decimal)
Treasure2:	.DB	0x03, 0x04				; X, Y coordinate of treasure 2 (+3 in decimal), (+4 in decimal)
Treasure3:	.DB	0x81, 0x76				; X, Y coordinate of treasure 3 (-127 in decimal), (+118 in decimal)

;***********************************************************
;*	Data Memory Allocation for Results
;***********************************************************
.dseg
.org	$0E00						; data memory allocation for results - Your grader only checks $0E00 - $0E16
Result1:		.byte 7				; x_squared, y_squared, x2_plus_y2, square_root (for treasure 1)
Result2:		.byte 7				; x_squared, y_squared, x2_plus_y2, square_root (for treasure 2)
Result3:		.byte 7				; x_squared, y_squared, x2_plus_y2, square_root (for treasure 3)
BestChoice:		.byte 1				; which treasure is closest? (indicate this with a value of 1, 2, or 3)
									; see the PDF for an explanation of the special case when 2 or more treasures
									; have an equal (rounded) distance
AvgDistance:	.byte 1				; the average distance to a treasure chest (rounded to the nearest integer)

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
