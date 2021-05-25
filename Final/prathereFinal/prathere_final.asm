;***********************************************************
;*	This is the final exam template for ECE375 Winter 2020
;***********************************************************
;*   Author: Eric Prather
;*   Date: March 19, 2020
;*	 Original Author: Youngbin Jin
;*   Original Date: March 13th, 2020
;***********************************************************
.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;* I am going to comment out some of these to reduce my "lines of code".
;***********************************************************
;.def	rlo = r0				; Low byte of MUL result
;.def	rhi = r1				; High byte of MUL result
;.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
;.def	A = r3					; A variable
;.def	B = r4					; Another variable
.def	mpr = r16				; Multipurpose register 
;.def	oloop = r17				; Outer Loop Counter
;.def	iloop = r18				; Inner Loop Counter
;.def	dataptr = r19			; data ptr

;;;;;;;;;;;;;;;;;
; MACROS ;;;;;;;;
;;;;;;;;;;;;;;;;;

        ;        As we learned
		; in class, it is very costly to do things like
		; rcall and ret, so I will avoid those whenever
		; possible and use macros instead. Because we 
		; are evaluated based on the LINES OF CODE
		; and not the SIZE OF THE PROGRAM FILE, this 
		; is okay for this project, although in a real 
		; world scenario where program memory is sparse
		; this macro-heavy approach would not be desired.

; @0 is the register to square
.macro DoSquare
		lpm @0, Z+ ; r16 = mpr 0
		muls @0, @0 ; r0, r1 contain square
		st Y+, r0 ; May need to swap order
		st Y+, r1 ; Result1 = r0, r1,...
.endm

; Depends on Y being set to output and Z being set to input
; @0 = sqrt output register (used later in program)
; @1 = unique ID for macro call (one per call) (not needed?)
.macro CalculateIndividualTreasure
		; Compute squares
		DoSquare r16 ; I defined this macro to save lines of assembler code (Extra credit).
		mov r17, r0 ; previously r18, r0
		mov r18, r1
		DoSquare r16
		; Sum squares
		add r17, r0 
		adc r18, r1 ; Does this work for signed numbers?
		st Y+, r17 ; May need to swap order
		st Y+, r18
		; Compute Square Root
		
		ldi @0, 0
		; Base case: 0
		; Unfortunately, this base case is a waste of space and cycles if it is not possible
		; for the coordinates of the treasure given to be (0,0).
		cpi r17, 0
		brne SQRLOOP
		cpi r18, 0
		breq DONE_SQR
SQRLOOP: ; Previously SQRLOOP\@, had to use numbered labels
		inc @0
		mul @0, @0 ; Signed multiplication shouldn't be necessary
		; because the added terms were, by definition, positive.
		cp r1, r18 ; compare most significant byte
		; We assume that the muls result will always be positive
		brlo SQRLOOP ; if r1 < r19, we need to repeat ; NOT brlt
		breq LEAST_SIG ; Same most significant bit as target value
		rjmp DONE_SQR ; We surpassed the target value ; NOT brsh
LEAST_SIG: ; Previously SQRLOOP\@, had to use numbered labels
		cp r0, r17 ; if r1 == r19 && r0 < 
		brlo SQRLOOP ; NOT brlt, this will cause an error because
		; the least significant byte's most significant bit would
		; be interpreted as a two's compliment flag
		
		; Always round UP for square roots.
DONE_SQR: ; Previously DONE_SQR\@, had to use numbered labels
		st Y+, @0
.endm

; @0 = literal value to write
; uses mpr
.macro WriteBest
		ldi mpr,  @0
		st Y+, mpr
		rjmp COMPUTE_AVG_DIST;
.endm

;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
;.dseg
;.org	$0100						; data memory allocation for operands
;operand1:		.byte 2				; allocate 2 bytes for a variable named op1


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
		;clr		zero
		; Initialize Stack Pointer
		
		; For the final project, I do NOT have to initalize
		; the stack pointer because I avoid using the 
		; stack in order to minimze cycles. As we learned
		; in class, it is very costly to do things like
		; rcall and ret, so I will avoid those whenever
		; possible and use macros instead. Because we 
		; are evaluated based on the LINES OF CODE
		; and not the SIZE OF THE PROGRAM FILE, this 
		; is okay for this project, although in a real 
		; world scenario where program memory is sparse
		; this macro-heavy approach would not be desired.
		
		; Init the 2 stack pointer registers
		; Initialize Stack Pointer (Code from Lab 1)
		;ldi		mpr, low(RAMEND)
		;out		SPL, mpr		; Load SPL with low byte of RAMEND
		;ldi		mpr, high(RAMEND)
		;out		SPH, mpr		; Load SPH with high byte of RAMEND

MAIN: 
		; Get first treasure addr
		ldi ZL, low(Treasure1<<1)
		ldi ZH, high(Treasure1<<1)
		ldi YL, low(Result1)
		ldi YH, high(Result1)
		; See macro definitions above
		CalculateIndividualTreasure r19 ;A
		; Z = Treasure2 << 1, Y = Result 2
		CalculateIndividualTreasure r20 ;B
		; Z = Treasure2 << 1, Y = Result 2
		CalculateIndividualTreasure r21 ;C
		; Z = Garbage, Y = BestChoice
		; Figure out the best choice
		cp r19, r20
		breq A_AND_B
		cp r19, r21
		breq A_AND_C
		cp r20, r21
		breq B_AND_C
		; AT THIS POINT IT IS GARUNTEED:
		; A != B != C
		brlo B_VS_A ; b < C
		rjmp C_VS_A ; b > C

B_VS_A: ; B < C
		cp r19, r20
		brlo WRITE_A
		WriteBest 2
		
C_VS_A: ; C < B
		cp r19, r21
		brlo WRITE_A
		WriteBest 3

WRITE_A:
		WriteBest 1

B_AND_C:
		cp r21, r19
		brlo BOTH_2 ; B and C are lowest
		WriteBest 1 ; A is lowest

A_AND_C:
		cp r19, r20
		brlo BOTH_2 ; A and C closer than B
		WriteBest 2; B is best

A_AND_B: ; First checked, so this is the path to all 3
		cp r20, r21
		breq ALL_3
		; C is less than or greater than A, B
		brlo BOTH_2; A and B are closer than C
		WriteBest 3
BOTH_2:
		WriteBest -2
ALL_3:
		WriteBest -3

		; Y = Average Distance 
COMPUTE_AVG_DIST:
		clr mpr ; quotient
		clr XL ; least significant
		clr XH ; most significant
		clr r18 ; zero
		add XL, r19
		adc XH, r18
		add XL, r20
		adc XH, r18
		add XL, r21
		adc XH, r18

		; Divide by 3 in AVR? ;

		; We must be especially careful to handle the special case
		; where the average distance is 0, 0.33, or 0.66.
		; Check to see if we are at at LEAST 3, special case @ 2
		cpi XH, 0
		brne DIV_BY_3
		cpi XL, 2
		brlo END_OF_DIV ; Final answer will be average distance 0
		breq ADD_1_TO_DIV_IN
		rjmp DIV_BY_3
ADD_1_TO_DIV_IN:
		inc XL
DIV_BY_3:
		sbiw X, 3 ; For each time we remove an instance of 3...
		inc mpr ; Add one to the quotient
		cpi XH, 0
		brne DIV_BY_3
		cpi XL, 3
		brsh DIV_BY_3 ; Branch if same higher
		; Round quotient based on remainder (Value in XL is 0, 1, or 2)
		; Each of those three possibilities rounds to a distinct output.
		cpi XL, 1
		brlo END_OF_DIV ; value in quotient: 0
		breq END_OF_DIV ; value in quotient: 1
		inc mpr ; Round up, value in quotient: 2
		; Store rounded quotient in memory
END_OF_DIV:
		st Y+, mpr
; end my code
	
		jmp	Grading

;***********************************************************
;*	Procedures and Subroutines
;***********************************************************
; your code can go here as well
; I did not use procedures nor subroutines because they are slower than macros and I 
; had ample register space

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
