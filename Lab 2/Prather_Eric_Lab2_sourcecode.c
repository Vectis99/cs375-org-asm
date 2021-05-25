
/*
; Eric Prather (932580666): prathere@oregonstate.edu
; January 15, 2019
File: main.c
Version: sourcecode

The following text is provided by the skeleton code framework and provides 
a useful reference for port mapping:

	This code will cause a TekBot connected to the AVR board to
	move forward and when it touches an obstacle, it will reverse
	and turn away from the obstacle and resume forward motion.

	PORT MAP
	Port B, Pin 4 -> Output -> Right Motor Enable
	Port B, Pin 5 -> Output -> Right Motor Direction
	Port B, Pin 7 -> Output -> Left Motor Enable
	Port B, Pin 6 -> Output -> Left Motor Direction
	Port D, Pin 1 -> Input -> Left Whisker
	Port D, Pin 0 -> Input -> Right Whisker
*/

// Frequency of the CPU:
#define F_CPU 16000000

// Board library:
#include <avr/io.h>

// C libraries:
#include <util/delay.h>
#include <stdio.h>

// Useful Macros
// Active low.
#define LEFT_BUMBER_PRESSED !bit_is_set(PIND, PD1)// (PIND & 0x01)
#define RIGHT_BUMBER_PRESSED !bit_is_set(PIND, PD0)//(PIND & 0x02)

// Initialize IO registers
// Uses macros defined in <avr/io.h>
// Manual: https://www.nongnu.org/avr-libc/user-manual/io_8h_source.html
// Therefore our specific IO file is avr/iom128.h, available at:
// https://github.com/vancegroup-mirrors/avr-libc/blob/master/avr-libc/include/avr/iom128.h
// Don't import this file directly though. Just reference it to know the macros you need.
void init_io()
{
	// Port B: Motors
	// IMPORTANT: The endianness of these registers is not consistent: Typically they are:
	// [7, 6, 5, 4, 3, 2, 1, 0]
	// or maybe they are just active low
	// Utilized motors are output, un-utilized pins are input.
	DDRB = 0b11110000; // 1 = output, 0 = input
	PORTB = 0b11110000; // Meaning varies based on io
	// Port D: Bumpers
	DDRD = 0b11111100; // Might as well all be input
	PORTD = 0b11111100; // Put pullup on unused pins, no pullup on used pins
	// However, the behaviors isn ot different based on whether pullup is enabled on the whiskers.
	// So should PORTD set the pullup or not?
	// Shouldn't I be using `PORTX |= (1<<PX#) | ...`?
}


// Drive using intuitive true/false values (instead of whatever the motor needs)
void drive(int leftOn, int leftDir, int rightOn, int rightDir)
{
	//This doesn't work.
	//PORTB |= (leftOn<<PB7) || (leftDir<<PB6) || (rightOn<<PB4) || (rightDir<<PB5);
	if(leftOn)
		PORTB |= (1<<PB7);
	else
		PORTB &= ~(1<<PB7);
	if(rightOn)
		PORTB |= (1<<PB4);
	else
		PORTB &= ~(1<<PB4);
	if(leftDir)
		PORTB |= (1<<PB6);
	else
		PORTB &= ~(1<<PB6);
	if(rightDir)
		PORTB |= (1<<PB5);
	else
		PORTB &= ~(1<<PB5);
}

// Go backwards for 1000 seconds
// Formerly this function took a parameter, but _delay_ms must only have a compile time constant
void reverse_step()
{
	drive(1,0,1,0);
	_delay_ms(1000); // Yield
}


void Update()
{
	if(LEFT_BUMBER_PRESSED) // left
	{
		reverse_step();
		drive(1,1,1,0); // turn right
		_delay_ms(1000);
	}
	else if(RIGHT_BUMBER_PRESSED)
	{
		reverse_step();
		drive(1,0,1,1); // turn right
		_delay_ms(1000);
	}
	else // Just go forward
		drive(1,1,1,1);
}

// Main method called at program start
int main(void) // No arguments
{
	init_io();
	while (1) // loop forever
	{
		Update();
	}
	return 1; // If we reach this, an error has occured (break; in while loop)
}
