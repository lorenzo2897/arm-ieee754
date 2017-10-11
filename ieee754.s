; Lorenzo Silvestri
;
; CArch final project
; Option 2 - IEEE 754 floating point operations

; ***************************************************************
; ***************************************************************

FDIV
; r0 in = op1
; r1 in = op2
; r0 out = op1 / op2

; push r4 to r11 onto stack
stmed r13!, {r4-r11, r14}


; separate exponents and mantissas
; (extract the two floats into 4 words)
; r4 = mantissa op1
; r5 = exponent op1
; r6 = mantissa op2
; r7 = exponent op2

bl INTERNAL_EXTRACT

; put the signs back in
and r0, r0, #0b10000000000000000000000000000000 ; mask for MSB
and r1, r1, #0b10000000000000000000000000000000 ; mask for MSB
orr r4, r4, r0
orr r6, r6, r1

; make the denominator between 0.5 and 1
orr r6, r6, #0b00111111000000000000000000000000

; adjust the numerator so that (aP / bP == a / b) still holds
sub r5, r5, r7
sub r5, r5, #1
and r5, r5, #0xff
orr r4, r4, r5, lsl #23

; aP is now in r4
; bP is now in r6

; get a first estimate for x and store it in r10
F48_17 dcd 0x4034b4b5 ; precomputed 48 / 17
F32_17 dcd 0x3ff0f0f1 ; precomputed 32 / 17
ldr r8, =F48_17
ldr r8, [r8]
ldr r9, =F32_17
ldr r9, [r9]
; x = 48/17 - (32/17 * bP);
mov r0, r6
mov r1, r9
bl FMUL
mov r1, r0
mov r0, r8
bl FSUB
mov r10, r0

; do 3 times
mov r11, #3 ; counter
FDIV_LOOP
	; x = x + (x * (1 - (bP * x)))
	mov r0, r10
	mov r1, r6
	bl FMUL
	mov r1, r0
	mov r0, #0b00111111100000000000000000000000 ; 1.0
	bl FSUB
	mov r1, r10
	bl FMUL
	add r10, r10, r0
sub r11, r11, #1
cmp r11, #0
bne FDIV_LOOP

; multiply aP * x
mov r0, r4
mov r1, r10
bl FMUL

; r0 now contains the result of the division

; pop r4-r11 off the stack
; return control
ldmed r13!, {r4-r11, pc}


; ***************************************************************
; ***************************************************************

FMUL
; r0 in = op1
; r1 in = op2
; r0 out = op1 * op2

; push r4 to r11 onto stack
stmed r13!, {r4-r11, r14}

; before we forget, just determine the sign and put it into r11
; this way, no matter what happens we have the correct sign!
lsr r11, r0, #31
eor r11, r11, r1, lsr #31

; check zeros
mov r2, #0
cmp r2, r0, lsl #1
moveq r0, #0
beq FMUL_DONE
cmp r2, r1, lsl #1
moveq r0, #0
beq FMUL_DONE

; separate exponents and mantissas
; (extract the two floats into 4 words)
; r4 = mantissa op1
; r5 = exponent op1
; r6 = mantissa op2
; r7 = exponent op2

bl INTERNAL_EXTRACT

; this extra bit is needed for denormalised inputs
cmp r5, #0
addeq r5, r5, #1
cmp r7, #0
addeq r7, r7, #1

; now that we are done extracting, set r0 to 0 because we will mask stuff into it later
mov r0, #0

; we have to multiply the mantissas and add the exponents

; multiply r4 * r6 -> r9:r8
mov r8, #0 ; prepare for result
mov r9, #0 ; prepare for result
mov r2, #0 ; r2 holds extra bits from r4 when it gets shifted left
FMUL_LOOP
	; if r6 is a multiple of 2, divide it by 2 and multiply r4 by 2
	; (basically juggling a factor of two between the two factors)
	; this is only to make the multiplication a bearable speed, but not very compact!
	tst r6, #1
	bne FMUL_SKIP_DIVIDE
	lsr r6, r6, #1
	adds r4, r4, r4
	adc r2, r2, r2
	b FMUL_LOOP ; branching back at this point makes the multiplication faster, but is not essential
	FMUL_SKIP_DIVIDE

	; otherwise, 
	adds r8, r8, r4
	adc r9, r9, r2
	sub r6, r6, #1
	cmp r6, #0
	bgt FMUL_LOOP

; done multiplying, and the product of the mantissas is now in r9:r8

; calculate exponent
; remember that the exponent will need to have 46 subtracted from it,
; since we have done integer multiplication
; therefore, new exponent = exp1 + exp2 - 127
; store it in r2
add r2, r5, r7
sub r2, r2, #127-18

; loop, using r2 as a decrementing counter,
; shifting the mantissa left by 1 each time until we find a 1
FMUL_LOOP_FINDMANTISSA
	
	; decrement counter
	sub r2, r2, #1
	
	; if counter == 0, it means no "1" was found.
	; branch into special case: denormalised output or underflow
	cmp r2, #0
	ble FMUL_WITHOUT_EXPONENT
	
	; shift r9:r8 left by 1
	adds r8, r8, r8
	adcs r9, r9, r9
	
	; if carry is set, branch out of the loop
	; (we found the most significant 24 bits)
	bcs FMUL_WITH_EXPONENT

b FMUL_LOOP_FINDMANTISSA

FMUL_WITH_EXPONENT
	; check for overflow
	cmp r2, #0xff
	bge FMUL_OVERFLOW
	; shift the exponent into place
	lsl r0, r2, #23

FMUL_WITHOUT_EXPONENT
	; frac of mantissa is the 23 MSBs in r9
	; if branched directly here, it means exponent is 0,
	; so float is denormalised.
	; otherwise exponent has already been set above

	; check for underflow
	cmp r2, #0
	blt FMUL_UNDERFLOW

	; shift the mantissa into place
	orrs r0, r0, r9, lsr #9
	; round: if the next bit is 1, round up
	addcs r0, r0, #1
	; special case: if exactly halfway between two numbers, round towards even
	;lsl r10, r9, #23
	;cmp r10, #0b10000000000000000000000000000000 ; testing the MSB
	; rounding towards even means truncating the LSB
	;lsreq r0, r0, #1
	;lsleq r0, r0, #1
b FMUL_DONE

; if out of the bounds 0-255, we have an under/over-flow
FMUL_OVERFLOW
mov r0, #0xff ; infinity
lsl r0, r0, #23
b FMUL_DONE

FMUL_UNDERFLOW
mov r0, #0 ; zero


FMUL_DONE
orr r0, r0, r11, lsl #31 ; add in the sign no matter what
; pop r4-r11 off the stack
; return control
ldmed r13!, {r4-r11, pc}


; ***************************************************************
; ***************************************************************

FADD
; r0 in = op1
; r1 in = op2
; r0 out = op1 + op2

; push r4 to r6 onto stack
stmed r13!, {r4-r6, r14}

; compute which one is bigger and store in r10
; 0 means op1 >= op2
; 1 means op1 < op2
mov r6, #0

; get difference of exponents
mov r4, r0, lsr #23
and r4, r4, #0xff
mov r5, r1, lsr #23
and r5, r5, #0xff
cmp r4, r5
movlt r6, #1
bne FADD_DONE_COMP
; if exponents are equal, compare the mantissas
lsl r4, r0, #9
lsr r4, r4, #9
lsl r5, r1, #9
lsr r5, r5, #9
cmp r4, r5
movlt r10, #1


FADD_DONE_COMP
; we now know which one is bigger, and it is stored in r10
; if r10 = 1, swap the two inputs
cmp r6, #1
moveq r2, r0
moveq r0, r1
moveq r1, r2
; calculate whether we need to add or subtract (and put it into r2)
mov r2, r0, lsr #31
eor r2, r2, r1, lsr #31

; call the addsub routine
bl DOADDSUB

; figure out the sign now
; because op1 is always >= op2 (we switch them earlier if needed),
; the result is negative if and only if op1 is negative.
; this makes it easy for us because op1 is already in r0!
and r0, r0, #0b10000000000000000000000000000000 ; mask for MSB

; add in the magnitude
orr r0, r0, r3

; pop r4-r6 off the stack
; return control
ldmed r13!, {r4-r6, pc}

; ***************************************************************
; ***************************************************************

FSUB
; r0 in = op1
; r1 in = op2
; r0 out = op1 - op2

; swap sign of op2 and then add
eor r1, r1, #0b10000000000000000000000000000000 ; mask for MSB
b FADD

; ***************************************************************
; ***************************************************************


DOADDSUB ; subroutine to add/substract two floats
; assumes a is larger than b
; only deals with magnitudes
; r0 = op1
; r1 = op2
; r2 = operation to perform (0 for add, 1 for sub)
; r3 = result

; push r4 to r10 onto stack
stmed r13!, {r4-r10, r14}

; extract the two floats into 4 words
; r4 = mantissa op1
; r5 = exponent op1
; r6 = mantissa op2
; r7 = exponent op2

bl INTERNAL_EXTRACT

; compute the difference in exponents
; which is also the amount of bits we need to shift op2 mantissa to the right
sub r7, r5, r7
; here we compute 32 - that difference, which will be used later
rsb r3, r7, #32

; now we're going to shift op2 mantissa right, adding the remainder into r8
; store the remainder of the shift into r8 (lsl 32-r7)
lsl r8, r6, r3
; shift op2 mantissa right by r7, keeping in r6
lsr r6, r6, r7

; clean r3, because we will construct the float in it later
mov r3, #0

; perform the operation (add or sub?)
cmp r2, #0
beq ADDSUB_ADD ; if r2 == 0, do add
; else, do subtraction
rsbs r8, r8, #0 ; r8 := 0 - r8
sbc r4, r4, r6
b ADDSUB_FIND_MSBS
ADDSUB_ADD add r4, r4, r6

ADDSUB_FIND_MSBS
; find the most significant 24 bits to act as mantissa:
; this means finding the most significant "1" in r4:r8

; add 9 to the exponent in r5 (this performs an 8-bit left shift in a very compact way)
add r5, r5, #9


; loop, using r5 as a decrementing counter,
; shifting the mantissa left by 1 each time until we find a 1
ADDSUB_LOOP_FINDMANTISSA
	
	; shift r4:r8 left by 1
	adds r8, r8, r8
	adcs r4, r4, r4
	
	; decrement counter
	sub r5, r5, #1
	
	; if carry is set, branch out of the loop
	; (we found the most significant 24 bits)
	bcs ADDSUB_WITH_EXPONENT
	
	; if counter == 0, it means no "1" was found.
	; branch into special case: denormalised output or zero
	cmp r5, #0
	beq ADDSUB_WITHOUT_EXPONENT

b ADDSUB_LOOP_FINDMANTISSA

ADDSUB_WITH_EXPONENT
	; shift the exponent into place
	lsl r3, r5, #23
	; check for infinity
	cmp r5, #0xff
	beq ADDSUB_DONE

ADDSUB_WITHOUT_EXPONENT
	; frac of mantissa is the 23 MSBs in r4
	; if branched directly here, it means exponent is 0,
	; so float might be denormalised, might be 0.
	; otherwise exponent has already been set above

	; shift the mantissa into place
	orrs r3, r3, r4, lsr #9
	; round: if the next bit is 1, round up
	addcs r3, r3, #1
	; special case: if exactly halfway between two numbers, round towards even
	lsl r10, r4, #23
	orr r10, r10, r8, lsr #16
	cmp r10, #0b10000000000000000000000000000000 ; testing the MSB
	; rounding towards even means truncating the LSB
	lsreq r3, r3, #1
	lsleq r3, r3, #1

ADDSUB_DONE

; r3 contains the result of the operation
; pop r4-r10 off the stack
; return control
ldmed r13!, {r4-r10, pc}

; ***************************************************************
; ***************************************************************

INTERNAL_EXTRACT
; this little piece of code (not a subroutine!) is used three times,
; so it was moved here for the sake of compactness.

; takes two floats, r0 and r1
; extracts them into r4 r5, and r6 r7
; it also sets r9 to a mask where only the 24th bit is set

	; exponents
	mov r5, r0, lsr #23
	and r5, r5, #0xff
	mov r7, r1, lsr #23
	and r7, r7, #0xff
	
	; mantissas
	lsl r4, r0, #9
	lsr r4, r4, #9
	lsl r6, r1, #9
	lsr r6, r6, #9
	; add a leading 1 to the mantissas for normalised values
	mov r9, #0b100000000000000000000000 ; mask
	cmp r5, #0
	orrne r4, r4, r9
	cmp r7, #0
	orrne r6, r6, r9

; return control
mov pc, lr



