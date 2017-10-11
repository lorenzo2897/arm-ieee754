# IEEE-754 Floating Point Subroutines


This project was originally coded as the coursework component for EIE1 Computer Architecture, for which it was awarded an A*. The requirement of the coursework was to write the most compact and correct assembly code that implemented IEEE-754 floating-point addition, subtraction, multiplication, and addition.

The code can be assembled by any standard ARM assembler. It is APCS-compliant, which means it can also be linked with C subroutines.

The rest of this page describes in detail the implementation of the code and trade-offs made.

### Addition and subtraction

Core functionality for both addition and subtraction is programmed into a single APCS-compliant subroutine, DOADDSUB, since most of the code is reused in both operations. This subroutine expects the first operand to be greater in magnitude than the second operand (due to how the shifts are performed), so the top-level wrappers FADD and FSUB are required to reorder the operands to feed them correctly to DOADDSUB.

#### DOADDSUB

```
R0 input: op1
R1 input: op2 (must be ≤ op1)
R2 input: operation to perform (0 for addition, 1 for subtraction)
R3 output: result (op1 ± op2)
```

This function performs the operation specified in R2 to the magnitudes of R0 and R1. This means that their signs are ignored, so the calling function is responsible for making the subroutine do the correct operation.

First, it extracts the components of both floats into separate registers, to work with them more easily. This is actually done by branching to a dedicated piece of code because the same action is done when multiplying, but it is not a callable subroutine! (This section also deals with denormalised inputs by deciding whether to add the leading 1 or not).

It computes the difference in exponents, and shifts op2’s mantissa to the right by that amount (because that’s how much smaller op2 is than op1) so that, in essence, we get two fixed point values.

Now it performs the operation specified in R2 (addition or subtraction) on the two fixed points values. All of this is done using two-word resolution.

To reconstruct the result into an IEEE float, we must find the 24 most significant bits. The program loops, shifting the fixed point result right by one every time, and decrementing the exponent, until:

- a) it finds a 1 to act as the most significant bit
- b) the exponent reaches zero, in which case the result will be either denormalised or zero.

Rounding: if the first excluded bit of the mantissa is 1, it will round up. If the number is exactly between two IEEE numbers, it will truncate the LSB (round towards even).

Finally, it pieces the float back together by masking the exponent and the mantissa into R3.

### Addition - FADD

```
R0 input: op1
R1 input: op2
R0 output: op1 + op2
```

First, FADD computes which operand is bigger than the other (first looking at the exponents, and then the mantissas should the exponents be equal).

If it finds that op1 is smaller than op2, it swaps them.

It then checks whether to perform an addition or a subtraction of the magnitudes (by XOR-ing the signs).
After calling the DOADDSUB subroutine, it figures out what sign the resulting float has. This is easy: the sign of op1 always dominates because it is greater in magnitude.

### Subtraction - FSUB

```
R0 input: op1
R1 input: op2
R0 output: op1 - op2
```

FSUB simply wraps FADD and inverts the sign of op2 using an exclusive OR.
It uses a tail call to FADD to save instructions.

### Multiplication – FMUL

```
R0 input: op1
R1 input: op2
R0 output: op1 * op2
```

The first thing FMUL does is determine the sign of the result (op1 sign XOR op2 sign).

It then takes care of multiplication by zero (in which case the result is zero).

If not, it extracts the components of op1 and op2 using the same code as explained in DOADDSUB.

The basic idea is to multiply the mantissas and add the exponents:

#### Multiplication of mantissas

The simplest (and most compact!!) way to achieve this would be to add op1 to itself op2 number of times. However, I found that this method becomes unbearably slow in VisUAL, so I implemented a faster algorithm that takes advantage of when the operands are even to do a multiplication by two.

Basically, every time mantissa2 is a multiple of two, it divides it by two and multiplies mantissa1 by two. It takes advantage of (a) * (b*2) == (a*2) * (b) and juggles the 2 between them.

Unfortunately, this adds about 7 unnecessary instructions to the code.

#### Evaluating the exponent

After the basic addition of the two exponents, it loops through the resulting mantissa to find the 24 MSBs. This alters the exponent just like in DOADDSUB, and the code is very similar.

Of course, it is possible that the resulting exponent falls outside of the representable range -127 to 128, in which case the code handles either an underflow (and returns zero) or an overflow (and returns infinity).

It then pieces together the new float and returns it in R0.


### Division – FDIV

```
R0 input: op1
R1 input: op2
R0 output: op1 / op2
```

Division implements the Newton-Rhapson iteration method as described on Wikipedia, and relies on the other subroutines FADD, FSUB, and FMUL to calculate its result.

I programmed the following C++ code and manually translated it into ARM assembly. Special outputs are handled according to the matrix provided in the project specs.

The functions i2f and f2i are just helper functions that bypass C++ type restrictions, so that the code can compile, but they do not do anything to the numbers.

```c++
float FDIV(float a, float b) {
    if (a == 0) {
        if (b == 0) {
            return NAN;
        }
        else {
            return 0;
        }
    }
    else if (b == 0) {
        return INFINITY;
    }

    // get the exponent of a and b
    int expA = (f2i(a) >> 23 & 0xff);
    int expB = (f2i(b) >> 23 & 0xff);

    // make the denominator between 0.5 and 1
    float bP;
    bP = i2f(f2i(b) & 0b10000000011111111111111111111111);
    bP = i2f(f2i(bP) | 0b00111111000000000000000000000000);
    // adjust the numerator accordingly so that (aP / bP == a / b) still holds
    float aP = a;
    if (expB < 127) {
        for (int i = 0; i > (expB - 126); i--) {
            aP *= 2;
        }
    }
    else {
        for (int i = 0; i < (expB - 126); i++) {
            aP = FHALVE(aP);
        }
    }

    // get a first estimate
    float f48_over_17 = 48 / 17;
    float f32_over_17 = 32 / 17;
    float x = f48_over_17 - (f32_over_17 * bP);

    // iterate 3 times (should be enough for single precision)
    for (int i = 0; i < 3; i++) {
        x = x + (x * (1 - (bP * x)));
    }

    return res = (aP * x);
}
```

Unfortunately, the resulting ARM assembly code turned out to be quite buggy and rarely outputs the correct result.
