# Doggerel

Doggerel is a programming language **dimensional analysis** is the
**static analysis**. Data is formed into vectors with explicit dimensionalities:
we can declare units, define how they relate to each other, use values with some
automatic conversions, and encounter static failures when dimensions are
misused.

The goal of Doggerel is to make it *easier* to ...

- Communicate reasoning with numbers,
- Use units and dimensions correctly, and
- Automatically verify units and dimensions.

... while making it *harder* to ..

- Deceive with numbers, and
- Misuse units or dimensions on accident.

## Language Overview

With Doggerel, values are tagged with their **units of measure**. For
example in the expression `5 centimeter/second`, the value of `5.0`
is recored as measured in units of *centimeters per second*. Implicitly, this
value also has a **dimensionality** of `length/time`.

The following simple program defines some dimensions, defines some units in
those dimensions and defines how units of the same dimension relate to each
other. Named variables are declared in these units, and finally we print out the
result of a math expression with a desired display units.

```
# Filename: example.dog
# Declare some dimensions;
dim length;
dim time;

# Declare some time units:
unit second of time;
unit minute of time;
unit hour of time;
convert minute = 60 * second;
convert hour = 60 * minute;

# Declare some length units:
unit meter of length;
unit kilometer of length;
unit mile of length;
convert kilometer = 1000 * meter;
convert mile = 1.60934 * kilometer;

# Use them:
let accelerationDueToGravity = 9.81 meter/second^2;
let timeToImpact = 4.2 second;
let speedAtImpact = accelerationDueToGravity * timeToImpact;

# Print the resulting value in the desired alternative units.
print speedAtImpact with units: mile/hour;
```

With the Doggerel runtime, we can evaluate this file by piping it in.

```
$ doggerel example.dog
speedAtImpact = {92.16647818360322 mile·hour⁻¹}
```

Alternatively, an interactive mode is supported using the `--repl` argument. For
example, using the `--stdlib` argument to load the standard library, and using
the `:dims` macro to inspect the dimensionality of an expression.

```
$ doggerel --repl --stdlib
 Initializing Doggerel repl...
╒╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╕
╵0   ╵⅙   ╵⅔   ╵½   ╵⅔   ╵⅚   ╵1
Loaded stdlib
Ready
> let densityOfPetrol = 748.9 kilogram/meter^3;
> let lengthOfTank = 1 meter;
> let heightOfTank = 40 centimeter;
> let depthOfTank = 50 centimeter;
> let volumeOfTank = lengthOfTank * (heightOfTank * depthOfTank);
> print volumeOfTank * densityOfPetrol;
volumeOfTank × densityOfPetrol = {149.78 kilogram}
> :dims volumeOfTank
{ length³ }
```
