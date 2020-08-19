# Doggerel

Doggerel is a small programming language for doing math where **dimensional
analysis** is a form of **static analysis**. We can declare units, define how
these units relate to each other, use values with some conversions done
automatically and encounter static failures in some cases when dimensions are
misused.

In the abstract, the goal of the language is to make it *easier* to ...

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
$ cat example.dog | doggerel --stdin
speedAtImpact = {92.16647818360322 mile·hour⁻¹}
```

Alternatively, an interactive mode is supported using the `--repl` argument.

```
$ doggerel --repl
 Initializing Doggerel repl...
╒╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╕
╵0   ╵⅙   ╵⅔   ╵½   ╵⅔   ╵⅚   ╵1
Ready
> dim mass;
> dim length;
> unit meter of length;
> unit centimeter of length;
> unit kilogram of mass;
> convert meter = 100 * centimeter;
> let densityOfPetrol = 748.9 kilogram/meter^3;
> let lengthOfTank = 1 meter;
> let heightOfTank = 40 centimeter;
> let depthOfTank = 50 centimeter;
> let volumeOfTank = lengthOfTank * (heightOfTank * depthOfTank);
> print volumeOfTank * densityOfPetrol;
volumeOfTank × densityOfPetrol = {149.78 kilogram}
```
