# Statements

Doggerel statements are terminated by semicolons. Wherever possible, whitespace
and indentation are ignored, and blocks are delineated by curly braces.

```
let x = 12 mile;
if (x < 20 kilometer) {
  print x with units: meter;
}
```

## Statement options

Some statements support options that modify their behavior. The pattern for
configuring options uses the `with` keyword at the end of the statement followed
by the options themselves and terminating with the statement's semicolon.

Options are a name followed by a colon: and some value. Options are separated by
commas.

```
<statement body> with option1: foo, option2: bar;
```

# Declaring dimensions

Dimensions can be declarend using the `dim` command.

## Base metric dimensions

A dimension can be declared with a simple statement reserving its name. For
example, to reserve a dimension named `jeNeSaisQuoi` the following statement
will work assuming that name hasn't already been taken.

```
dim jeNeSaisQuoi;
```

Following this statement `jeNeSaisQuoi` is a **metric** dimension because it's a
_dimension of measure_. (The term "metric" in this context doesn't relate to the
S.I. units system.) See the section on units for more on the metric/parametric
distinction.

`jeNeSaisQuoi` is called a **base** dimension because it's an atomic building
block for dimensionalities. See the section on **Dimensionality Expressions**
for more.

Dimension names should be camel-cased in this manner.

## Natural parametric dimensions

A natural dimension can be declared using the `natural: true` option.

```
dim rank with natural: true;
```

Following this, the `rank` dimension takes a natural number as its index
(e.g. `rank(4)`).

## Dimension Aliases

Compound dimensional expressions can become cumbersome. For example, the concept
of "speed" is a common stand-in for the `length/time` compound dimensionality.
We can reserve this word for use in dimensionality expressions with an equation
in the declaration.

```
dim speed = length / time;
```

Following this statement, `speed` can be referred to as a metric dimensionality
and even be composed into larger dimensionalities such as:

```
dim acceleration = speed/time
```

Note: this provides a syntax sugar that expands `speed` to `length/time` when
used. It does **not** declare speed as a new base dimension. (i.e. `speed/time`
would be equivalent to `length/time^2`.)

<!-- # Declaring Units

```
┌─────────────────────────────────────────────────────────────┐
│         ( parentheses surround value expressions )          │
│                                                             │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │      [ square brackets surround unit expressions ]      │ │
│ │                                                         │ │
│ │  ┌───────────────────────────────────────────────────┐  │ │
│ │  │ { curly braces surround dimensional expressions } │  │ │
│ │  └───────────────────────────────────────────────────┘  │ │
│ │                                                         │ │
│ └─────────────────────────────────────────────────────────┘ │
│                                                             │
└─────────────────────────────────────────────────────────────┘
``` -->
