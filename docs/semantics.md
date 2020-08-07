# Concepts

The core principle of Doggerel is packaging data and doing operations on
**vectors**. In this context, a vector is a collection of one or more primitive
data, where each is labeled with a distinct **dimension**. We refer to these
labeled primitives as **components** of their vectors and the set of their
distinct dimensions as the **dimensionality** of the vector. Typically these
components are numeric data, and their dimension is implied by **units of
measure**.

Like traditional vectors in mathematics, in this environment, we're able to
apply mathematical operations across Doggerel vectors in ways that produce
vectors of the same or potentially different **dimensionalities**.

As with most typed programming languages, static analysis provides limited
guarantees that a program would work as expected given any well-typed inputs
without having to execute the program itself. However, unlike most typed
languages, Doggerel's static analysis checks expectations across vector
dimensionalities. Furthermore, as with polymorphic programming languages, in
some cases, Doggerel's static analysis partially also determines the specific
operations that are performed.

## Dimensions and Units

In Doggerel, **base dimensions** are static symbols declared and unique within a
scope. Similarly, **base units** are static symbols uniquely declared within a
scope, and associated with a dimension.

This implies a many-to-one relationship between units and dimensions. We
furthermore allow for a symbol to be both a base unit and a base dimension. We
call these **parametric units** or **parametric dimensions** (depending on the
context). Unlike non-parametric dimensions, a parametric dimension may be
associated with one and only one unit.

```
                  ┌──┐      ┌──┐      ┌──┐
Dimensions:       │D₁│      │D₂│      │PU│
                  └┬─┘      └┬─┘      └┬─┘
┈┈┈┈┈┈┈┈┈┈┈    ┌───┤     ┌───┼────┐    │
             ┌─┴┐ ┌┴─┐ ┌─┴┐ ┌┴─┐ ┌┴─┐ ┌┴─┐
     Units:  │U₁│ │U₂│ │U₃│ │U₄│ │U₅│ │PU│
             └──┘ └──┘ └──┘ └──┘ └──┘ └──┘

```

### Degree Map Structure

When we talk about the **units** or the **dimension** of a vector component,
however, we are usually referring to a compound expression made up up of these
base units or base dimensionalities respectively. We'll store the expression in
a generic data structure called a **degree map** because it's a mapping from
the base symbol of whichever kind to an integer representing the **degree** of
that symbol.

To illustrate this, we will use real-world units and dimensions.

```
                        ┌──────┐ ┌────┐
Dimensions:             │Length│ │Time│
                        └┬─────┘ └┬───┘
┈┈┈┈┈┈┈┈┈┈┈       ┌──────┤        │
             ┌────┴┐ ┌───┴─────┐ ┌┴─────┐
     Units:  │Meter│ │Kilometer│ │Second│
             └─────┘ └─────────┘ └──────┘

```

In the simplest case, a units expression refers to a single unit, in which case
it maps that symbol to a degree of 1. The units expression *kilometer* would be
encoded with the following degree maps. We find the corresponding dimension of
these units by substituting the base unit with its corresponding base dimension.

```
    Units:  [  meter ↦ 1 ]
Dimension:  [ length ↦ 1 ]
```

With these symbols, we might naturally describe an area in units of *square
meters*, which can be encoded with a similar map that uses a degree of 2.

```
    Units:  [  meter ↦ 2 ]
Dimension:  [ length ↦ 2 ]
```

Or describe units of *meters per second* with a negative degree.

```
    Units:  [  meter ↦ 1 , second ↦ -1 ]
Dimension:  [ length ↦ 1 ,   time ↦ -1 ]
```

With this data structure, we automatically preserve some properties. For
example, when using multiple units of the same dimension, such as *meter
kilometers*, then we merge their dimensions in the dimension mapping by summing
degrees. This results in a dimension of area as expected.

```
    Units:  [  meter ↦ 1 , kilometer ↦ 1 ]
Dimension:  [ length ↦ 2 ]
```

This demonstrates the distinction that while *area* is conceptually a dimension,
it isn't a base dimension, but rather a composite of base dimensions: namely
*length²*.

Implicitly, if any symbol is not mapped in a degree map, we say it is of degree
zero. Thus, if summing identical dimensionalities results in a degree of zero,
then that mapping can be dropped. Consider, for example, how the units given by
*kilometers per meter* are dimensionless.

```
    Units:  [ kilometer ↦ 1 , meter ↦ -1 ]
Dimension:    Ø
```

### Degree Map Operations

Finally, degree maps are closed under two operations: unary **inverse**, and
binary **product**.

Inverting a degree map is defined as the negation of each degree. For example:

```
[ A ↦ 2 , B ↦ 1 , C ↦ -3 ]⁻¹ = [ C ↦ 3 , B ↦ -1 , A ↦ -2 ]
```

The product of two degree maps is a merge of each mapping where the degrees of
matching symbols summed together. For example:

```
[ A ↦ 3 , B ↦ 1, C ↦ -2 ] ×
  [ B ↦ 2 , C ↦ -2, D ↦ -3 ] =
    [ A ↦ 3 , B ↦ 3 , D ↦ -3 ]

```

Finally, this allows us to define a binary **divide** operation across degree
maps as a composition of the product and inverse.

```
m ÷ n = m × n⁻¹
```

### Convertible Units

Because we record the units that quantities are measured in, when equipped with
additional information of how units of the same dimension relate to each other,
in some contexts, it's possible for quantities to be **converted**
automatically. We'll describe these contexts in greater detail below, but for
now we describe the encoding for the relations.

Any two units of the same dimension may be related to each other by a conversion
which is declared within a scope. For any two base units of the same dimension
*p* and *q*, a conversion is a transformation that can be applied to a scalar
quantity was measured in *p* to achieve a quantity measured in *q*.

For example, we might consider a set of units in the dimension of temperature.

```
                      ┌───────────┐
Dimension:            │Temperature│
                      └─────┬─────┘
┈┈┈┈┈┈┈┈┈┈         ┌────────┼───────┐
            ┌──────┴┐ ┌─────┴────┐ ┌┴─────┐
    Units:  │Celsius│ │Fahrenheit│ │Kelvin│
            └───────┘ └──────────┘ └──────┘

```

The familiar conversions would be given by the triples of *(from unit, to unit,
transformation)*. (Here we describe the transformations as lambda expressions
and assign them names for convenience.)

```
( Celsius , Kelvin     , f = λC. C × 273.15     )
( Celsius , Fahrenheit , g = λC. C × (5/9) + 32 )
```

Because these transformations are invertible, these two conversions express a
connected graph among the three units with transformations as edges. We may
describe this graph in the following table.

| From \ To  | Celsius | Fahrenheit | Kelvin  |
|------------|---------|------------|---------|
| Celsius    | id      | f          | g       |
| Fahrenheit | f⁻¹     | id         | f⁻¹ ∘ g |
| Kelvin     | g⁻¹     | g⁻¹ ∘ f    | id      |

Since, in this example dimension, we have defined only three units, this pair of
conversions causes each unit to be mutually reachable via transitive
conversions. We could imagine additionally defining a fourth unit of
temperature, (e.g. Rankine), but do not define an additional conversion, then we
have a disconnected subgraph and, in this table representation, we'll have empty
cells.

In conclusion, some properties of an ideal convertibility graph of a dimension's
units.

- Has no isolated subgraphs in order to maximize usable units.
- Uses minimal transitive conversions in order to minimize roundoff errors.

Either of these can result in a static analysis warning.

## Vectors

### Vector Structure

We encode vectors as a mapping from units to scalar quantities.
