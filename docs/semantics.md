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
    Units:  [  Meter ↦ 1 ]
Dimension:  [ Length ↦ 1 ]
```

With these symbols, we might naturally describe an area in units of *square
meters*, which can be encoded with a similar map that uses a degree of 2.

```
    Units:  [  Meter ↦ 2 ]
Dimension:  [ Length ↦ 2 ]
```

Or describe units of *meters per second* with a negative degree.

```
    Units:  [  Meter ↦ 1 , Second ↦ -1 ]
Dimension:  [ Length ↦ 1 ,   Time ↦ -1 ]
```

With this data structure, we automatically preserve some properties. For
example, when using multiple units of the same dimension, such as *meter
kilometers*, then we merge their dimensions in the dimension mapping by summing
degrees. This results in a dimension of area as expected.

```
    Units:  [  Meter ↦ 1 , Kilometer ↦ 1 ]
Dimension:  [ Length ↦ 2 ]
```

This demonstrates the distinction that while *area* is conceptually a dimension,
it isn't a base dimension, but rather a composite of base dimensions: namely
*length²*.

Implicitly, if any symbol is not mapped in a degree map, we say it is of degree
zero. Thus, if summing identical dimensionalities results in a degree of zero,
then that mapping can be dropped. Consider, for example, how the units given by
*kilometers per meter* are dimensionless.

```
    Units:  [ Kilometer ↦ 1 , Meter ↦ -1 ]
Dimension:    Ø
```

In general, a dimensionless units expression will be disallowed and cause a
static analysis failure.

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

Notationally, we define the *d = dimᵤ(u)* function to access the dimension
degree map *d* corresponding to the given unit degree map *u*.

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
( Celsius , Kelvin     , f = λC. C + 273.15     )
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

We encode vectors as a mapping from units to scalar quantities. For example, a
vector described by *{ 88 miles per hour, 1.21 gigawatts }* could be represented
with the following nested map.

```
let a = [ [ Mile ↦ 1 , Hour ↦ -1 ] ↦ 88 , [ Gigawatt ↦ 1 ] ↦ 1.21 ]
```

Likewise, any vector will have a property of **vector dimensionality** which is
the set of dimensions corresponding to each units expression composing the map
keys. In this case, the example vector has a dimensionality described by the
following *dimᵥ(v)* function.

```
dimᵥ(a) = { [ Length ↦ 1 , Time ↦ -1 ] , [ Energy ↦ 1 , Time ↦ -1 ] }
```

Vector dimensionality expressions like these are the primary data structure
across which static analysis is performed.

Note that this somewhat-looser data structure can be denormal. If, two or more
components of the vector are keyed by distinct units expressions of the same
dimension, then they will all register under a single entry of the vector
dimensionality set.

- If the units expressions of such keys are convertible, they should be
  automatically converted combined into a single component in the expression
  evaluation process that results in the vector to keep vectors normalized.
- If there is no conversion path to automatically combine the key units, this
  denormal vector form will result in a dynamic (runtime) warning or error.

We define the size of a vector as *sizeᵥ(v) = size(dimᵥ(v))*, the size of the
vector dimensionality set.

### Vector Operations

The following math operations are available to vector values.

#### Vector Inversion

The inverse of a vector is defined as the inverse of each key (the degree map
inverse) mapped to the reciprocal of the corresponding quantity.

```
[ U₁ ↦ q₁ , U₂ ↦ q₂ , ⋯ , Uᵢ ↦ qᵢ ]⁻¹ =
  [ U₁⁻¹ ↦ 1/q₁ , U₂⁻¹ ↦ 1/q₂ , ⋯ , Uᵢ⁻¹ ↦ 1/qᵢ ]
```

Correspondingly, in terms of the vector dimensionality of the inverted vector,
the resulting dimensionality is the inverse of each member of the set.

#### Vector Product

For Doggerel, the product of two vectors *v₁* and *v₂* is only defined when
*sizeᵥ(v₁) = 1 ∨ sizeᵥ(v₂) = 1*. When it is defined, we arrange so that the left
hand operand is the vector of size 1.

```
[ U₁ ↦ q₁ ] × [ U₂ ↦ q₂ , U₃ ↦ q₃ , ⋯ , Uᵢ ↦ qᵢ ] =
  [ U₁×U₂ ↦ q₁×q₂ , U₁×U₃ ↦ q₁×q₃ , ⋯ , U₁×Uᵢ ↦ q₁×qᵢ ]
```

Correspondingly, in terms of the vector dimensionality of the product vector,
the result is the single component vector's dimensionality degree map,
multiplied by each element of the alternative vector's dimensionality set
elements.

A static analysis error is emitted when the precondition is unsatisfied and the
product is undefined.

#### Vector Quotient

The vector quotient is defined as the composition of the inverse and the product.

```
v₁ ÷ v₂ = v₁ × v₂⁻¹
```

#### Vector Negation

The unary negation of a vector is defined as the negation of each mapped
quantity. This operation does not alter the resulting vector dimensionality.

```
-[ U₁ ↦ q₁ , U₂ ↦ q₂ , ⋯ , Uᵢ ↦ qᵢ ]⁻¹ = [ U₁ ↦ -q₁ , U₂ ↦ -q₂ , ⋯ , Uᵢ ↦ -qᵢ ]
```

#### Vector Sum

The sum of two vectors is defined as a merge of two vectors where components of
the right-hand vector with dimensionalities that match components of the
left-hand vector are converted to match the units of their corresponding
left-hand components before having their quantities summed. The remaining,
unmatched components are collected in the final vector.

If, during this process, any conversions are not possible, the resulting vector
will be denormal, and may result in a dynamic (runtime) failure.
