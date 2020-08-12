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

## Base Dimensions and Compound Dimensions

In Doggerel, **base dimensions** are static symbols declared and unique within a
scope. Example real-world dimension symbols might be `Length`, `Time` or
`Temperature`.

## Generic Degree Maps

In practice, we'll rarely deal with base dimensions individually, but will
rather deal with **compound dimensions** that are built out of base dimensions
combined together in a generic data structure called a **degree map**.

A degree map structure for a set of simbols (e.g. base dimension symbols, but
not necessarily) is a mapping from instances of those symbols to integer
**degrees**.

The trivial degree map maps a single symbol to a degree of 1. For example, we
write a trivial degree map for the base dimension of `Time` like the following.

```
[ Time ↦ 1 ]
```

Contrast this trivial example with a dimension of `Speed`. In this case, it's
written in the degree map to describe `Length` per `Time` using a negative
degree for the latter.

```
[ Length ↦ 1, Time ↦ -1 ]
```

Likewise, we might describe `Area` like `[ Length ↦ 2 ]`, and volume like
`[ Length ↦ 3 ]` or even `Temperature` per `Volume` like
`[ Temperature ↦ 1 , Volume ↦ -3 ]`.

If any symbol is not explicitly mapped in a degree map, we say it is of degree
zero. Thus, if some degree map operation results in a degree of zero,
then that mapping can be dropped. Consider, for example, the compound dimension
of `Length` per `Length` is an empty mapping.

### Degree Map Operations

Finally, degree maps are closed under a pair of operations: **product** and
**exponent**.

The product of two degree maps is a merge of each mapping where the degrees of
matching symbols summed together. Note that, in this example, the degrees of
symbol `C` sum to zero, so it does not appear in the product.

```
[ A ↦ 3 , B ↦ 1, C ↦ -2 ] ×
  [ B ↦ 2 , C ↦ -2, D ↦ -3 ] =
    [ A ↦ 3 , B ↦ 3 , D ↦ -3 ]
```

The operation of raising a degree map to a real exponsnt is defined as the
original degree map with each mapped degree multiplied by the real. The real
operand need not be whole, but, since degrees are integers, the exponent is
*only defined when result of multiplying each degree is whole*. Consider the
following examples.

```
[A ↦ 3, B ↦ 1,  C ↦ -2]^-2    = [C ↦ 4, B ↦ -2, A ↦ -6]

       [E ↦ 12, F ↦ -4]^-0.25 = [E ↦ 3, F ↦ -1]
```

Under this operation, the **inverse** of a degree map corresponds to the
conventional exponent of -1. This allows us to synthesize a binary **divide**
operation across degree maps as a composition of the product and inverse.

```
m ÷ n = m × n⁻¹
```

## Base Units and Compound Units

Like base dimensions, **base units** are static symbols uniquely declared within
a scope, however, when they are defined, they are associated with dimensions.

Base unit declarations can take two forms:

1. A base unit symbol, associated with a compound dimension. Typically, this is
   a trivial dimension, e.g. the unit `Mile` would be associated with the
   dimension `[ Length ↦ 1 ]`. However, a unit such as `Acre` will be assoctated
   with the compound dimension of `[ Length ↦ 2 ]` or the Unit of `Watt` with
   the compound dimension of `[ Energy ↦ 1, Time ↦ -1 ]`.
2. A base unit symbol, associated with the trivial degree map of itself as a
   dimension. We call units of this form **parametric units** or **parametric
   dimensions** (depending on the context).

As with dimensions, we'll typically interact with **compound units**, which are
degree maps, just like compound dimensions, but mapping base unit symbols rather
than base dimensions. Consider the units of `Miles` per `Hour`.

```
[ Mile ↦ 1, Hour ↦ -1 ]
```

### Dimensionality

Because base units are associated with compound dimensions, we can associate any
compound units with a **compound dimensionality**. Notationally, we define *D =
dim(u)* function to access the associated compound dimension *D* corresponding
to the given base unit *u*.

We can use this to define *D = dimᵤ(U)* which gives the compound dimension for
the given compound unit.

```
                                            ᵢ
dimᵤ([ U₁ ↦ d₁ , U₂ ↦ d₂ , ⋯ , Uᵢ ↦ dᵢ ]) = ∏ dim(Uⱼ)^dⱼ
                                           ʲ⁼¹
```

## Convertible Units

Because Doggerel records the units that quantities are measured in, when
equipped with additional information of how units of the same dimensionality
relate to each other, in some contexts, it's possible for quantities to be
**converted** automatically. We'll describe these contexts in greater detail
below, but for now we describe the encoding for the relations.

Any two units of equal dimensionality may be related to each other by a
conversion which is declared within a scope. For any two base units of equal
dimensionality *p* and *q*, a conversion is a transformation that can be applied
to a scalar quantity was measured in *p* to achieve a quantity measured in *q*.

For example, we might consider a set of common units in the dimension of
temperature: `Celsius`, `Fahrenheit` and `Kelvin`. The familiar conversions
would be given by the triples of *(from unit, to unit, transformation)*. Here we
describe the transformations as lambda expressions and assign them names for
convenience.

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
conversions. We could imagine additionally defining a fourth unit of temperature
(e.g. Rankine) but not define an additional conversion, then the result will be
a disconnected subgraph and, in this table representation, there would be empty
cells.

In conclusion, some properties of an ideal convertibility graph of a dimension's
units.

- Has no isolated subgraphs in order to maximize usable units.
- Uses minimal transitive conversions in order to minimize roundoff errors.

Either of these can result in a static analysis warning.

#### Formalization

We'll say that, under defined sets of dimensions, units and conversion, then, as
long as compound units u₁ and u₂ are of the same dimensionality, and where
conversions exist between their differences in base units, then, the following
notation describes the conversion of a scalar quantity *q₁*, measured in
base-unit *u₁* to a scalar quantity *q₂* measured in base-unit *u₂*.

```
q₂ = convert(u₁, u₂, q₁)
```

We extend this with a best-effort conversion across compound units. As before,
we're given a scalar quantity of *q₁* which is now measured in the compound
units of *U₁*, and we're given the target compound units of *U₂*. This gives a
resulting scalar *q₂* measured in compound units *U₃*.

```
(q₂, U₃) = best-convert(U₁, U₂, q₁)
```

Mechanically, this is achieved my matching base unit entries of *U₁* with those
of *U₂* which have the same dimension and successively applying those
transformations to *q₁*. If each of those matches have a compatible conversion
available, this result will have *U₃=U₂*, otherwise *U₃* will be closer or at
least not farther apart from *U₂* than *U₁*.

## Vectors

### Vector Structure

We encode vectors as a mapping from units to scalar quantities. For example, a
vector described by *{ 88 miles per hour, 1.21 gigawatts }* could be represented
with the following nested map.

```
a = [ [ Mile ↦ 1 , Hour ↦ -1 ] ↦ 88 , [ Gigawatt ↦ 1 ] ↦ 1.21 ]
```

Likewise, any vector will have a property of **vector dimensionality** which is
a set of compound dimensionalities of each key.

```
                                            ᵢ
dimᵥ([ U₁ ↦ d₁ , U₂ ↦ d₂ , ⋯ , Uᵢ ↦ dᵢ ]) = ⋃ dimᵤ(Uⱼ)
                                           ʲ⁼¹
```

For example:

```
dimᵥ(a) = { [ Length ↦ 1 , Time ↦ -1 ] , [ Energy ↦ 1 , Time ↦ -1 ] }
```

Vector dimensionality sets like these are the primary data structure across
which static analysis is performed.

Note that, compared with other data structures, a vector is somewhat-looser and
can be denormal. If, two or more components of the vector are keyed by distinct
units expressions of the same dimensionality, then they will all register under a
single entry of the vector dimensionality set.

- If the units expressions of such keys are convertible, they should be
  automatically converted combined into a single component in the expression
  evaluation process that results in the vector to keep vectors normalized.
- If there is no conversion path to automatically combine the key units, this
  denormal vector form will result in a warning or error.

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

The units of the RHS vector are converted the the LHS vector component for
optimal cancellation.

```
[ U₁ ↦ q₁ ] × [ U₂ ↦ q₂ , U₃ ↦ q₃ , ⋯ , Uᵢ ↦ qᵢ ] =
  [ U₁×U₂′ ↦ q₁×q₂′ , U₁×U₃′ ↦ q₁×q₃′ , ⋯ , U₁×Uᵢ′ ↦ q₁×qᵢ′ ]

where (Uⱼ′, qⱼ′) = best-convert(Uⱼ′, U₁, qⱼ)
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
-[ U₁ ↦ q₁ , U₂ ↦ q₂ , ⋯ , Uᵢ ↦ qᵢ ] = [ U₁ ↦ -q₁ , U₂ ↦ -q₂ , ⋯ , Uᵢ ↦ -qᵢ ]
```

#### Vector Sum

The sum of two vectors is defined as a merge of two vectors where components of
the right-hand vector with dimensionalities that match components of the
left-hand vector are converted to match the units of their corresponding
left-hand components before having their quantities summed. The remaining,
unmatched components are collected in the final vector.

If, during this process, any conversions are not possible, the resulting vector
will be denormal, and may result in a dynamic (runtime) failure.

In terms of the vector dimensionality of the sum, the result will be the union
of the two operand's dimensionality sets.

```
dimᵥ(v₁ + v₂) = dimᵥ(v₁) ∪ dimᵥ(v₂)
```

#### Vector Difference

The vector difference is defined as the composition of negation and the sum.

```
v₁ - v₂ = v₁ + (-v₂)
```

#### Vector Exponent

Vectors can be raised to a real exponent. We define the exponent of a vector
*vᵉ* to be the following, only when each unit dot product is defined.

```
[ U₁ ↦ q₁ , U₂ ↦ q₂ , ⋯ , Uᵢ ↦ qᵢ ]ᵉ =
  [ U₁·e ↦ q₁ᵉ , U₂·e ↦ q₂ᵉ , ⋯ , Uᵢ·e ↦ qᵢᵉ ]
```

This will result in a static analysis error when any unit dot product is
undefined.

#### Function Application

Vectors can be passed into functions to produce new vectors. At the semantic
level, a function can be polymorphic in the sense that the operation specified
by its application depends on the dimensionality of the input vector.

A polymorphic function is a mapping from sets of units expressions to a pair of
resulting units and the operation across a vector in the key units. The result
is a value in those resulting units.

```
[ {U₁ , U₂ , U₃ } ↦ (Uᵣ₁, λ₁) ,
  {U₄ , U₅ , U₆ } ↦ (Uᵣ₂, λ₂) ,
   ⋮    ⋮             ⋮    ⋮
  {U₇ , U₈      } ↦ (Uᵣᵢ, λᵢ) ]
```

A function application is not defined, when the vector dimensionality of the
argument has no key in the mapping with corresponding dimensionality.
