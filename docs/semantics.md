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

Although we're packaging data in vectors, the desired result of a Doggerel
program will almost always be a single value. While this runs somewhat counter
to math tradition where a vector must be of greater than one dimension, and
would otherwise be a scalar. In this environment, we'll treat vectors more like
a kind of bucket for multiple pieces of data which are distinguished by their
dimensionalities. So in this sense, a scalar would be a piece of data of unknown
dimensionality, and would not fit within the encoding.

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

The operation of raising a degree map to a real exponent is defined as the
original degree map with each mapped degree multiplied by the real. The real
operand need not be whole, but, since degrees are integers, the exponent is
*only defined when result of multiplying each degree is whole*. Consider the
following examples.

```
[A ↦ 3, B ↦ 1,  C ↦ -2]^-2   = [C ↦ 4, B ↦ -2, A ↦ -6]

       [E ↦ 12, F ↦ -4]^0.25 = [E ↦ 3, F ↦ -1]
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
   dimension `[ Length ↦ 1 ]`. However, a unit such as `Acre` will be associated
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

### Natural Units

Some units can be marked as "natural", meaning that they need to be tagged with
a natural number to be used and cannot be associated with a dimension. Their
parametric dimensions are automatically tagged with the same index.

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
dimᵥ([ U₁ ↦ q₁ , U₂ ↦ q₂ , ⋯ , Uᵢ ↦ qᵢ ]) = ⋃ dimᵤ(Uⱼ)
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

Note: how does this vector product relate to the traditional geometric cross
product of vectors? How does it relate to the Cartesian product? In this
formulation, it gets a little ambiguous which product we'll want. The geometric
cross product is only well-defined when a vector is measured in three spatial
dimensions, although general formulations exist -- this may be better-suited as
a function. For now, we avoid this ambiguity by only defining the vector product
when at least one operand is of size 1, which is probably closer to the
Cartesian.

#### Vector Exponent

Vectors can be raised to a real exponent. We define the exponent of a vector
*vᵉ* to be the vector with each units key raised to the degree-map exponent *e*
and each mapped to the arithmetic exponent *e*.

```
[ U₁ ↦ q₁ , U₂ ↦ q₂ , ⋯ , Uᵢ ↦ qᵢ ]ᵉ =
  [ U₁ᵉ ↦ q₁ᵉ , U₂ᵉ ↦ q₂ᵉ , ⋯ , Uᵢᵉ ↦ qᵢᵉ ]
```

This will result in a static analysis error when any component exponent is
undefined because of non-whole resulting unit degrees.

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

## Dimspecs

Dimspecs provide a language for describing vector dimensionalities abstractly.

### Dimspec Variables

To start with, dimspec variables are symbols in a dimspec that start with a
colon. A dimspec of `{ :a }` will match any single-component vector, because
`:a` is an unconstrained dimension variable. Likewise, `{ :a, :b, :c }` will
match any three-component vector for the same reason.

Note: `{ :a, :a }` is an invalid dimspec, because each component of a vector
must have a distinct dimension.

Note: the ordering of components won't matter in a dimspec.

### Concrete Dimensions

For more constrained dimspecs, if `length` and `time` are dimensions, then we
can write a dimspec like `{ length, time, length/time }` to match a three
component vector of length, time and speed.

### Dimspec Factorization

We also mix variables and concrete dimensions, for example, the dimspec
`{ time :a }` will match any single component vector where time is in the
numerator of the component's dimension and there is some other dimension factor.
For example,
* It will match a vector of dimension `{ time·length }`, of dimension
  `{ time·length⁻¹ }`, or dimension `{ time² }`.
* It will not match a vector of dimension `{ time }`, of `{ length·time⁻¹ }`, of
`{ time⁻² }` or of dimension `{ length }`.

Likeweise, we could write a dimspec like `{ :a / time }` to describe some rate
where time must be in the denominator.

### Natural Dimensions in Dimspecs

If a dimension is natural, we can specify specific indices in dimspecs. For
example, a list of four lengths might be written like the following:

```
{ length index(0), length index(1), length index(2), length index(4)}
```

But we can use a more concise format to describe lists that are homogenous like
this one by specifying a range in the dimspec: `{ length index(0..3) }`. This
statement is *equivalent* to the more verbose construct above, but it is
automatically expanded to four terms.

Note: this declares a specific length for the array *and* encodes that the list
uses a zero-offset.

Furthermore, to describe a homogeneous list of *any length*, the range in the
dimspec is written without a terminus. `{ :a index(0..) }`. Note: even if this
expression expands to any number of terms, the dimension variable `:a` must
match a single dimension factor, so the list remains homegeneous.

### Dimspec Cartesian Products

Another approach to abbreviating dimspoec definitions is cartesian products
using parentheses.

* `{ :a (:b, :c / :d) }` expands to `{ :a:b, :a:c/:d }`.
* `{ (:e, :f) (:g, :h, :i) }` expands to
  `{ :e:g, :f:g, :e:h, :f:h, :e:i, :f:i }`
* And finally `{ row(0..3) col(0..3) }` expands to a four by four matrix.
