---
layout: docs
title:  "Numbers"
section: "numbers"
position: 2
---

## Numbers

In addition to supporting all of Scala's built-in number types, Spire
introduces several new ones, all of which can be found in `spire.math`.

### Exact Real Numbers

 * [`Natural`](numbers/natural.html) unsigned, immutable, arbitrary precision integer
 * [`SafeLong`](numbers/safelong.html) fast, overflow-proof integer type
 * [`Rational`](numbers/rational.html) fractions of integers with perfect precision
 * [`Algebraic`](numbers/algebraic.html) lazily-computed, arbitrary precision algebraic numbers
 * [`Real`](numbers/real.html) computable real number implementation

### Unsigned Fixed-width Integers

 * `UByte` unsigned 8-bit integers
 * `UShort` unsigned 16-bit integers
 * `UInt` unsigned 32-bit integers
 * `ULong` unsigned 64-bit integers

### Other Number Types

 * [`Number`](numbers/number.html) boxed type supporting a traditional numeric tower
 * [`Complex[A]`](numbers/complex.html) complex numbers, points on the complex plane
 * [`Quaternion[A]`](numbers/quaternion.html) extension of complex numbers into 4D space
 * [`Interval[A]`](numbers/interval.html) arithmetic on open, closed, and unbound intervals
 * [`Polynomial[A]`](numbers/polynomial.html) univariate (single-variable) polynomial expressions
 * [`Trilean`](numbers/trilean.html) value class supporting three-valued logic
 * [`FixedPoint`](numbers/fixedpoint.html) fractions with Long numerator and implicit denominator (in extras)
 * [`Jet[A]`](numbers/jet.html) N-dimensional dual numbers, for automatic differentiation

## Which number types should I use?

Spire provides many number types, and it is not always obvious what their
relative merits are. This section explains the distinctions between them, and
may help you decide which numeric representation(s) to use.

There is usually a tension between numbers that have correctness caveats (like
possible overflow or precision issues) and numbers that have performance
caveats (like extra allocations and/or slower code). Spire provides a wide
range of numeric types that should address most needs.

### Natural numbers (unsigned, whole-value numbers)

For non-negative numbers, the safe type to use is `Natural`. It is quite fast
when representing small-ish numbers (128-bits or less), but has no upper bound
on the values it can represent. However, its unique cons-structure means that
for very large values `BigInt` and `SafeLong` may be faster. Since it only
supports non-negative values, subtraction is non-total (and may throw an
exception).

If your values are guaranteed to be small (or you are prepared to detect
truncation), you can use `UByte` (8-bit), `UShort` (16-bit), `UInt` (32-bit),
or `ULong` (64-bit), depending on how much space you need. These types have
the same unsigned semantics as unsigned types in languages like C. These types
are not boxed, although care must be used with arrays (like any value class).

### Integer numbers (signed, whole-value numbers)

There are two safe types that can be used with integer values: `SafeLong` and
`BigInt`. Both support arbitrarily large values, as well as the usual
semantics for things like integer division (`quot`). The former (`SafeLong`)
performs much better for values that can be represented with a `Long` (e.g.
64-bit or less), and is a good default choice. When dealing with values that
are mostly or entirely very large, `BigInt` may be a bit faster.

Like the unsigned case, you can use `Byte` (8-bit), `Short` (16-bit), `Int`
(32-bit), or `Long` (64-bit) to handle cases where your values are small, or
where you want to avoid allocations and will handle truncation issues
yourself. These types are provided by Scala (and ultimately the JVM) and will
not cause object allocations.

### Fractional numbers (numbers that can be divided)

There are many different fractional flavors, which support various trade-offs
between expressive power, precision, and performance.

Fractional types come in two basic flavors: precise or imprecise. Imprecise
types (like `Double`) will accumulate error and are not associative in some
cases (meaning that `(x + y) + z` may produce different results than `x + (y +
z)`). These types are often faster than precise types but can be risky to use.

Precise numbers make stronger precision guarantees, but at the cost of
performance or expressiveness. They are often a bit slower, and may restrict
the operations they support (to preserve guarantees about precision).

#### Precise types

The most powerful precise type is `Real`. It represents computable real
numbers, and supports all the operations you would expect, including roots and
trigonometry. However, irrational values (like `Real(2).sqrt` or `Real.pi`)
are represented via functions from precision to approximations. This means
that in some situations this type might be too slow, or use too much memory.
Additionally, operations requiring comparison or equality tests can only be
approximately computed. However, this type should never accumulate error, so
your results will always be correctly approximated to whatever precision you
need.

The next precise type is `Algebraic`. This type supports all rational values
as well as roots. However, it cannot represent transcendental values like "pi",
making its values a subset of `Real`'s. Unlike `Real`, this type is able to do
exact sign tests (and thus, equality tests and comparisons). Due to the ASTs
`Algebraic` uses to represent expressions, execution may be slow and involve
lots of allocations.

Finally there is `Rational`. This type represents values as irreducible
fractions (e.g. `n/d`). `Rational` cannot represent irrational values (such as
roots), but efficiently implements all operations on rational values. This
type has the fewest performance "gotchas", although obviously fractions with
large numerators or denominators will take longer to operate on.

#### Imprecise types

These types are more efficient than the precise types, but require care and
analysis to ensure that results are correct and sufficiently accurate.

The imprecise type with the most potential precision is `BigDecimal` which is
provided by Scala. This number approximates real values to a number of decimal
(base-10) digits (by default 34). Unlike floating point values, this type has
an exact representation of values like `0.111110`, and the user can use
`java.math.MathContext` to configure how much precision is used. Even so, the
type is still subject to accumulated rounding error, and thus is not truly
associative.

Next come `Float` and `Double`, the built-in 32- and 64-bit floating-point
implementations on the JVM. The pitfalls of using floating-point values are
well-known (and documented elsewhere) but these types are very fast.

Finally, Spire supports the experimental `FixedPoint` class. This value class
uses unboxed `Long` values to represent fractions in terms of user-specified
denominator (supplied via an implicit `FixedScale` instance). This is a very
special-purpose type to be used in cases where floating-point approximations
have problems and unboxed values are required. You should avoid this type
unless your applications has a known, specific need for fixed-point
arithmetic.

### Other types

The other numeric and pseudo-numeric types (like `Polynomial`, `Interval`,
`Complex`, and `Quaternion`) each implement specific functionality, so there
should be less confusion about which type to use. The sections describing
these types explain their properties and trade-offs.
