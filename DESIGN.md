## Design

This document is intended to describe design goals, invariants and implementation notes for the various types

## SafeLong

The design goal of SafeLong is to provide worry-free arbitrary precision integers that nevertheless have performance approaching boxed Longs for calculations that remain within the range of a 64bit signed integer.

This is achieved by having two cases: `SafeLongLong(x: Long)` is used for numbers within the long range. `SafeLongBigInt(x: BigInt)` is used if this is not the case.

### Invariants

- A number in the long range must *always* be represented as a `SafeLongLong`. This is used both within the SafeLong operations and from other classes in spire.math. Consequences of this invariant are
  - `SafeLongBigInt` can never be zero
  - A `SafeLongBigInt` can never be equal to a `SafeLongLong`

### Implementation notes

Operations of `SafeLongBigInt` use the underlying BigInt and create the appropriate kind of result depending on whether the result is a valid long.

Operations on `SafeLongLong` use the Checked macro to perform operations using long integers, and only fall back to using `BigInt` in case of numeric overflow.

### Performance tips

- comparison to zero is more efficient using signum than using compare
- the `isValidLong` method can be used to check if a `SafeLong` can be losslessly converted to a long using `toLong`

## Rational

The design goal of Rational is to provide good performance and low memory overhead in the very frequent case of rational numbers with small magnitude, while still being able to represent rational numbers of arbitrary size.

To achieve this goal, there are two different cases. `LongRational(n: Long, d: Long)` is used to represent small
rational numbers. `BigRational(n: BigInt, d: BigInt)` is used whenever either numerator or denominator are too large to be stored in a `LongRational`.

### Invariants

Rationals are always stored in a normalized form so that there is an unique representation for each number:

- Common factors of numerator and denominator are eliminated, so e.g. `3/9` is stored as `n = 1, d = 3`
- Negative rational numbers are always stored as a negative numerator. Therefore the denominator is always positive
- A rational number with a zero denominator is invalid
- Zero will always be stored as `n = 0, d = 1`

In addition to these invariants that are quite common in many rational implementations, there are additional invariants
related to the usage of LongRational or BigRational:

- Every rational number that can be stored in a LongRational given the invariants above will *always* be stored in a
LongRational. Consequences of this are:
  - The valid range for the numerator is `[Long.MinValue, Long.MaxValue]` and for the denominator `[1, Long.MaxValue]`.
  - Since the denominator is always stored as a positive number, e.g. `Rational(1, Long.MinValue)` must be stored as a
  BigRational
  - A BigRational can never be equal to a LongRational
  - The numerator of a BigRational can never be zero, since zero is always represented as `LongRational(0, 1)`

When implementing operations for Rationals, it is important to make sure that the invariants are not violated. E.g. when performing an operation with two `BigRational` that results in a small number that can be represented in a
`LongRational`, the result ***must*** be a `LongRational`.

### Implementation notes

The implementation of BigRational is pretty straightforward. Usually, operations will be performed using `SafeLong`, and the result will be created using a builder method that takes a `SafeLong` for both numerator and denominator and produces the right kind of rational number given the invariants.

LongRational operations are a bit more complex: Basic operations use the Checked macro to try to perform the operation with just long integers, and only fall back to using `SafeLong` when there is a numeric overflow. The advantage of this approach is that there are no unnecessary object allocations in the very common case that there is no overflow.

### Performance tips

The performance characteristics of rational operations are a bit different to normal integer or floating point operations. 

- the basic operations +,-,*,/ have roughly the same cost
- comparison is cheaper than the basic operations, and will not allocate any objects for small rational numbers
- comparison to zero is more efficient using signum than using compare
- comparison to one is best done with the compareToOne method instead of using compare
