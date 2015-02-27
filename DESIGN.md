## Design

This document is intended to describe design goals, invariants and implementation notes for the various types

## SafeLong

## Rational

The design goal of Rational is to provide good performance and low memory overhead in the very frequent case of rational
numbers with small magnitude, while still being able to represent rational numbers of arbitrary size.

To achieve this goal, there are two different cases. `LongRational(n: Long, d: Long)` is used to represent small
rational numbers. `BigRational(n: BigInt, d: BigInt)` is used whenever either numerator or denominator are too large to
be stored in a `LongRational`.

### Invariants

Rationals are always stored in a normalized form so that there is an unique representation for each number:

- Common factors of numerator and denominator are eliminated, so e.g. `3/9` is stored as `n = 1, d = 3`
- Negative rational numbers are always stored as a negative numerator. Therefore the denominator is always larger than
zero
- A rational number with a zero denominator is invalid
- Zero will always be stored as `n = 0, d = 1`

In addition to these invariants that are quite common in many rational implementations, there are additional invariants
related to the usage of LongRational or BigRational:

- Every rational number that can be stored in a LongRational given the invariants above will always be stored in a
LongRational. Consequences of this are:
  - The valid range for the numerator is `[Long.MinValue, Long.MaxValue]` and for the denominator `[1, Long.MaxValue]`.
  - Since the denominator is always stored as a positive number, e.g. `Rational(1, Long.MinValue)` must be stored as a
  BigRational
  - A BigRational can never be equal to a LongRational
  - The numerator of a BigRational can never be zero, since zero is always represented as `LongRational(0, 1)`

When implementing operations for Rationals, it is important to make sure that the invariants are not violated. E.g. when
performing an operation with two `BigRational` that results in a small number that can be represented in a
`LongRational`, the result ***must*** be a `LongRational`.

### Implementation notes