---
layout: docs
title:  "Numbers"
section: "numbers"
position: 2
---

## Numbers

In addition to supporting all of Scala's built-in number types, Spire
introduces several new ones, all of which can be found in `spire.math`:

 * [`SafeLong`](numbers/safelong.html) fast, overflow-proof integer type
 * [`Rational`](numbers/rational.html) fractions of integers with perfect precision
 * [`Algebraic`](numbers/algebraic.html) lazily-computed, arbitrary precision algebraic numbers
 * [`Real`](numbers/real.html) computable real number implementation
 * [Unsigned value classes](numbers/unsigned.html) supporting unsigned operations on fixed-width integers
   * `UByte` unsigned 8-bit integers
   * `UShort` unsigned 16-bit integers
   * `UInt` unsigned 32-bit integers
   * `ULong` unsigned 64-bit integers
 * [`Natural`](numbers/natural.html) unsigned, immutable, arbitrary precision integer
 * [`Number`](numbers/number.html) boxed type supporting a traditional numeric tower
 * [`Complex[A]`](numbers/complex.md) complex numbers, points on the complex plane
 * `Quaternion[A]` extension of complex numbers into 4D space
 * `Jet[A]` N-dimensional dual numbers, for automatic differentiation
 * `Interval[A]` arithmetic on open, closed, and unbound intervals
 * `Polynomial[A]` univariate (single-variable) polynomial expressions
 * `Trilean` value class supporting three-valued logic
 * `FixedPoint` fractions with Long numerator and implicit denominator (in extras)

Detailed treatment of these types can be found in the guide.
