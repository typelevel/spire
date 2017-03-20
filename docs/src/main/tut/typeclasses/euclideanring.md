---
layout: docs
title:  "EuclideanRing"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/EuclideanRing.scala"
scaladoc: "#spire.algebra.EuclideanRing"
---

## EuclideanRing

Spire supports euclidean domains (called `EuclideanRing[A]`). A
euclidean domain is a commutative ring (`CRing[A]`) that also supports
euclidean division (e.g. floor division or integer division). This
structure generalizes many useful properties of the integers (for
instance, quotients and remainders, and greatest common divisors).

Formally, euclidean domains have a *euclidean function* f such that
for any `x` and `y` in `A`, if `y` is nonzero, then there are `q` and
`r` (quotient and remainder) such that `a = b*q + r` and `r = 0` or
`f(r) < f(b)`. For integers, `f` is usually the absolute value
function.

Spire's `EuclideanRing[A]` supports the following operations:

 * `quot` (`a /~ b`) finding the quotient (often integer division).
 * `mod` (`a % b`) the remainder from the quotient operation.
 * `quotmod` (`a /% b`) combines `quot` and `mod` into one operation.
 * `gcd` (`a gcd b`) find the greatest common divisor of `a` and `b`.
 * `lcm` (`a lcm b`) find the lowest common multiple of `a` and `b`.

Spire requires that `b * (a /~ b) + (a % b)` is equivalent to `a`.
