---
layout: docs
title:  "Rings and Fields"
section: "typeclasses"
---

## Rings and Fields

Rings and fields let us abstract over things that behave like numbers. Rings
are a set together with 2 binary operations: addition and multiplication.
Fields add division into the mix. Spire provides additional types for to
abstract over algebraic (eg `sqrt`, `nroot`, etc) and transcendental functions
(`exp`, `sin`, `ln`, etc).

 * [`Semiring`](typeclass/ring.html) multiplication and addition
 * [`Rig`](typeclass/ring.html) semiring with a multiplicative identity (1)
 * [`Rng`](typeclass/ring.html) semiring with an additive inverse
 * [`Ring`](typeclasses/ring.html) extends both rig and rng
 * [`CRing`](typeclasses/ring.html) ring with commutative multiplication
 * [`EuclideanRing`](typeclasses/euclideanring.html) a ring with quotient, remainder, gcd, etc
 * [`Field`](typeclasses/field.html) ring with division
 * [`NRoot`](typeclasses/nroot.html) sqrt, nroot, etc
 * [`Trig`](typeclasses/trig.html) exp, ln, sin, cos, pi, etc
