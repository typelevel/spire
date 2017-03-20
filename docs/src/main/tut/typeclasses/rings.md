---
layout: docs
title:  "Rings and Fields"
section: "typeclasses"
---

## Rings and Fields

Rings and fields let us abstract over things that behave like numbers. Rings
are a set together with 2 binary operations: addition and multiplication.
Fields add division into the mix. Spire provides additional types to abstract
over algebraic (eg `sqrt`, `nroot`, etc) and transcendental functions (`exp`,
`sin`, `ln`, etc).

 * [`Semiring`](ring.html) multiplication and addition
 * [`Rig`](ring.html) semiring with a multiplicative identity (1)
 * [`Rng`](ring.html) semiring with an additive inverse
 * [`Ring`](ring.html) extends both rig and rng
 * [`CRing`](ring.html) ring with commutative multiplication
 * [`EuclideanRing`](euclideanring.html) a ring with quotient, remainder, gcd, etc
 * [`Field`](field.html) ring with division
 * [`NRoot`](nroot.html) sqrt, nroot, etc
 * [`Trig`](trig.html) exp, ln, sin, cos, pi, etc
