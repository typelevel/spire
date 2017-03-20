---
layout: docs
title:  "Eq"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/Eq.scala"
scaladoc: "#spire.algebra.Eq"
---

## Eq

Spire provides an `Eq[A]` type class to represent type-safe
equality. This allows us to talk about types for which there isn't a
computationally useful notion of equality, and also to avoid
programming errors caused by universal equality.

`Eq[A]` provides two operators

 * `eqv` (`a === b`) equality operator.
 * `neqv` (`a =!= b`) inequality operator (defaults to `!(a === b)`).

Spire requires that `eqv` obey the laws of an equivalence relation, namely:

 * `a === a` (*reflexivity*)
 * if `a === b` then `b === a` (*symmetry*)
 * if `a === b` then `a` is `b` (*anti-symmetry*)
 * if `a === b` and `b === c` then `a === c` (*transitivity*)

The anti-symmetry property may seem confusing. The idea is that if `a === b`
then `a` and `b` must be substitutable for each other, such that for any
expression `f(x)`, `f(a) === f(b)`.
