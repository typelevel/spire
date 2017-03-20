---
layout: docs
title:  "Field"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/Field.scala"
scaladoc: "#spire.algebra.Field"
---

## Field

Fields are commutative rings with commutative multiplication (both
multiplication `*`, and division `/`) and multiplicative inverses for all
non-zero elements. Fields generalize how most people think about rational
numbers.

Spire's `Field[A]` supports the following operations:

 * `div` (`a / b`) divide `a` by `b`.
 * `reciprocal` (`a.reciprocal`) the multiplicative inverse of `a`, i.e. `one/a`.

Even though fields sit at the top of the ring hierarchy, there are
many operations which are not provided by fields:

 * equality and ordering (provided by `Eq[A]` and `Order[A]`).
 * square root, and other roots (provided by `NRoot[A]`).
 * sine, cosine, and trigonometric functions (provided by `Trig[A]`).
