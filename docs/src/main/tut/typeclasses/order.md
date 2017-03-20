---
layout: docs
title:  "Order"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/Order.scala"
scaladoc: "#spire.algebra.Order"
---

## Order

Total orderings in Spire are supported by the `Order[A]` type class. Unlike
other ordering type classes (e.g. `scala.math.Ordering`), this one is
specialized to avoid boxing.  `Order[A]` extends `Eq[A]` can be implemented
via a single `compare` method, although it provides all of the following:

 * `eqv` (`a === b`)
 * `neqv` (`a =!= b`)
 * `lt` (`a < b`)
 * `gt` (`a > b`)
 * `lteqv` (`a <= b`)
 * `gteqv` (`a >= b`)
 * `compare` (`a compare b`)
 * `min` (`a min b`)
 * `max` (`a max b`)

Instances of `Order[A]` are required to observe the following
properties:

 * if `a <= b` and `b <= a` then `a === b` (*anti-symmetry*)
 * if `a <= b` and `b <= c` then `a <= c` (*transitivity*)
 * either `a <= b` or `b <= a` (*totality*)

Additionally, total orderings across fields should obey the following
additional laws:

 * if `a <= b` then `(a + c) <= (b + c)` (*O1*)
 * if `zero <= a` and `zero <= b` then `zero <= (a * b)` (*O2*)
 
(These are laws are required by ordered fields.)

In some cases users may need to use (or define) total orderings that
do not follow all these laws, or may break laws required by other
structures. An example would be the lexicographic ordering of complex
numbers, which breaks *O2*.  In these cases, users will need to be
aware of the risks and limit their use to situations where the
particular law is not needed.
