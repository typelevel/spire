---
layout: docs
title:  "Exact Real Numbers"
section: "typeclasses"
---

## Equality and Ordering

Spire provides type classes for working with equality and ordering of types:

 * [Eq[A]](typeclasses/eq.html),
 * [PartialOrder[A]](typeclasses/partialorder.html),
 * [Order[A]](typeclasses/order.html).

These type classes can be used to define functions that operate over generic
types. For example, `Eq` is used extensively in Spire's law checking, and
`Order` is used in Spire's utility functions for sorting and searching Scala
collections:

```tut:book
import spire.std.int._
import spire.syntax.std.seq._ // gives us .qsorted, .qselectk

val rng = new scala.util.Random(123)
val xs: Vector[Int] = Vector.fill(10)(rng.nextInt)

// Return a sorted version of xs in O(n log n).
xs.qsorted

// Return the bottom 5 values in O(n)
xs.qselectk(5)

// Return and sort the bottom k values in O(n + k log k)
xs.qtopk(5)
```

Note: While Scala provides a built-in `Ordering[A]` type class, Spire's
`Order[A]` is specialized on all primitive types and also interops with
Spire's `Eq[A]` type.
