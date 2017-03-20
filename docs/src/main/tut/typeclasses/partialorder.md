---
layout: docs
title:  "PartialOrder"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/PartialOrder.scala"
scaladoc: "#spire.algebra.PartialOrder"
---

## PartialOrder

Partial orderings in Spire are supported by the `PartialOrder[A]` type class.
Its implementation differs from `scala.math.PartialOrdering` in two features:
`PartialOrder` is specialized to avoid boxing, and the `partialCompare`
method returns a `Double` and avoids allocation of an `Option[Int]` instance.
`PartialOrder[A]` extends `Eq[A]`, and can be implemented via a single
`partialCompare` method, described below. `PartialOrder` provides:

 * `eqv` (`a === b`)
 * `neqv` (`a =!= b`)
 * `lt` (`a < b`)
 * `gt` (`a > b`)
 * `lteqv` (`a <= b`)
 * `gteqv` (`a >= b`)
 * `partialCompare` (`a partialCompare b`)
 * `tryCompare` (`a tryCompare b`)
 * `pmin` (`a pmin b`) -- returns the least element if they can be compared
 * `pmax` (`a pmax b`) -- returns the greatest element if they can be compared

A partial order is defined from a binary relation `<=`, which satisfies the
relations:

* `a <= a` (*reflexitivity*)
* if `a <= b` and `b <= a`, then `a === c` (*anti-symmetry*)
* if `a <= b` and `b <= c`, then `a <= c` (*transitivity*)

To compute both `<=` and `>=` at the same time, the method `partialCompare`
uses a `Double` number to encode the result of both comparisons. The truth
table is defined as follows:

| `a <= b` |`a >= b` | `partialCompare(a, b)` |   corresponds to          |
|:--------:|:-------:|:----------------------:|:-------------------------:|
| `true`   | `true`  | `0`                    | `a === b`                 |
| `false`  | `false` | `NaN`                  | `a` incomparable with `b` |
| `true`   | `false` | `-1`                   | `a < b`                   |
| `false`  | `true`  | `1`                    | `a > b`                   |

The method `tryCompare` returns maps `-1.0`, `0.0`, `1.0` to `Some(-1)`,
`Some(0)`, `Some(1)`, and `NaN` to `None`, allowing the use of `getOrElse`
and higher-order methods, at the price of an `Option[Int]` allocation.

Instances of `PartialOrder[A]` are required to observe the properties above.
