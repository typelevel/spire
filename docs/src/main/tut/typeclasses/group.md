---
layout: docs
title:  "Semigroups, Monoids, and Groups"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/Semigroup.scala"
scaladoc: "#spire.algebra.Semigroup"
---

## Semigroups, Monoids, and Groups

The most basic algebraic type classes Spire supports involve a single
associative binary operator (called `op` and represented as `|+|`):

 * `Semigroup[A]` just the associative operator `|+|`, nothing more.
 * `Monoid[A]` a semigroup that also has an identity element `id`.
 * `Group[A]` a monoid that also has an inverse operation (`inverse` or `|-|`).
 * `CSemigroup[A]` a semigroup that is commutative.
 * `CMonoid[A]` a monoid that is commutative.
 * `AbGroup[A]` an "abelian group", a group that is commutative.

Most types have many possible implementations of these types classes. For
example, `Int` has 2 obvious `Monoid`s: (`+`, 0) and (`*`, 1). We can go even
further though, since (`min`, `Int.MaxValue`), (`max`, `Int.MinValue`),
(`&&`, -1), and (`||`, 0) are all monoids as well! In these cases Spire
requires users to explicitly choose which implementation they want.

Spire also defines two parallel group hierarchies for *additive* and
*multiplicative* groups. These have the same properties but different names
and symbols. The following list provides the generic, additive, and
multiplicative variants:

 * operator method: `op`, `plus`, `times`
 * operator symbol: `|+|`, `+`, `*`
 * identity name: `id`, `zero`, `one`
 * inverse method: `inverse`, `negate`, `reciprocal`
 * inverse symbol: `inverse`, `-` (unary), `reciprocal`
 * inverse binary operator: `|-|`, `-` (binary), `/`
