---
layout: docs
title:  "Ring"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/Ring.scala"
scaladoc: "#spire.algebra.Ring"
---

## Ring

Rings are a set together with two binary operations (addition and
multiplication). Spire defines these by extending the appropriate additive and
multiplicative group traits (eg `AdditiveCommutativeMonoid`). The following
list roughly describes the Ring-like type classes Spire provides:

 * `Semiring[A]` provides `+`, `zero`, and `*`.
 * `Rig[A]` provides `+`, `zero`, `*`, and `one`.
 * `Rng[A]` provides commutative `+`, `zero`, `-`, and `*`.
 * `Ring[A]` provides commutative `+`, `zero`, `-`, `*`, and `one`.
 * `CRing[A]` provides commutative `+`, `zero`, `-`, commutative `*`, and `one`.

The following list makes clear how these type classes are defined via
inheritance:

 * `Semiring[A]` extends `AdditiveMonoid[A]` with `MultiplicativeSemigroup[A]`
 * `Rig[A]` extends `Semiring[A]` with `MultiplicativeMonoid[A]`
 * `Rng[A]` extends `Semiring[A]` with `AdditiveAbGroup[A]`
 * `Ring[A]` extends `Rig[A]` with `Rng[A]`
 * `CRing[A]` extends `Ring[A]` with `MultiplicativeCMonoid[A]`

Rings also provide a `pow` method (`**`) for doing repeated multiplication and
a `fromInt` method for representing an `Int` in the set.
