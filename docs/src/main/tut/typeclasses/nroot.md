---
layout: docs
title:  "NRoot"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/NRoot.scala"
scaladoc: "#spire.algebra.NRoot"
---

## NRoot

Spire supports square roots and fractional powers via
`NRoot[A]`. There are three basic methods available:

 * `sqrt` (`a.sqrt`) finds the square root of `a`
 * `nroot` (`(a nroot k)`) finds the kth root of `a`
 * `fpow` (`(a fpow b)`) takes `a` to the fractional power `b`

While Spire does have types that can represent irrational roots exactly -
[`Algebraic`](../numbers/algebraic.html) and [`Real`](../numbers/real.html) -
most types that support square and n-roots are not exact (eg `Double` or
`BigDecimal`). This means that many laws we might like to write about roots
will be weaker than we would like:

 * `a.sqrt` = `(a nroot 2)` = `(a fpow 2.reciprocal)`
 * if `A` can represent `1/k` exactly, then `(a nroot k)` = `(a fpow k.reciprocal)`
 * if `(a nroot k)` is rational, then `(a nroot k).pow(k)` = `a`

Approximate types like `Double` and `BigDecimal` have a built-in precision to
which Spire can find roots. The roots will be the best possible approximations
at the given precision. If you need exact nth-roots of real numbers, or roots
of rational polynomials, you can use Spire's
[`Algebraic`](../numbers/algebraic.html) or [`Real`](../numbers/real.html)
number types. `Rational` does not have an `NRoot` instance, since it is
ill-suited to the task (slow and imprecise).
