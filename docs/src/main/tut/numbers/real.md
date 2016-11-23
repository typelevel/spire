---
layout: docs
title:  "Real"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Real.scala"
scaladoc: "#spire.math.Real"
---

## Real

`Real` stands for "computable real". Spire's `Real` implementation is
based on ERA, written in Haskell by David Lester. Computable real
numbers are those which can be computed (i.e. approximated) to any
desired precision. Unlike `Double` and `BigDecimal`, `Real` values are
not stored as approximations, but rather as a function from a desired
precision to the closest approximate value.

If we have an instance `x` of `Real` which approximates a real number
*r*, this means that for any precision *p* (in bits), our instance
will produce an *x* such that *x/2^p* is the closest rational value to
*r*. Translated into Scala, this means that `x.apply(p)` returns a
`SafeLong` value `x`, such that `Rational(x, SafeLong(2).pow(p))` is a
best approximation for *r*.

Spire represents two types of `Real` values: `Exact` and
`Inexact`. The former are rational values for which we have an
existing instance of `Rational`, and are inexpensive to work with. The
latter are functions for approximating (potentially) irrational
values, are lazily evaluated and memoized, and can potentially be very
expensive to compute.

As with `Rational` values, operations on `Real` values are able to
obey the relevant algebraic identities. But unlike `Rational`, `Real`
supports roots and trigonometric functions. Furthermore, important
trig identities are also preserved:

```tut:book
import spire.math.Real
import Real.{sin, cos}

def circle(a: Real): Real = (cos(a).pow(2) + sin(a).pow(2)).sqrt
```

Regardless of the argument we pass to `circle`, it'll return 1:

```tut:book
circle(Real(2))
circle(Real(-3.2))
```

One consequence of the design of computable real numbers is non-continuous
operations (such as sign tests, comparisons, and equality) cannot be
performed exactly. If `x.apply(p)` returns `0`, there is no way to know
whether the value is actually zero, or just a very small value (either
positive or negative!) which is approximately zero at this precision.
Similarly, it's not possible to say that `x` is equal to `y`, but only that
they are equivalent (or not) at a given precision. If you need these
guarantees and are able to give up trig functions, consider using
[`Algebraic`](algebraic.html) or [`Rational`](rational.html) instead.

Spire currently bakes in a "default" precision to use with these kinds
of methods. Furthermore, these methods will always work with `Exact`
values: the issues only arise when using `Inexact` values. Given that
the alternative to using `Real` is to use another approximate type,
providing approximate comparisons and equality seems like a reasonable
compromise.
