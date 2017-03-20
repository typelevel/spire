---
layout: docs
title:  "Polynomial"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Polynomial.scala"
scaladoc: "#spire.math.Polynomial"
---

## Polynomial[A]

Currently Spire supports univariate polynomials. These are polynomials
with a single variable (e.g. *x*) with the following structure:

```
c0 + (c1 * x^1) + (c2 * x^2) + ... + (cn * x^n)
```

The coefficients (`c0` through `cn`) are values of the type `C`, and
the exponents (`1` through `n`) are `Int` values (this does mean that
Spire's implementation only supports polynomials whose exponents are
less than 2147483648).

Like interval, arithmetic on polynomials is accomplished using type
classes for `C`, such as `Semiring[C]`. With the right type classes,
polynomials can support all the arithmetic operations covered by
euclidean rings, but not fields. Division and reciprocal operations
are impossible because polynomials do not support fractional or
negative exponents. Polynomials also support `interval`, `derivative`,
and other operations.

Spire does support a convenient syntax for literal polynomials. By
importing `spire.syntax.literals._` (or just `spire.implicits._`) you
can use the `poly` string interpolator to create
`Polynomial[Rational]` instances:

```tut:book
import spire.syntax.literals._
poly"3x^2 - 5x + 1"
poly"5/4x^6 - 7x - 2"
poly"1.2x^3 - 6.1x^2 + 9x - 3.33"
```

Spire actually supports two types of polynomials: dense and
sparse. For most simple polynomials used in these examples, you'll
probably want dense polynomials. However, in cases where your
polynomials have a few terms with very large exponents the sparse
implementation will be more efficient. In any case, the underlying
representation is an implementation detail and both types support the
same operations (and can interoperate).
