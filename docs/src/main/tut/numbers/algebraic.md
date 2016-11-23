---
layout: docs
title:  "Algebraic"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Algebraic.scala"
scaladoc: "#spire.math.Algebraic"
---

## Algebraic

This type can be used to represent real, algebraic numbers exactly.  This
includes fractional numbers, like [`Rational`](rational.html), but also
square roots, n-roots, and real roots of rational polynomials. Algebraic
numbers have exact sign functions, can be approximated with either an
absolute or relative error, and can be compared exactly, even if the numbers
are irrational.

Algebraic numbers are useful to provide robust and exact implementations of
many predicates and sign tests found in, for example, computational geometry.
They provide an easy way to guarantee robustness, without having to optimize
floating point arithmetic.

Because Algebraic can represent arbitrary real algebraic numbers, they have a
wider range than Rational. However, whereas Rational represents numbers
exactly, in their normalized form, Algebraic numbers are represented as an AST
of operations. To compute the sign of an Algebraic number, or to compare 2
different Algebraic numbers, they must often be approximated as `BigDecimal`s
first. While the precision required to compute the sign or compare 2 numbers
exactly is bounded by the complexity of the expression (any expression's sign
can be computed in finite time), it can still often be fairly high. Algebraic
adds square roots, n-roots, roots of polynomials to `Rational`s, but it
sacrifices performance for this goal. If you don't need these extra operations,
you should consider using [`Rational`](rational.html). If you don't need exact
comparisons or sign operations, you should consider using an approximate number
type like `Double` or `BigDecimal`.
