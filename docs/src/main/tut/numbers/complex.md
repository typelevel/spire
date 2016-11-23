---
layout: docs
title:  "Complex"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Complex.scala"
scaladoc: "#spire.math.Complex"
---

## Complex[A]

This generic type represents complex numbers (`x + yi`). It can be
parameterized with any type `A` which has a `Semiring[A]`. However, many useful
operations will only be available for types that also have instances of
`Field[A]`, `NRoot[A]`, and `Trig[A]` available.  In general these values are
as exact as their underlying `A` values are, although in some cases approximate
results are necessarily returned (in cases where roots or trigonometry
functions are used).

`Complex[A]` is specialized on `Float` and `Double`, so most operations should
be quite fast and not cause unnecessary boxing. However, these types use more
memory than a non-generic complex number based on `Double` values would, and
are a bit slower.
