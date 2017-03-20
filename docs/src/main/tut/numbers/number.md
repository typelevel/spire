---
layout: docs
title:  "Number"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Number.scala"
scaladoc: "#spire.math.Number"
---

## Number

This is a boxed number type that approximates the semantics of numbers
in a dynamically-typed numeric tower (like Scheme or Python). There
are four subtypes of `Number`, based on `SafeLong`, `Double`,
`BigDecimal`, and `Rational`. Combining two numbers will always return
a number of the highest precision.

`Number` is a good choice for users who want simple and correct
numbers. The type keeps operations as safe as possible, while
providing access to all operators and methods.
