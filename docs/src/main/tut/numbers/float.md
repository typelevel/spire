---
layout: docs
title:  "Float and Double"
section: "numbers"
---

## Float and Double

These fractional types correspond to IEEE-754 floating point (32- and 64-bit
respectively). They contain three sentinel values: positive and negative
infinity and NaN. Large positive and negative quantities will overflow to their
respective infinity value, and division by zero will silently go to infinity.

Comparison and equality semantics for NaN are tricky (for example NaN == NaN is
false). This also means that there is no total ordering for doubles that
complies with IEEE comparisons. For an alternate `Ordering[Double]` that is
total, see `spire.optional.totalfloat`.

Since floating-point values are approximations of real values, loss of
precision can occur when adding values of different magnitudes. Thus, many
operations are not always associative. Spire assumes that users who work with
`Float` and `Double` are aware of these problems, and provides instances like
`Ring[Double]` even though it will fail to be associative in some cases.
