---
layout: docs
title:  "FixedPoint"
section: "numbers"
source: "extras/src/main/scala/spire/math/FixedPoint.scala"
scaladoc: "#spire.math.FixedPoint"
---

## FixedPoint

This value class uses a `Long` with an implicit denominator. The type itself
doesn't contain information about the denominator. Instead, an implicit
`FixedScale` instance is required to provide that context when necessary (for
instance, during multiplication). Like unsigned values, fixed point values will
not be boxed in most cases.

This type is designed to solve a specific type of problem and should only be
used in situations where a large number of rational numbers with the same
denominator are needed, and efficiency is very important.

### FixedScale

`FixedPoint` numbers require an implicit `FixedScale` to work correctly. All
operations performed on `FixedPoint` numbers will use the implicit scale as the
denominator. `FixedPoint` numbers with different scales cannot be combined! If
you need this, use [`Rational`](rational.html).

```tut:book
import spire.math.Rational
import spire.math.extras.{FixedPoint, FixedScale}

// Fix our denominator to be 1000.
implicit val scale: FixedScale = FixedScale(1000)

// these three values are equivalent
FixedPoint("12.345").toString(scale)            // decimal repr
FixedPoint(Rational(2469, 200)).toString(scale) // fraction repr
new FixedPoint(12345L).toString(scale)          // "raw" repr
```
