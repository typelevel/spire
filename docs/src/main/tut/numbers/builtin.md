---
layout: docs
title:  "Scala Built-in Numbers"
section: "numbers"
---

## Scala Built-in Numbers

Spire supports Scala's built-in number types, providing instances of Spire's
type classes for them. These are available in `spire.std`.

These instances let us use Scala's standard number types with things like Spire's
`Polynomial` or `Complex`. For example, we can work with `Polynomial[BigDecimal]`:

```tut:book
import spire.math.Polynomial
import spire.std.bigDecimal._
import java.math.MathContext

val p = Polynomial(Map(0 -> BigDecimal(0.5), 1 -> BigDecimal(-3), 2 -> BigDecimal("3.2")))
p.roots(MathContext.DECIMAL128)
```

Or we can work with `Interval[Int]`:

```tut:book
import spire.math.Interval
import spire.std.int._

val i1 = Interval.open(0, 5)
val i2 = Interval.closed(3, 6)
i1 & i2
i1 | i2
```

In some cases these provide new functionality, such as adding n-roots and trig
functions to `BigDecimal`.

```tut:book
import spire.syntax.all._

BigDecimal(2, MathContext.DECIMAL128).sqrt
BigDecimal(2, new MathContext(1024)).nroot(5)

spire.math.sin(BigDecimal(0.5))
```
