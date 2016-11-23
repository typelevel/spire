---
layout: docs
title:  "Unsigned Integers"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/UInt.scala"
scaladoc: "#spire.math.UInt"
---

## Unsigned Integers: UByte, UShort, UInt, and ULong

These unsigned integral types are provided by Spire. They have most of
the same operations as their signed counterparts, although they use
unsigned division which is a bit more involved.

They are value classes, so in most cases there should be no extra
overhead when compared with their primitive counterparts. The one
exception is with arrays. `Array[UInt]` will be boxed whereas
`Array[Int]` is not. Since conversions between `UInt` and `Int` only
exit at compile-time, it's easy to work around this issue by storing
`UInt` instances in an `Array[Int]`.

Writing literal unsigned values is slightly more cumbersome than their
signed counterparts (consider `UInt(7)` versus `7`). Spire provides
syntax imports which make these slightly easier to write:

```tut:book
import spire.syntax.literals._

ui"7" // equivalent to UInt(7)
```
