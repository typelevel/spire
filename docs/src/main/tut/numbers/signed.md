---
layout: docs
title:  "Byte, Short, Int and Long"
section: "numbers"
---

## Byte, Short, Int and Long

These built-in integral types are all signed and have a fixed-width (8, 16,
32, and 64 bits respectively). Division with these types is truncated, and
overflow can silently occur when numbers to get too big (or too small).
Division by zero will trigger an exception.

It's worth noting that the JVM does not support operating on `Byte` and
`Short` directly: these operations will usually return `Int`. This can cause
confusion when using type inference, and can also lead to differences between
direct code (where adding bytes produces an int) and generic code (where
adding bytes produces a byte).
