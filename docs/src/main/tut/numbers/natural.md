---
layout: docs
title:  "Natural"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Natural.scala"
scaladoc: "#spire.math.Natural"
---

## Natural

This is a simple unbounded, unsigned integral type. It models natural numbers
a as a cons list of digits (each "digit" being a 32-bit unsigned integer).
For relatively small values (32-128 bits) it is often faster than SafeLong or
BigInt. For larger values it becomes slower.

The Natural type a bit of an odd-ball type at present. However the fact that
it is guaranteed to be non-negative is useful.
