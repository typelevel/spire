---
layout: docs
title:  "BigInt"
section: "numbers"
---

## BigInt

This integral type is unbounded--it will never overflow (although operations
will get slower and slower as the value gets larger). This is probably one of
the least difficult types to use correctly.

Spire provides a similar type, [`SafeLong`](safelong.html) that behaves similar
to `BigInt`, but optimizes values that can be represented as a `Long`.
