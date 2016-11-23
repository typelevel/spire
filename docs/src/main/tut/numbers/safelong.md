---
layout: docs
title:  "SafeLong"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/SafeLong.scala"
scaladoc: "#spire.math.SafeLong"
---

## SafeLong

This is an unbounded integral type, like `BigInt`. However, it is more
efficient for small values, where it will use a `Long` instead. There is
usually no reason to prefer using a `BigInt` over a `SafeLong` except to comply
with an external API, or in cases where most values are expected to exceed a
long's storage capacity.
