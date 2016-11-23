---
layout: docs
title:  "Rational"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Rational.scala"
scaladoc: "#spire.math.Rational"
---

## Rational

This fractional type represents a rational number; a fraction of two integers
(n/d). It is an exact type, although as you might expect it can't represent
irrational numbers without approximating them as rationals. It is unbounded,
although as the fraction becomes larger or more complex, operations will
become slower. Rationals are always stored in simplest, normalized form to
speed up future calculations.

This is probably the easiest fractional type to use correctly.
