---
layout: docs
title:  "BigDecimal"
section: "numbers"
---

## BigDecimal

This fractional type is different than the previous floating point values. It
contains a `MathContext` object which specifies a certain number of decimal
digits of precision (34 by default). Results will be rounded to this level of
precision, which also makes this type not associative in some cases (although
with user-specified precision it is easier to avoid cases where this matters).

The math context also defines how values should be rounded. Since this type is
decimal, it can exactly represent any decimal number (unlike a floating point
value) although its math context will need enough digits to do so.

As with floating point, Spire makes a best effort to support this type even
though there may be problems related to precision and rounding. Spire also
provides capabilities which the underlying type lacks, including roots,
fractional powers, and trigonometric methods.
