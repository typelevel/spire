---
layout: docs
title:  "Interval"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Interval.scala"
scaladoc: "#spire.math.Interval"
---

## Interval[A]

`Interval` supports arithmetic across a range of possible `A`
values. This can be thought of as representing uncertainty about a
single, actual value, or as operating on the entire set of values
simultaneously. Any type that has an `Order[A]` can be used in an
interval, although most arithmetic operations will require additional
type classes (ranging from `AdditiveSemigroup[A]` for `+` to
`Field[A]` for `/`).

Intervals may be unbounded on either side, and bounds can be open or
closed.  (An interval includes closed boundaries, but not open
boundaries). Here are some string representations of various
intervals:

 * `[3, 6]` the set of values between 3 and 6 (including both).
 * `(2, 4)` the set of values between 2 and 4 (excluding both).
 * `(1, 2]` half-open set, including 1 but not 2.
 * `(-∞, 5)` the set of values less than 5.

Intervals model continuous spaces, even if the type A is discrete. So
for instance when `(3, 4)` is an `Interval[Int]` it is not considered
"empty" , even though there are no `Int` values between 3 and 4. This
is because we can multiply the interval by 2 to get `(6, 8)` which is
clearly not empty. The underlying continuous interval contains values
which when multiplied by a scalar become valid `Int` values.
