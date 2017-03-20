---
layout: docs
title:  "Trig"
section: "typeclasses"
source: "core/shared/src/main/scala/spire/algebra/Trig.scala"
scaladoc: "#spire.algebra.Trig"
---

## Trig

Spire supports trigonometric functions with the `Trig[A]` type class.  While
Spire does have a type that can represent the results of transcendental
functions exactly - [`Real`](../numbers/real.html) - most types that support
transcendental functions are not exact (eg `Double` or `BigDecimal`). This
means that many laws we might like to write about `Trig` will be weaker than
we would like. The following methods are supported:

  * `e` Euler's number
  * `pi` ratio of a circle's diameter to its circumference.

  * `exp(a)` raises `e` to `a`-th power.
  * `expm1(a)` equivalent to `exp(a) - 1` with less error.
  * `log(a)` find the natural logarithm of `a` (`r` such that `expr(r)` = `a`)
  * `log1p(a)` equivalent to `log(1 + a)` but with less error.

  * `sin(a)` sine: the y-coordinate of the unit circle.
  * `cos(a)` cosine: the x-coordinate of the unit circle.
  * `tan(a)` tangent: equivalent to `sin(a) / cos(a)`.

  * `asin(a)` inverse sine function, `asin(sin(a))` = `a`.
  * `acos(a)` inverse cosine function, `acos(cos(a))` = `a`.
  * `atan(a)` inverse tangent function, `atan(tan(a))` = `a`.
  * `atan2(y, x)` like `atan` but returns results in `(-pi, pi]`.

  * `sinh(x)` hyperbolic sine, y-coordinate of the unit hyperbola.
  * `cosh(x)` hyperbolic cosine, x-coordinate of the unit hyperbola.
  * `tanh(x)` hyperbolic tangent, `sinh(a) / cosh(a)`.

  * `toRadians(a)` convert degrees (e.g. `180`) to pi-radians (e.g. `pi`)
  * `toDegrees(a)` convert pi-radians (e.g. `pi/2`) to degrees (e.g. `90`).

Spire is able to calculate trigonometric values (like pi) and functions (like
sine) to arbitrary precision when using `BigDecimal`. `Float`, `Double` and
`BigDecimal` will all return the most precise result they can, given their
precision. If you need a type that supports exact `Trig` operations, you can
use Spire's [`Real`](../numbers/real.html) number type.

There is no support for creating `Trig[Rational]` instances with arbitrary
precision, but an instance with `Double` precision can be found in
`spire.optional.rationalTrig`).
