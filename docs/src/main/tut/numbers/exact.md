---
layout: docs
title:  "Exact Real Numbers"
section: "numbers"
---

## Exact Real Numbers

One of Spire's goals is to allow users to choose their own balance of
correctness and performance. To this end, Spire tries to provide a fairly
complete hierarchy of exact, correct real number types to complement the usual
set of approximate ones found in Scala.

Spire provides exact implementations of:

 * [natural numbers](natural.html),
 * [integers](safelong.html),
 * [rationals](rational.html),
 * [algebraic numbers](algebraic.html), and
 * [computable real numbers](real.html).

Each has trade offs - generally as the flexibility of the number grows
(division, roots, trig functions), the guarantees we are able to provide around
correctness shrinks. For example, [`Rational`](rational.html) are represented
exactly, as an irreducible fraction. The structure is well known and
understood. They can be printed and compared exactly - no approximation
necessary. [`Algebraic`](algebraic.html) numbers are represented as an AST, so
we can't reach in and understand the structure of the number in a meaningful
way. However, we are still able to guarantee that comparisons, relative
approximations and and sign tests are exact and correct. [`Real`](real.html)
numbers are represented very abstractly, as a function that takes a number of
bits and produces an absolute approximation. `Real`s can always be computed to
any desired approximation with a fixed absolute (additive) error, but they
cannot be compared exactly and cannot be approximated to some relative
(multiplicative) error.
