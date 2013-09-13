## Spire User's Guide

This guide explains the architecture, layout, and usage of Spire. Initially,
we will explore some of the basic structures and patterns used by Spire. Then,
we'll look at many of the concrete types that Spire defines. Finally, we'll
peek at some of the advanced or tricky corners of the library.

### Type Classes

Like many Scala libraries, Spire uses type classes to define generic
operations.

For example, `Ring[A]` is a type class that defines many basic operations,
such as `+` and `*` on a type `A`. When using type classes it's important to
try to distinguish the following:

 1. The type class itself (`Ring[A]`). This is often a trait.
 2. Concrete instances of the type class, such as `Ring[Int]`.
 3. Syntax implicits that use the type class to define operators.

#### Package Layout

In the case of `Ring[A]`, the type class itself is located in `spire.algebra`.
Except for certain special-purpose type classes in `spire.math`, all of
Spire's type classes can be found in `spire.algebra`.

Type class members (also called instances) can be found in two different
places. For types defined in Spire, or code that is aware of Spire, type class
instances should be placed in the type's companion object. For instance,
`UByte` (an unsigned byte type) has an instance of `Rig[UByte]` contained in
its companion object.

For types defined elsewhere (including the built-in number types) Spire
defines objects in `spire.std` which contain their instances. So to get all
the instances for `Int` you'd import them from `spire.std.int._`. To get all
these "standard instances" at one go, import `spire.std.any._`.

Finally, syntax implicits are imported from objecs in `spire.syntax`. To get
the syntax for `Ring[A]` you'd import `spire.syntax.ring._`. Again, there is a
shortcut import `spire.syntax.all._` which imports all syntax.

These imports might seem a bit confusing, but they are very useful if you find
a situation where Spire's types or operators conflict with another libraries.
We provide an even more basic import `spire.implicits._` for when you want all
instances and operators. This is useful for working in the console, or for
when you're sure there won't be a conflict.

#### Usage

Most of the time, you'll be using type classes as context bounds. For
instance:

```scala
import spire.algebra._
import spire.std.any._
import spire.syntax.ring._

object Demo {
  def double[A: Ring](x: A): A = x + x
  def triple[A: Ring](x: A): A = x * 3
  println((double(3), triple(4)))
}
```

This code ends up being equivalent to:

```scala
object Demo {
  def double[A](x: A)(implicit ev: Ring[A]): A = ev.plus(x, x)
  def triple[A](x: A)(implicit ev: Ring[A]): A = ev.times(x, ev.fromInt(3))
  println((double(3)(IntAlgebra), triple(4)(IntAlgebra)))
}
```

The `IntAlgebra` type extends `Ring[Int]` and has been imported via
`spire.std.any._`. The implicits providing the `+` and `*` binary operators
(and also the implicit to convert the integer literal into an `A`) were all
imported form `spire.syntax.ring._`. And the `Ring` context bound is really
just sugar for an implicit object (the type class instance).

Hopefully this small example gives you an idea of the basic mechanics behind
Spire's generic math capabilities.

#### Specialization

To achieve speed on-par with direct (non-generic) code, you will need to use
specialization. The good news is that most of Spire's code is already
specialized (and tested for proper performance). The bad news is that you'll
have to annotate all your generic code like so:

```scala
import spire.algebra._
import spire.std.any._
import spire.syntax.ring._

import scala.{specialized => sp}

object Demo {
  def double[@sp A: Ring](x: A): A = x + x
  def triple[@sp A: Ring](x: A): A = x * 3
  println((double(3), triple(4)))
}
```

There are too many gotchas with specialization to list here. But the (very)
short guide to specialization is:

 1. It's much easier to specialize methods.
 2. Calls from generic code into specialized code are not specialized.
 3. Limit specialization to types you'll use via `@sp(Int, Double)`.
 4. Specialization will increase bytecode size by a factor of x2-10.

If you have questions about specialization feel free to ask on the mailing
list. You may notice that some code in Spire is structured in an unusual way,
and often this is to make sure specialization works properly.

Of course, if your code is not generic, you can call into Spire's specialized
code without worrying about any of this (and the result will be unboxed and
fast).

### Types

This section attempts to chronicle the existing number types in terms of their
capabilities and problems.

#### Byte, Short, Int, and Long

These built-in integral types are all signed and have a fixed-width. Division
with these types is truncated, and overflow can silently occur when numbers to
get too big (or too small). Division by zero will trigger an exception.

#### Float and Double

These fractional types correspond to IEEE-754 floating point (32- and 64-bit
respectively). They contain three sentinel values: positive and negative
infinity and NaN. Large positive and negative quantities will overflow to
their respective infinity value, and division by zero will silently go to
infinity.

Comparison and equality semantics for NaN are tricky (for example NaN == NaN
is false). This also means that there is no total ordering for doubles that
complies with IEEE comparisons. For an alternate `Ordering[Double]` that is
total, see `spire.optional.totalfloat`.

Since floating-point values are approximations of real values, loss of
precision can occur when adding values of different magnitudes. Thus, many
operations are not always associative.

#### BigInt

This integral type is unbounded--it will never overflow (although operations
will get slower and slower as the value gets larger). This is probably one of
the least difficult types to use correctly.

#### BigDecimal

This fractional type is different than the previous floating point values. It
contains a `MathContext` object which specifies a certain number of decimal
digits of precision (34 by default). Results will be rounded to this level of
precision, which also makes this type not associative in some cases (although
with user-specified precision it is easier to avoid cases where this matters).

The math context also defines how values should be rounded. Since this type is
decimal, it can exactly represent any decimal number exactly (unlike a
floating point value) although its math context will need enough digits to do
so.

#### Rational

This fractional type represents a rational number, a fraction of two integers
(`n/d`). It is an exact type, although it can't represent irrational numbers
without approximating them as rationals. It is unbounded, although as the
fraction becomes larger or more complex, operations will become slower.
Rationals are always stored in simplest form to speed up future calculations.

#### UByte, UShort, UInt, and ULong

These unsigned integral types are provided by Spire. They have most of the same
operations as their signed counterparts, although they use unsigned division
which is a bit more involved.

They are value classes, so in most cases there should be no extra overhead
when compared with their primitive counterparts. The one exception is with
arrays. `Array[UInt]` will be boxed whereas `Array[Int]` is not. Since
conversions between `UInt` and `Int` only exit at compile-time, it's easy to
work around this issue by storing `UInt` instances in an `Array[Int]`.

#### FixedPoint

This value class uses a `Long` with an implicit denominator. The type itself
doesn't contain information about the denominator. Instead, an implicit
`FixedScale` instance is required to provide that context. This type is
relatively specialized and should only be used in situations where a large
number of rational numbers with the same denominator are needed.

#### SafeLong

This integral type is also unbounded, like `BigInt`. However, it is more
efficient for small values, where it will use a `Long` instead. There is
usually no reason to prefer using a `BigInt` over a `SafeLong` except to
comply with an external API, or in cases where all numbers are known to exceed
a long's storage capacity.

#### Natural

This is a simple unbounded, unsigned integral type. For relatively small
values 32-128 bits) it is faster than `SafeLong` or `BigInt` in many cases,
although for larger values it becomes a bit slower. It's a bit of an odd-ball
at present, however the fact that it is guaranteed to be non-negative is nice.

#### Complex[A] and Quanternion[A]

These generic types represent complex numbers (`x + yi`) and quaternions (`w +
xi + xj + zk`) respectively. They can be parameterized with fractional types.
In general they are as exact as their underlying types are, although in some
cases approximate results are necessarily returned (in cases where roots or
trigonometry functions are used).

#### Number

TODO

#### Interval

TODO

#### Polynomial

TODO

#### Real

TODO

### Pseudo-Random Number Generators, Distributions, etc

Since Spire tries to make number types more pluggable in Scala code, it only
makes sense that we'd want to allow users to easily generate a wide variety of
number types using pluggable PRNGs. The `spire.random` package contains random
number generators appropriate to many different tasks, as well as a functional
interface to creating uniform distributions of values.

#### Pseudo-Random Number Generators

The `Generator` trait represents a PRNG strategy. Using uniformly-generated
`Int` or `Long` values it can generate random values, arrays of values, and so
on. Defining a generator is relatively easy (for a very simple example see
`Lcg64`).

By default, generators are not threadsafe. A synchronized generator can be
created from an unsynchronized one via the `sync` method. Generators can be
copied, and their seeds can be saved and restored. This allows users to create
deterministic streams of values by using the same seed.

Although the `Generator` class itself only provides low-level methods like
`nextInt`, it can produce values of any type using the `Dist[A]` type class,
which will be discussed in the next seciton.

#### Distributions

The `Dist[A]` type class represents a strategy for generating a (uniform)
distribution of `A` values given a `Generator` instance. Distributions
themselves are immutable, and can be transformed into other distributions via
`map`, `flatMap`, and so on.
