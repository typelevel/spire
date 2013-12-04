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
 
The methods in these type classes are always given text names (like
`plus`). In some cases these names correspond to symbolic operators:
in the case of `plus`, it corresponds with `+`. When using these type
classes, users have the option of using the symbolic syntax on the
values directly or calling the method on the type class instance:

```scala
import spire.algebra.Ring
import spire.syntax.ring._

def usingSymbols[A: Ring](x: A, y: A): A = x + y
def usingNames[A](x: A, y: A)(implicit r: Ring[A]): A = r.plus(x, y)
```

Some methods (e.g. `sqrt`) do not have corresponding symbols. In those
cases, the method name itself can be used with the values:

```scala
def sqrt[A: NRoot](x: A): A = x.sqrt
```

#### Package Layout

In the case of `Ring[A]`, the type class itself is located in
`spire.algebra`. Except for a few special cases, all of Spire's type
classes can be found in `spire.algebra`.

Type class instances can be found in two different places. For types
defined in Spire, or code that is aware of Spire, type class instances
should be placed in the type's companion object. For example, `UByte`
(an unsigned byte type) has an instance of `Rig[UByte]` contained in
its companion object.

For types defined elsewhere that Spire supports directly (for example
the built-in number types) Spire defines objects in `spire.std` which
contain their instances. So to get all the instances for `Int` you'd
import them from `spire.std.int._`. To get all these "standard
instances" at one go, import `spire.std.any._`. This pattern should
also be used when supporting other number types that are not
Spire-aware.

Finally, syntax implicits are imported from objecs in
`spire.syntax`. To get the syntax for `Ring[A]` you'd import
`spire.syntax.ring._`. Again, there is a shortcut package: you can
import `spire.syntax.all._` to get all syntax.

These imports might seem a bit confusing, but they are very useful
when you find a situation where Spire's types or operators conflict
with another library's.  We provide an even more basic import
(`spire.implicits._`) for when you want all instances and all
operators. This is nice when working in the console or experimenting,
and for when you're sure there won't be a conflict.

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
`spire.std.any._`. The implicits providing the binary operators `+`
and `*` (and also the implicit to convert the integer literal into an
`A`) were all imported form `spire.syntax.ring._`. And the `Ring`
context bound is really just sugar for an implicit parameter (the type
class instance).

Hopefully this small example gives you an idea of the basic mechanics
behind Spire's generic math capabilities.

#### Specialization

To achieve speed on-par with direct (non-generic) code, you will need
to use specialization. The good news is that most of Spire's code is
already specialized (and tested for proper performance). The bad news
is that you'll have to annotate all your generic code like so:

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

There are too many gotchas with specialization to list here. But the
(very) short guide to specialization is:

 1. It's much easier to specialize methods.
 2. Calls from generic code into specialized code are not specialized.
 3. Limit specialization to types you'll use via `@sp(Int, Double)`.
 4. Specialization will increase bytecode size by a factor of x2-10.

If you have questions about specialization feel free to ask on the
mailing list. You may notice that some code in Spire is structured in
an unusual way, and often this is to make sure specialization works
properly.

You may find that it's easy to develop generic code without using
specialization first (to keep things simple) and then going back and
adding annotations later if necessary. This helps keep things simple
while you get your code working correctly, and it's a (relatively)
minor change to enable specialization later (as long as you are
consistent).

Of course, if your code is not generic, you can call into Spire's
specialized code without worrying about any of this (and the result
will be unboxed and fast).

### Type Classes

#### Properties

Spire's type classes are often described in terms of properties (or
"laws").  These properties must be true no matter what values are
used.

Here's a brief description of some of the most common properties:

 * *associativity*: `|+|` is associative if `(a |+| b) |+| c` = `a |+| (b |+| c)`.
 * *identity*: `id` is an identity value for `|+|` if `a |+| id` = `a` = `id |+| a`.
 * *inverse*: `|+|` has an `inverse` operation if `a |+| a.inverse` = `id` = `a.inverse |+| a`.
 * *commutativity*: `|+|` is commutative if `a |+| b` = `b |+| a`.

In some cases the operator names are different (e.g. `+`, `*`) but the
properties themselves remain the same.

### Eq

Spire provides an `Eq[A]` type class to represent type-safe
equality. This allows us to talk about types for which there isn't a
computationally useful notion of equality, and also to avoid
programming errors caused by universal equality.

`Eq[A]` provides two operators

 * `eqv` (`a === b`) equality operator.
 * `neqv` (`a =!= `) inequality operator (defaults to `!(a === b)`).

Spire requires that `eqv` obey the laws of an equivalence relation, namely:

 * `a === a` (*reflexivity*)
 * if `a === b` then `b === a` (*symmetry*)
 * if `a === b` then `a` is `b` (*anti-symmetry*)
 * if `a === b` and `b === c` then `a === c` (*transitivity*)

The anti-symmetry property may seem confusing. The idea is that if `a === b`
then `a` and `b` must be substitutable for each other, such that for any
expression `f(x)`, `f(a) === f(b)`.

### Order

Total orderings in Spire are supported by the `Order[A]` type
class. Unlike other ordering type classes
(e.g. `scala.math.Ordering`), this one is specialized to avoid boxing.
`Order[A]` extends `Eq[A]` can be implemented via a single `compare`
method, although it provides all of the following:

 * `eqv` (`a === b`)
 * `neqv` (`a =!= b`)
 * `lt` (`a < b`)
 * `gt` (`a > b`)
 * `lteqv` (`a <= b`)
 * `gteqv` (`a >= b`)
 * `compare` (`a compare b`)
 * `min` (`a min b`)
 * `max` (`a max b`)

Instances of `Order[A]` are required to observe the following
properties:

 * if `a <= b` and `b <= a` then `a === b` (*anti-symmetry*)
 * if `a <= b` and `b <= c` then `a <= b` (*transitivity*)
 * either `a <= b` or `b <= a` (*totality*)

Additionally, total orderings across fields should obey the following
additional laws:

 * if `a <= b` then `(a + c) <= (b + c)` (*O1*)
 * if `zero <= a` and `zero <= b` then `zero <= (a * b)` (*O2*)
 
(These are laws are required by ordered fields.)

In some cases users may need to use (or define) total orderings that
do not follow all these laws, or may break laws required by other
structures. An example would be the lexicographic ordering of complex
numbers, which breaks *O2*.  In these cases, users will need to be
aware of the risks and limit their use to situations where the
particular law is not needed.

#### Groups

The most basic algebraic type classes Spire supports involve a single
associative binary operator (called `op` and represented as `|+|`):

 * `Semigroup[A]` just the associative operator `|+|`, nothing more.
 * `Monoid[A]` a semigroup that also has an identity element `id`.
 * `Group[A]` a monoid that also has an inverse operation (`inverse` or `-`).
 * `CSemigroup[A]` a semigroup that is commutative.
 * `CMonoid[A]` a monoid that is commutative.
 * `AbGroup[A]` an "abelian group", a group that is commutative.

Most types have many possible implementations of these types
classes. In these cases Spire requires users to explicitly choose
which implementation they want.

Spire also defines two parallel group heirarchies for *additive* and
*multiplicative* groups. These have the same properties but different
names and symbols. The following list provides the generic, additive,
and multiplicative variants:

 * operator method: `op`, `plus`, `times`
 * operator symbol: `|+|`, `+`, `*`
 * identity name: `id`, `zero`, `one`
 * inverse method: `inverse`, `negate`, `reciprocal`
 * inverse symbol: `inverse`, `-` (unary), `reciprocal`
 * inverse binary operator: `|-|`, `-` (binary), `/`

#### Rings and Fields

Rings are a set together with two binary operation (additive and
multiplicative). Spire defines these by extending the appropriate
additive and multiplicative group traits. The following list roughly
describes the Ring-like type classes Spire provides:

 * `Semiring[A]` provides `+`, `zero`, and `*`.
 * `Rig[A]` provides `+`, `zero`, `*`, and `one`.
 * `Rng[A]` provides commutative `+`, `zero`, `-`, and `*`.
 * `Ring[A]` provides commutative `+`, `zero`, `-`, `*`, and `one`.
 * `CRing[A]` provides commutative `+`, `zero`, `-`, commutative `*`, and `one`.

The following list makes clear how these type classes are defined via
inheritance:

 * `Semiring[A]` extends `AdditiveMonoid[A]` with `MultiplicativeSemigroup[A]`
 * `Rig[A]` extends `Semiring[A]` with `MultiplicativeMonoid[A]`
 * `Rng[A]` extends `Semiring[A]` with `AdditiveAbGroup[A]`
 * `Ring[A]` extends `Rig[A]` with `Rng[A]`
 * `CRing[A]` extends `Ring[A]` with `MultiplicativeCMonoid[A]`

Rings also provide a `pow` method (`**`) for doing repeated multiplication.

#### EuclideanRings

Spire supports euclidean domains (called `EuclideanRing[A]`). A
euclidean domain is a commutative ring (`CRing[A]`) that also supports
euclidean division (e.g. floor division or integer division). This
structure generalizes many useful properties of the integers (for
instance, quotients and remainders, and greatest common divisors).

Formally, euclidean domains have a *euclidean function* f such that
for any `x` and `y` in `A`, if `y` is nonzero, then there are `q` and
`r` (quotient and remainder) such that `a = b*q + r` and `r = 0` or
`f(r) < f(b)`. For integers, `f` is usually the absolute value
function.

Spire's `EuclideanRing[A]` supports the following operations:

 * `quot` (`a /~ b`) finding the quotient (often integer division).
 * `mod` (`a % b`) the remainder from the quotient operation.
 * `quotmod` (`a /% b`) combines `quot` and `mod` into one operation.
 * `gcd` (`a gcd b`) find the greatest common divisor of `a` and `b`.
 * `lcm` (`a lcm b`) find the lowest common multiple of `a` and `b`.

Spire requires that `b * (a /~ b) + (a % b)` is equivalent to `a`.

#### Fields

Fields are commutative rings with commutative multiplication and
multiplicative inverses for all non-zero elements. Fields generalize
how most people think about rational numbers.

Spire's `Field[A]` supports the following operations:

 * `div` (`a / b`) divide `a` by `b`.
 * `reciprocal` (`a.reciprocal`) the multiplicative inverse of `a`, i.e. `one/a`.

Even though fields sit at the top of the ring hierarchy, there are
many operations which are not provided by fields:

 * equality and ordering (provided by `Eq[A]` and `Order[A]`).
 * square root, and other roots (provided by `NRoot[A]`).
 * sine, cosine, and trigonometric functions (provided by `Trig[A]`).

#### Irrational and Transcendental type classes

Spire supports square roots and fractional powers via
`NRoot[A]`. There are three basic methods available:

 * `sqrt` (`a.sqrt`) finds the square root of `a`
 * `nroot` (`(a nroot k)`) finds the kth root of `a`
 * `fpow` (`(a fpow b)`) takes `a` to the fractional power `b`

Spire does not have any fractional types that can represent irrational
roots exactly. This means that many laws we might like to write about
roots will be weaker than we would like:

 * `a.sqrt` = `(a nroot 2)` = `(a fpow 2)`
 * if `A` can represent `1/k` exactly, then `(a nroot k)` = `(a fpow k.reciprocal)`
 * if `(a nroot k)` is rational, then `(a nroot k).pow(k)` = `a`
 
Approximate types like `Double` and `BigDecimal` have a built-in
precision to which Spire can find roots. Exact types like `Rational`
do not have `NRoot` instances defined by default, but instances can
instantiated with user-provided precision.

Similarly, Spire supports the Trigonometric functions via
`Trig[A]`. The preceeding caveats about precision apply to these
functions and values as well. The following methods are supported:

  * `e` Euler's number
  * `pi` Ratio of a circle's diameter to its circumfernce.

  * `exp(a)` Raise `e` to `a`-th power.
  * `expm1(a)` Equivalent to `exp(a) - 1` with less error.
  * `log(a)` Find the natural logarithm of `a` (`r` such that `expr(r)` = `a`)
  * `log1p(a)` Equivalent to `log(1 + a)` but with less error.

  * `sin(a)` Sine: the y-coordinate of the unit circle.
  * `cos(a)` Cosine: the x-coordinate of the unit circle.
  * `tan(a)` Tangent: equivalent to `sin(a) / cos(a)`.

  * `asin(a)` inverse sine function, `asin(sin(a))` = `a`.
  * `acos(a)` inverse cosine function, `acos(cos(a))` = `a`.
  * `atan(a)` inverse tangent function, `atan(tan(a))` = `a`.
  * `atan2(y, x)` like `atan` but returns results in `(0, 2pi)`.

  * `sinh(x)` hyperbolic sine, y-coordinate of the unit hyperbola.
  * `cosh(x)` hyperbolic cosine, x-coordinate of the unit hyperbola.
  * `tanh(x)` hyperbolic tangent, `sinh(a) / cosh(a)`.

  * `toRadians(a)` convert degrees (e.g. `180`) to pi-radians (e.g. `pi`)
  * `toDegrees(a)` convert pi-radians (e.g. `pi/2`) to degrees (e.g. `90`).

Spire is able to calculate trigonometric values (like pi) and
functions (like sine) to arbitrary precision when using
`BigDecimal`. Unlike with `NRoot`, there is no support for creating
`Trig[Rational]` instances with arbitrary precision (although an
instance with `Double` precision can be found in
`spire.optional.rationalTrig`).

#### Modules, VectorSpaces, &co

TODO

### Types

This section attempts to chronicle the existing number types in terms of their
capabilities and problems.

#### Byte, Short, Int, and Long

These built-in integral types are all signed and have a fixed-width (8, 16,
32, and 64 bits respectively). Division with these types is truncated, and
overflow can silently occur when numbers to get too big (or too small).
Division by zero will trigger an exception.

It's worth noting that the JVM does not support operating on `Byte` and
`Short` directly: these operations will usually return `Int`. This can cause
confusion when using type inference, and can also lead to differences between
direct code (where adding bytes produces an int) and generic code (where
adding bytes produces a byte).

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
operations are not always associative. Spire assumes that users who work with
`Float` and `Double` are aware of these problems, and provides instances like
`Ring[Double]` even though it will fail to be associative in some cases.

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
decimal, it can exactly represent any decimal number (unlike a floating point
value) although its math context will need enough digits to do so.

As with floating point, Spire makes a best effort to support this type even
though there may be problems related to precision and rounding. Spire also
provides capabilities which the underlying type lacks, including roots,
fractional powers, and trigonometric methods.

#### Rational

This fractional type represents a rational number, a fraction of two integers
(`n/d`). It is an exact type, although as you might expect it can't represent
irrational numbers without approximating them as rationals. It is unbounded,
although as the fraction becomes larger or more complex, operations will
become slower. Rationals are always stored in simplest form to speed up future
calculations.

This is probably the easiest fractional type to use correctly.

#### SafeLong

This integral type is also unbounded, like `BigInt`. However, it is more
efficient for small values, where it will use a `Long` instead. There is
usually no reason to prefer using a `BigInt` over a `SafeLong` except to
comply with an external API, or in cases where all the values are known to
exceed a long's storage capacity.

#### Natural

This is a simple unbounded, unsigned integral type. It models natural numbers
a as a cons list of digits (each "digit" being a 32-bit unsigned integer). For
relatively small values 32-128 bits) it is faster than `SafeLong` or `BigInt`
in most cases. For larger values it becomes a bit slower.

The `Natural` type a bit of an odd-ball type at present. However the fact that
it is guaranteed to be non-negative is nice.

#### UByte, UShort, UInt, and ULong

These unsigned integral types are provided by Spire. They have most of the same
operations as their signed counterparts, although they use unsigned division
which is a bit more involved.

They are value classes, so in most cases there should be no extra overhead
when compared with their primitive counterparts. The one exception is with
arrays. `Array[UInt]` will be boxed whereas `Array[Int]` is not. Since
conversions between `UInt` and `Int` only exit at compile-time, it's easy to
work around this issue by storing `UInt` instances in an `Array[Int]`.

Writing literal unsigned values is slightly more cumbersome than their signed
counterparts (consider `UInt(7)` versus `7`). Spire provides syntax imports
which make these slightly easier to write:

```scala
import spire.syntax.literals._

ui"7" // equivalent to UInt(7)
```

#### FixedPoint

This value class uses a `Long` with an implicit denominator. The type itself
doesn't contain information about the denominator. Instead, an implicit
`FixedScale` instance is required to provide that context when necessary (for
instance, during multiplication). Like the previous unsigned values, fixed
point values will not be boxed in most cases.

This type is designed to solve a specific type of problem and should only be
used in situations where a large number of rational numbers with the same
denominator are needed, and efficiency is very important.

#### Complex[A] and Quanternion[A]

These generic types represent complex numbers (`x + yi`) and quaternions (`w +
xi + xj + zk`) respectively. They can be parameterized with any fractional
type `A` which has a `Field[A]`, `NRoot[A]`, and `Trig[A]`. In general these
values are as exact as their underlying `A` values are, although in some cases
approximate results are necessarily returned (in cases where roots or
trigonometry functions are used).

These types are specialized, so most operations should be quite fast and not
cause unnecessary boxing. However, these types use more memory than a
non-generic complex number based on `Double` values would, and are a bit
slower.

#### Number

This is a boxed number type that approximates the semantics of numbers in a
dynamically-typed numeric tower (like Scheme or Python). There are four
subtypes of `Number`, based on `SafeLong`, `Double`, `BigDecimal`, and
`Rational`. Combining two numbers will always return a number of the highest
precision.

`Number` is a good choice for users who want simple and correct numbers. The
type keeps operations as safe as possible, while providing access to all
operators and methods.

#### Interval[A]

Interval supports arithmetic across a range of possible `A`
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

#### Polynomial[C]

Currently Spire supports univariate polynomials. These are polynomials
with a single variable (e.g. *x*) with the following structure:

```
c0 + (c1 * x^1) + (c2 * x^2) + ... + (cn * x^n)
```
  
The coefficents (`c0` through `cn`) are values of the type `C`, and
the exponents (`1` through `n`) are `Int` values (this does mean that
Spire's implementation only supports polynomials whose exponents are
less than 2147483648).

Like interval, arithmetic on polynomials is accomplished using type
classes for `C`, such as `Semiring[C]`. With the right type classes,
polynomials can support all the arithmetic operations covered by
euclidean rings, but not fields. Division and reciprocal operations
are impossible because polynomials do not support fractional or
negative exponents. Polynomials also support `interval`, `derivative`,
and other operations.

Spire does support a convenient syntax for literal polynomials. By
importing `spire.syntax.literals._` (or just `spire.implicits._`) you
can use the `poly` string interpolator to create
`Polynomial[Rational]` instances:

```scala
import spire.syntax.literals._
poly"3x^2 - 5x + 1"
poly"5/4x^6 - 7x - 2"
poly"1.2x^3 - 6.1x^2 + 9x - 3.33"
```

Spire actually supports two types of polynomials: dense and
sparse. For most simple polynomials used in these examples, you'll
probably want dense polynomials. However, in cases where your
polynomials have a few terms with very large exponents the sparse
implementation will be more efficient. In any case, the underlying
representation is an implementation detail and both types support the
same operations (and can interoperate).

#### CReal

`CReal` stands for "computable real". Spire's `CReal` implementation
is based on ERA, written in Haskell by David Lester. Computable real
numbers are those which can be computed (i.e. approximated) to any
desired precision. Unlike `Double` and `BigDecimal`, `CReal` values are not
stored as approximations, but rather as a function from desired
precision to closest approximate value.

If we have an instance `x` of `CReal` which approximates a real number
*r*, this means that for any precision *p* (in bits), our instance
will produce an *x* such that *x/2^p* is the closest rational value to
*r*. Translated into Scala, this means that `x.apply(p)` returns a
`SafeLong` value `x`, such that `Rational(x, SafeLong(2).pow(p))` is
the best approximation for *r*.

Spire represents two types of `CReal` values: `Exact` and
`Inexact`. The former are rational values for which we have an
existing instance of `Rational`, and are inexpensive to work with. The
latter are functions for approximating (potentially) irrational
values, are lazily evaluated and memoized, and can be potentially very
expensive to compute.

As with `Rational` values, operations on `CReal` values are able to
obey the relevant algebraic identities. But unlike `Rational`, `CReal`
supports roots and trigonometric functions. Furthermore, important
trig identities are also preserved:

```scala
import CReal.{sin, cos}
def circle(a: CReal): CReal = (cos(a).pow(2) + sin(a).pow(2)).sqrt
// will return CR(1) no matter what value is provided
```

One interesting consequence of the design of computable real numbers
is non-continuous operations (such as sign tests, comparisons, and
equality) cannot be performed exactly. If `x.apply(p)` returns `0`,
there is no way to know whether the value is actually zero, or just a
very small value (either positive or negative!) which is approximately
zero at this precision. Similarly, it's not possible to say that `x`
is equal to `y`, but only that they are equivalent (or not) at a given
precision.

Spire currently bakes in a "default" precision to use with these kinds
of methods. Furthermore, these methods will always work with `Exact`
values: the issues only arise when using `Inexact` values. Given that
the alternative to using `CReal` is to use another approximate type,
providing approximate comparisons and equality seems like a reasonable
compromise.

#### Real

TODO

### Pseudo-Random Number Generators, Distributions, etc

Since Spire tries to make number types more pluggable in Scala code, it only
makes sense that we'd want to allow users to easily generate a wide variety of
number types using pluggable PRNGs. The `spire.random` package contains random
number generators appropriate to many different tasks, as well as a functional
interface to creating uniform distributions of values.

#### Pseudo-Random Number Generators

Spire supports two types of PRNGs: mutable and immutable.

The `mutable.Generator` trait represents a PRNG strategy. Using
uniformly-generated `Int` or `Long` values it can generate random values,
arrays of values, and so on. Defining a generator is relatively easy (for a
very simple example see `Lcg64`).

By default, generators are not threadsafe. A synchronized generator can be
created from an unsynchronized one via the `sync` method. Generators can be
copied, and their seeds can be saved and restored. This allows users to create
deterministic streams of values by using the same seed. In general, it is
preferred for users to create and use their own generators as opposed to
relying on a single generator across threads.

Although the `mutable.Generator` trait itself only provides low-level methods
like `nextInt`, it can produce values of any type using the `Dist[A]` type
class, which will be discussed in the next seciton.

The `immutable.Generator` trait is similar to `mutable.Generator`, although
the state it stores is immutable. Each time a number is generated a new
generator is returned as well, which allows these generators to be used in a
pure-functional context. The same `Dist[A]` instances that would be used with
a mutable generator are also applicable here.

#### Creation random values with Dist[A]

The `Dist[A]` type class represents a strategy for generating a distribution
of `A` values given a `Generator` instance. `Dist[A]` makes no guarantee as to
how the `A` values are distributed (for instance, it may always return the
same value). Users who are interested in particular distributions should use
the `Uniform[A]` and `Gaussian[A]` traits to generate `Dist[A]` instances that
correspond to their needs.

The `Dist[A]` objects themselves are immutable and are powered by generators
(both mutable and immutable). They can be transformed via `map`, `flatMap`,
and other combinators. Given the appropriate structure on `A`, `Dist[A]`
instances can also be operated on as if they were value.

#### Distributions

Currently, `spire.random` provides `Uniform[A]` and `Gaussian[A]` type classes
which given appropriate parameters can produce `Dist[A]` instances. Since most
types have a (approximately) infinite number of possible values, bounds and
other constraints need to be put on these types before we can usefully talk
about (or implement) probability distributions in Spire.

Given `min` and `max`, a `Uniform[A]` instance can produce a
uniformly-distributed `Dist[A]` instance.

Given a `mean` and `stdDev`, a `Gaussian[A]` instance can produce a `Dist[A]`
whose values are distributed according to the desired gaussian distribution.
