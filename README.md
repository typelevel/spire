## Spire

### Overview

Spire is a numeric library for Scala which is intended to be generic, fast,
and precise.

Using features such as specialization, macros, type classes, and implicits,
Spire works had to defy conventional wisdom around performance and precision
trade-offs. A major goal is to allow developers to write efficient numeric
code without having to "bake in" particular numeric representations. In most
cases, generic implementations using Spire's specialized type classes perform
identically to corresponding direct implementations.

Spire is provided to you as free software under the MIT license.

### Set up

Spire currently relies heavily on macros introduced Scala 2.10.0, as well as
many improvements to specialization. Now that 2.10.0 has been released, the
0.3.0 release of Spire is expected to follow.

Until then, Spire 0.3.0-RC1 is available.

To get started with SBT, simply add the following to your `build.sbt` file:

```
scalaVersion := "2.10.0"

libraryDependencies += "org.spire-math" %% "spire" % "0.3.0-RC1"
```

### Number Types

In addition to supporting all of Scala's built-in number types, Spire
introduces several new ones, all of which can be found in `spire.math`:

 * `Rational` fractions of integers with perfect precision
 * `Complex[A]` and `Gaussian[A]` points on the complex plane
 * `Real` lazily-computed, arbitrary precision number type
 * `SafeLong` fast, overflow-proof integer type
 * `Interval[A]` arithmetic on open, closed, and unbound intervals
 * `Number` boxed type supporting a traditional numeric tower
 * `UByte` through `ULong` value classes supporting unsigned operations
 * `Natural` unsigned, immutable, arbitrary precision intger
 * `EuclideanRational` fractions of types from any Euclidean domain

### Type Classes

Spire provides type classes to support the a wide range of unary and binary
operations on numbers. The type classes are specialized, do no boxing, and use
implicits to provide convenient infix syntax.

The general-purpose type classes can be found in `spire.math` and consist of:

 * `Numeric[A]` all number types, makes "best effort" to support operators
 * `Fractional[A]` fractional number types, where `/` is true division
 * `Integral[A]` integral number types, where `/` is floor division
 * `Eq[A]` types that can be compared for equality
 * `Order[A]` types that can be compared and ordered
 * `Trig[A]` types that support trigonometric functions

Some of the general-purpose type classes are built in terms of a set of more
fundamental type classes defined in `spire.algebra`. Many of these correspond
to concepts from abstract algebra:

 * `Semigroup[A]` types with an associtive binary operator
 * `Monoid[A]` semigroups who have an identity element
 * `Group[A]` monoids that have an inverse operator
 * `Semiring[A]` types that form semigroups under `+` and `*`
 * `Rng[A]` types that form a group under `+` and a semigroup under `*`
 * `Rig[A]` types that form monoids under `+` and `*`
 * `Ring[A]` types that form a group under `+` and a monoid under `*`
 * `EuclideanRing[A]` rings with quotients and remainders (euclidean division)
 * `Field[A]` euclidean rings with multiplicative inverses (reciprocals)
 * `Signed[A]` types that have a sign (negative, zero, positive)
 * `NRoot[A]` types that support k-roots, logs, and fractional powers
 * `Module[V,R]` types that form a left R-module
 * `VectorSpace[V,F]` types that form a vector space
 * `NormedVectorSpace[V,F]` types with an associated norm
 * `InnerProductSpace[V,F]` types with an inner product
 * `MetricSpace[V,R]` types with an associated metric

In addition to the type classes themselves, `spire.implicits` defines many
implicits which provide unary and infix operators for the type classes. The
easiest way to use these is via a wildcard import of `spire.implicits._`.

### Getting Started

Spire contains a lot of types, as well as other machinery to provide a nice
user experience. The easiest way to use spire is via wildcard imports:

```scala
import spire.algebra._   // provides algebraic type classes
import spire.math._      // provides functions, types, and type classes
import spire.implicits._ // provides infix operators and conversions
```

Of course, you can still productively use Spire without wildcard imports, but
it may require a bit more work to figure out which functionality you want and
where it's coming from.

### Operators by Type Class

The following is an outline in more detail of the type classes provided by
Spire, as well as the operators that they use. While Spire avoids introducing
novel operators when possible, in a few cases it was unavoidable.

#### Eq and Order

The type classes provide type-safe equivalence and comparison functions. The
orderings are total, although undefined elements like `NaN` or `null` will
cause problems in the default implementations [1].

 * *Eq*
   + eqv (`===`): equivalence
   + neqv (`=!=`): non-equivalence
 * *Order*
   + compare: less-than (-1), equivalent (0), or greater-than (1)
   + gt (`>`): greater-than
   + gteqv (`>=`): greater-than-or-equivalent
   + lt (`<`): less-than
   + lteqv (`<=`): less-than-or-equivalent
   + min: find least value
   + max: find greatest value

[1] For floating-point numbers, alternate implementations that take `NaN` into
account can be imported from `spire.optional.totalfloat._`.

#### Semigroup, Monoid, and Group

These general type classes constitute very general operations. The operations
range from addition and multiplication to concatenating strings or lists, and
beyond!

 * *Semigroup*
   + op (`|+|`): associative binary operator
 * *Monoid*
   + id: an identity element
 * *Group*
   + inverse: an unary operator

There are Additive and Multiplicative refinements of these general type
classes, which are used in the Ring-family of type classes.

#### Rings &co

The Ring family of type classes provides the typical arithmetic operations
most users will expect.

 * *Semiring*
   + plus (`+`): addition
   + times (`*`): multiplication
   + pow (`**`): exponentiation (integral exponent)
 * *Rng*
   + negate (`-`): additive inverse
   + minus (`-`): subtraction
   + zero: additive identity
 * *Rig*
   + zero: additive identity
   + one: multiplicative identity
 * *Ring* (Rng + Rig)
 * *EuclideanRing*
   + quot (`/~`): quotient (floor division)
   + mod (`%`): remainder
   + quotmod (`/%`): quotient and mod
   + gcd: greatest-common-divisor
   + lcm: least-common-multiple
 * *Field*
   + reciprocal: multiplicative inverse
   + div (`/`): division
   + ceil: round up
   + floor: round down
   + round: round to nearest
 * *NRoot*
   + nroot: k-roots (k: Int)
   + sqrt: square root
   + log: natural logarithm
   + fpow (`**`): exponentiation (fractional exponent)

#### VectorSpaces &co

The vector space family of type classes provide basic vector operations. They
are parameterized on 2 types: the vector type and the scalar type.

 * *Module*
   + plus (`+`): vector addition
   + minus (`-`): vector subtraction
   + timesl (`*:`): scalar multiplication
 * *VectorSpace*
   + divr (`:/`): scalar division
 * *NormedVectorSpace* 
   + norm: vector norm
   + normalize: normalizes vector (so norm is 1)
 * *InnerProductSpace*
   + dot (`⋅`, `dot`): vector inner product

#### Numeric, Integral, and Fractional

These high-level type classes will pull in all of the relevant algebraic type
classes. Users who aren't concerned with algebraic properties directly, or who
wish for more flexibility, should prefer these type classes.

 * *Integral*: whole number types (e.g. `Int`)
 * *Fractional*: fractional/decimal types (e.g. `Double`)
 * *Numeric*: any number type, making "best effort" to support ops

The `Numeric` type class is unique in that it provides the same functionality
as `Fractional` for all number types. Each type will attempt to "do the right
thing" as far as possible, and throw errors otherwise. Users who are leery of
this behavior are encouraged to use more precise type classes.

#### BooleanAlgebra

BooleanAlgebras provide an abstraction of the familiar bitwise boolean
operators.

 * *BooleanAlgebra*
   + complement (unary `~`): complement
   + and (`&`): conjunction
   + or (`|`): disjunction
   + xor (`^`): exclusive-disjunction

BooleanAlgebras exist not just for `Boolean`, but also for `Byte`, `Short`,
`Int`, `Long`, `UByte`, `UShort`, `UInt`,  and `ULong`.

#### Errata

Additional type classes `BooleanAlgebra` and `Trig` are provided.

### Syntax

Using string interpolation and macros, Spire provides convenient syntax for
number types. These macros are evaluated at compile-time, and any errors they
encounter will occur at compile-time.

For example:

```scala
import spire.syntax._

// bytes and shorts
val x = b"100" // without type annotation!
val y = h"999"
val mask = b"255" // unsigned constant converted to signed (-1)

// rationals
val n1 = r"1/3"
val n2 = r"1599/115866" // simplified at compile-time to 13/942

// representations of the number 23
val a = x2"10111" // binary
val b = x8"27" // octal
val c = x16"17" // hex

// SI notation for large numbers
import spire.syntax.si._ // .us and .eu also available

val w = i"1 944 234 123" // Int
val x = j"89 234 614 123 234 772" // Long
val y = big"123 234 435 456 567 678 234 123 112 234 345" // BigInt
val z = dec"1 234 456 789.123456789098765" // BigDecimal
```

Spire also provides a loop macro called `cfor` whose syntax bears a slight
resemblance to a traditional for-loop from C or Java. This macro expands to a
tail-recursive function, which will inline literal function arguments.

The macro can be nested in itself and compares favorably with other looping
constructs in Scala such as `for` and `while`:

```scala
import spire.syntax._

// print numbers 1 through 10
cfor(0)(_ < 10, _ + 1) { i =>
  println(i)
}

// naive sorting algorithm
def selectionSort(ns: Array[Int]) {
  val limit = ns.length -1
  cfor(0)(_ < limit, _ + 1) { i =>
    var k = i
    val n = ns(i)
    cfor(i + 1)(_ <= limit, _ + 1) {
      j => if (ns(j) < ns(k)) k = j
    }
    ns(i) = ns(k)
    ns(k) = n
  }
}
```

### Sorting and Selection

Since Spire provides a specialized ordering type class, it makes sense that it
also provides its own sorting and selection methods. These methods are defined
on arrays and occur in-place, mutating the array. Other collections can take
advantage of sorting by converting to an array, sorting, and converting back
(which is what the Scala collections framework already does in most cases).

Sorting methods can be found in the `spire.math.Sorting` object. They are:

 * `quickSort` fastest, nlog(n), not stable with potential n^2 worst-case
 * `mergeSort` also fast, nlog(n), stable but allocates extra temporary space
 * `insertionSort` n^2 but stable and fast for small arrays
 * `sort` alias for `quickSort`

Both `mergeSort` and `quickSort` delegate to `insertionSort` when dealing with
arrays (or slices) below a certain length. So, it would be more accurate to
describe them as hybrid sorts.

Selection methods can be found in an analagous `spire.math.Selection` object.
Given an array and an index `k` these methods put the _kth_ largest element at
position `k`, ensuring that all preceeding elements are less-than or equal-to,
and all succeeding elements are greater-than or equal-to, the _kth_ element.

There are two methods defined:

 * `quickSelect` usually faster, not stable, potentially bad worst-case
 * `linearSelect` usually slower, but with guaranteed linear complexity
 * `select` alias for `quickSelect`

### Miscellany

In addition, Spire provides many other methods which are "missing" from
`java.Math` (and `scala.math`), such as:

 * log(BigDecimal): BigDecimal
 * exp(BigDecimal): BigDecimal
 * pow(BigDecimal): BigDecimal
 * pow(Long): Long
 * gcd(Long, Long): Long

### Benchmarks

In addition to unit tests, Spire comes with a relatively fleshed-out set of
micro-benchmarks written against Caliper. To run the benchmarks from within
SBT, change to the `benchmark` subproject and then `run` to see a list of
benchmarks:

```
$ sbt
[info] Set current project to spire (in build file:/Users/erik/w/spire/)
> project benchmark
[info] Set current project to benchmark (in build file:/Users/erik/w/spire/)
> run

Multiple main classes detected, select one to run:

 [1] spire.benchmark.AnyValAddBenchmarks
 [2] spire.benchmark.AnyValSubtractBenchmarks
 [3] spire.benchmark.AddBenchmarks
 [4] spire.benchmark.GcdBenchmarks
 [5] spire.benchmark.RationalBenchmarks
 [6] spire.benchmark.JuliaBenchmarks
 [7] spire.benchmark.ComplexAddBenchmarks
 [8] spire.benchmark.CForBenchmarks
 [9] spire.benchmark.SelectionBenchmarks
 [10] spire.benchmark.Mo5Benchmarks
 [11] spire.benchmark.SortingBenchmarks
 [12] spire.benchmark.ScalaVsSpireBenchmarks
 [13] spire.benchmark.MaybeAddBenchmarks
```

If you plan to contribute to Spire, please make sure to run the relevant
benchmarks to be sure that your changes don't impact performance. Benchmarks
usually include comparisons against equivalent Scala or Java classes to try to
measure relative as well as absolute performance.

### Caveats

Code is offered as-is, with no implied warranty of any kind. Comments,
criticisms, and/or praise are welcome, especially from numerical analysts! ;)

Copyright 2011-2012 Erik Osheim, Tom Switzer

The MIT software license is attached in the COPYING file.
