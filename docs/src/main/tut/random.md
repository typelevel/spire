---
layout: docs
title:  "Random Numbers"
section: "random"
position: 3
---

## Pseudo-Random Number Generators, Distributions, etc

Since Spire tries to make number types more pluggable in Scala code, it only
makes sense that we'd want to allow users to easily generate a wide variety
of number types using pluggable PRNGs. The `spire.random` package contains
random number generators appropriate to many different tasks, as well as a
functional interface to creating uniform distributions of values.

### Pseudo-Random Number Generators

**This section needs revising! Immutable generators have been replaced by the
`Random[A]` monad, and the package structure has changed a bit!**

Spire supports two types of PRNGs: mutable and immutable.

The `mutable.Generator` trait represents a PRNG strategy. Using
uniformly-generated `Int` or `Long` values it can generate random values,
arrays of values, and so on. Defining a generator is relatively easy (for a
very simple example see `Lcg64`).

By default, generators are not threadsafe. A synchronized generator can be
created from an unsynchronized one via the `sync` method. Generators can be
copied, and their seeds can be saved and restored. This allows users to
create deterministic streams of values by using the same seed. In general, it
is preferred for users to create and use their own generators as opposed to
relying on a single generator across threads.

Although the `mutable.Generator` trait itself only provides low-level methods
like `nextInt`, it can produce values of any type using the `Dist[A]` type
class, which will be discussed in the next section.

The `immutable.Generator` trait is similar to `mutable.Generator`, although
the state it stores is immutable. Each time a number is generated a new
generator is returned as well, which allows these generators to be used in a
pure-functional context. The same `Dist[A]` instances that would be used with
a mutable generator are also applicable here.

### Creation random values with Dist[A]

The `Dist[A]` type class represents a strategy for generating a distribution
of `A` values given a `Generator` instance. `Dist[A]` makes no guarantee as
to how the `A` values are distributed (for instance, it may always return the
same value). Users who are interested in particular distributions should use
the `Uniform[A]`, `Gaussian[A]`, and `Exponential[A]` traits to generate
`Dist[A]` instances that correspond to their needs.

The `Dist[A]` objects themselves are immutable and are powered by generators
(both mutable and immutable). They can be transformed via `map`, `flatMap`,
and other combinators. Given the appropriate structure on `A`, `Dist[A]`
instances can also be operated on as if they were value.

### Distributions

Currently, `spire.random` provides `Uniform[A]`, `Gaussian[A]`, and
`Exponential[A]` type classes which given appropriate parameters can produce
`Dist[A]` instances. Since most types have a (approximately) infinite number
of possible values, bounds and other constraints need to be put on these
types before we can usefully talk about (or implement) probability
distributions in Spire.

 * Given `min` and `max`, a `Uniform[A]` instance can produce a
   uniformly-distributed `Dist[A]` instance.

 * Given `mean` and `stdDev`, a `Gaussian[A]` instance can produce a
   `Dist[A]` whose values are distributed according to the desired gaussian
   distribution.

 * Given `rate`, a `Gaussian[A]` instance can produce a `Dist[A]` whose
   values are distributed according to the desired exponential distribution.
