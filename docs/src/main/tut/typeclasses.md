---
layout: docs
title:  "Type Classes"
section: "typeclasses"
position: 1
---

## Type Classes

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

```tut:book
import spire.algebra.Ring
import spire.syntax.ring._

def usingSymbols[A: Ring](x: A, y: A): A = x + y
def usingNames[A](x: A, y: A)(implicit r: Ring[A]): A = r.plus(x, y)
```

Some methods (e.g. `sqrt`) do not have corresponding symbols. In those
cases, the method name itself can be used with the values:

```tut:book
import spire.algebra.NRoot
import spire.syntax.nroot._

def sqrt[A: NRoot](x: A): A = x.sqrt
```

### Package Layout

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

Finally, syntax implicits are imported from objects in
`spire.syntax`. To get the syntax for `Ring[A]` you'd import
`spire.syntax.ring._`. Again, there is a shortcut package: you can
import `spire.syntax.all._` to get all syntax.

These imports might seem a bit confusing, but they are very useful
when you find a situation where Spire's types or operators conflict
with another library's.  We provide an even more basic import
(`spire.implicits._`) for when you want all instances and all
operators. This is nice when working in the console or experimenting,
and for when you're sure there won't be a conflict.

### Usage

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

### Specialization

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

### Properties

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
