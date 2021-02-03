package spire
package example

import org.apfloat._
import org.jscience.mathematics.number.{Rational => JRational}

import scala.jdk.CollectionConverters._

import spire.macros.Auto
import spire.algebra._
import spire.math._
import spire.implicits._

/**
 * An example showing how to use the Auto macros.
 *
 * The Auto macros let you auto-generate Spire type classes for Scala and Java
 * types that follow common conventions in those languages. For instance, in
 * Scala, people will usually use the `+` method to denote addition, whereas in
 * Java, they use `add`.
 */
object AutoAlgebraExample extends App {

  // Let's say you wanted to use the Apfloat arbitrary precision number type,
  // from the Apfloat Java library, with some code that uses Spire. Normally,
  // you have to create the implementations of various Spire type classes
  // (like Order, Field, etc.) yourself. However, Apfloat follows a common
  // convention for mapping operators to method names that's common in
  // Javaland. It maps addition to `plus`, subtraction to `subtract`, and
  // so-on. This convention is so common that Groovy uses it as the basis for
  // its operator overloading! With Spire, it means that we can auto generate
  // some type class instances for you, rather than have you type out the same
  // thing for every well-behaved Java number type. So, let's create some
  // instances for a few Spire type classes.

  implicit val apfloatOrder = Auto.java.order[Apfloat]
  implicit val apfloatField = Auto.java.field[Apfloat](Apcomplex.ZERO, Apcomplex.ONE)

  // That's it! Auto.java.order provides Eq & Order instances for Apfloat and
  // Auto.java.field provides a Field instance. We use the `Auto.java` macros
  // because Apfloat follows Java conventions. There is also an `Auto.scala`
  // for number types that follow Scala conventions. Anywys, now we can treat
  // this as a Spire number.

  val a = new Apfloat("0.33") + new Apfloat("0.66")
  assert(a === new Apfloat("0.99"))
  assert(new Apfloat(0) < new Apfloat(1))

  // Apfloat has some other number types too.

  implicit val apintOrder = Auto.java.order[Apint]
  implicit val apintEuclideanRing = Auto.java.euclideanRing[Apint](Apcomplex.ZERO, Apcomplex.ONE)
  implicit val aprationalOrder = Auto.java.order[Aprational]
  implicit val aprationalField = Auto.java.field[Aprational](Apcomplex.ZERO, Apcomplex.ONE)
  implicit val apcomplexOrder = Auto.java.eq[Apcomplex]
  implicit val apcomplexField = Auto.java.field[Apcomplex](Apcomplex.ZERO, Apcomplex.ONE)

  // That's a total of 8 lines to create an Apfloat -> Spire bridge!

  // Creating a JScience bridge isn't any harder:

  implicit val jrationalOrder = Auto.java.order[JRational]
  implicit val jrationalField = Auto.java.field[JRational](JRational.ZERO, JRational.ONE)

  assert(-JRational.valueOf(2L, 1L) === JRational.valueOf(-2L, 1L))

  // On top of that, Auto also has some basic support for Java collections.
  // Do you really love `java.util._`? Probably not, but let's say you still
  // need to work with Java Lists, so you want to create a monoid for it.

  implicit def javaListEq[A] = Auto.java.eq[java.util.List[A]]
  implicit def javaListMonoid[A] =
    Auto.java.collection.monoid[java.util.List[A]](new java.util.ArrayList[A]())

  // The Auto.java.collection's use addAll and the identity collection to
  // implement concatention. This means Java's collecitons behave well.

  val xs = List(1, 2).asJava
  val ys = List(3, 4).asJava
  val zs = List(5, 6).asJava
  assert(((xs |+| ys) |+| zs) === (xs |+| (ys |+| zs)))
  assert((xs |+| Monoid[java.util.List[Int]].empty) === xs)
  assert((Monoid[java.util.List[Int]].empty |+| xs) === xs)

  // As a final example, we'll recreate some instances for Scala types. These
  // already exist in Spire, so this is just to demonstrate Auto's utility.

  implicit val ushortOrder = Auto.scala.order[UShort]
  implicit val ushortRig = Auto.scala.rig[UShort](UShort(0), UShort(1))

  implicit val intOrder = Auto.scala.order[Int]
  implicit val intEuclideanRing = Auto.scala.euclideanRing[Int](0, 1)

  implicit val bigIntOrder = Auto.scala.order[BigInt]
  implicit val bigIntField = Auto.scala.euclideanRing[BigInt](BigInt(0), BigInt(1))

  implicit val rationalOrder = Auto.scala.order[Rational]
  implicit val rationalField = Auto.scala.field[Rational](Rational.zero, Rational.one)

  implicit val doubleOrder = Auto.scala.order[Double]
  implicit val doubleField = Auto.scala.field[Double](0d, 1d)

  implicit def listMonoid[A] = Auto.scala.collection.monoid[List[A]](Nil)
}
