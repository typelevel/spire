package spire.optional

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.{specialized => spec}

import spire.algebra._
import spire.std.{ SeqVectorEq, SeqVectorOrder }
import spire.std.{ ArrayVectorEq, ArrayVectorOrder }
import spire.std.MapVectorEq
import spire.math._
import spire.macrosk._

import java.lang.Long.numberOfTrailingZeros
import java.lang.Math
import java.math.BigInteger

import scala.collection.SeqLike

/**
 * This provides an implicit `Eq[A]` for any type `A` using Scala's (Java's)
 * `==` (`equals`). This is generally considered a bad idea, since it means you
 * lose all type safety -- for instance, any 2 types can always be compared as
 * `Eq[Any]`.
 */
object genericEq {
  @SerialVersionUID(0L)
  private class GenericEq[@spec A] extends Eq[A] with Serializable {
    def eqv(x:A, y:A): Boolean = x == y
  }

  implicit def generic[@spec A]: Eq[A] = new GenericEq[A]
}

trait VectorOrderLow {
  implicit def seqEq[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      A0: Eq[A], module: Module[CC[A], A]) = new SeqVectorEq[A, CC[A]]()(A0, module.scalar)

  implicit def arrayEq[@spec(Int,Long,Float,Double) A](implicit ev: Eq[A], module: Module[Array[A], A]) =
    new ArrayVectorEq[A]()(ev, module.scalar)

  implicit def mapEq[K, V](implicit V0: Eq[V], module: Module[Map[K, V], V]) =
    new MapVectorEq[K, V]()(V0, module.scalar)
}

/**
 * This object provides implicit instances of Eq and Order for Seq-likes
 * that will behave like infinite vectors. Essentially all this means is that
 * `Seq(0, 0, 0) === Seq()`, since both are infinite vectors of zeros. Any
 * element not explicitly set is implied to be 0.
 */
object vectorOrder extends VectorOrderLow {
  implicit def seqOrder[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      A0: Order[A], module: Module[CC[A], A]) = new SeqVectorOrder[A, CC[A]]()(A0, module.scalar)

  import spire.std.ArraySupport

  implicit def arrayOrder[@spec(Int,Long,Float,Double) A](implicit ev: Order[A], module: Module[Array[A], A]) =
    new ArrayVectorOrder[A]()(ev, module.scalar)
}

/**
 * This provides orderings (Order and Eq) for Float and Double that have
 * a total order. Specifically, this will order NaN's consistently, rather
 * than having their order be undefined. However, this won't be as fast as
 * the default ordering.
 */
object totalfloat {
  trait TotalFloatOrder extends Order[Float] {
    override def eqv(x:Float, y:Float) = java.lang.Float.compare(x, y) == 0
    override def neqv(x:Float, y:Float) = java.lang.Float.compare(x, y) != 0
    override def gt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
    override def gteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
    override def lt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
    override def lteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
    override def min(x: Float, y: Float) = if (java.lang.Float.compare(x, y) < 0) x else y
    override def max(x: Float, y: Float) = Math.max(x, y)
    def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
  }
  implicit final val TotalFloatOrder = new TotalFloatOrder {}

  trait TotalDoubleOrder extends Order[Double] {
    override def eqv(x:Double, y:Double) = java.lang.Double.compare(x, y) == 0
    override def neqv(x:Double, y:Double) = java.lang.Double.compare(x, y) != 0
    override def gt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
    override def gteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
    override def lt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
    override def lteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
    override def min(x: Double, y: Double) = if (java.lang.Double.compare(x, y) < 0) x else y
    override def max(x: Double, y: Double) = Math.max(x, y)
    def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
  }
  implicit final val TotalDoubleOrder = new TotalDoubleOrder {}
}

object rationalTrig {
  implicit val trigRational = new Trig[Rational] {
    val r180 = Rational(180)
    import spire.std.double._
    def acos(a: Rational): Rational = Rational(spire.math.acos(a.toDouble))
    def asin(a: Rational): Rational = Rational(spire.math.asin(a.toDouble))
    def atan(a: Rational): Rational = Rational(spire.math.atan(a.toDouble))
    def atan2(y: Rational,x: Rational): Rational = Rational(spire.math.atan2(y.toDouble, x.toDouble))
    def cos(a: Rational): Rational = Rational(spire.math.cos(a.toDouble))
    def cosh(x: Rational): Rational = Rational(spire.math.cosh(x.toDouble))
    val e: Rational = Rational(spire.math.e)
    def exp(a: Rational): Rational = Rational(spire.math.exp(a.toDouble))
    def expm1(a: Rational): Rational = Rational(spire.math.expm1(a.toDouble))
    def log(a: Rational): Rational = Rational(spire.math.log(a.toDouble))
    def log1p(a: Rational): Rational = Rational(spire.math.log1p(a.toDouble))
    val pi: Rational = Rational(spire.math.pi)
    def sin(a: Rational): Rational = Rational(spire.math.sin(a.toDouble))
    def sinh(x: Rational): Rational = Rational(spire.math.sinh(x.toDouble))
    def tan(a: Rational): Rational = Rational(spire.math.tan(a.toDouble))
    def tanh(x: Rational): Rational = Rational(spire.math.tanh(x.toDouble))
    def toDegrees(a: Rational): Rational = (a * r180) / pi
    def toRadians(a: Rational): Rational = (a / r180) * pi
  }
}
