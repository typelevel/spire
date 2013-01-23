package spire.optional

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.{specialized => spec}

import spire.algebra._
import spire.math._
import spire.macrosk._

import java.lang.Long.numberOfTrailingZeros
import java.lang.Math
import java.math.BigInteger

import scala.collection.SeqLike

/**
 * This provides an implicit `Eq[A]` for any type `A` using Scala's (Java's)
 * `==` (`equals`).
 */
object genericEq {
  private class GenericEq[@spec A] extends Eq[A] {
    def eqv(x:A, y:A): Boolean = x == y
  }

  implicit def generic[@spec A]: Eq[A] = new GenericEq[A]
}

/**
 * This object provides implicit instances of Eq and Order for Seq-likes
 * that will behave like infinite vectors. Essentially all this means is that
 * `Seq(0, 0, 0) === Seq()`.
 */
object vectorOrder {
  implicit def seqEq[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      A0: Eq[A], module: Module[CC[A], A]) = new SeqVectorEq[A, CC[A]] {
    val scalar = module.scalar
    val A = A0
  }

  implicit def seqOrder[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      A0: Order[A], module: Module[CC[A], A]) = new SeqVectorOrder[A, CC[A]] {
    val scalar = module.scalar
    val A = A0
  }

  implicit def arrayEq[@spec(Int,Long,Float,Double) A](implicit
      A0: Eq[A], module: Module[Array[A], A], ct: ClassTag[A]) = new ArrayVectorEq[A] {
    val scalar = module.scalar
    val A = A0
    val classTag = ct
  }

  implicit def arrayOrder[@spec(Int,Long,Float,Double) A](implicit
      A0: Order[A], module: Module[Array[A], A], ct: ClassTag[A]) = new ArrayVectorOrder[A] {
    val scalar = module.scalar
    val A = A0
    val classTag = ct
  }

  implicit def mapOrder[K, V](implicit
      V0: Eq[V], module: Module[Map[K, V], V]) = new MapVectorEq[K, V] {
    val V = V0
    val scalar = module.scalar
  }
}

/**
 * This provides orderings (Order and Eq) for Float and Double that have
 * a total order. Specifically, this will order NaN's consistently, rather
 * than having their order be undefined. However, this won't be as fast as
 * the default ordering.
 */
object totalfloat {
  trait TotalFloatEq extends Eq[Float] {
    def eqv(x:Float, y:Float) = java.lang.Float.compare(x, y) == 0
    override def neqv(x:Float, y:Float) = java.lang.Float.compare(x, y) != 0
  }
  trait TotalFloatOrder extends Order[Float] with TotalFloatEq {
    override def gt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
    override def gteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
    override def lt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
    override def lteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
    override def min(x: Float, y: Float) = if (java.lang.Float.compare(x, y) < 0) x else y
    override def max(x: Float, y: Float) = Math.max(x, y)
    def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
  }
  implicit object TotalFloatOrder extends TotalFloatOrder

  trait TotalDoubleEq extends Eq[Double] {
    def eqv(x:Double, y:Double) = java.lang.Double.compare(x, y) == 0
    override def neqv(x:Double, y:Double) = java.lang.Double.compare(x, y) != 0
  }
  trait TotalDoubleOrder extends Order[Double] with TotalDoubleEq {
    override def gt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
    override def gteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
    override def lt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
    override def lteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
    override def min(x: Double, y: Double) = if (java.lang.Double.compare(x, y) < 0) x else y
    override def max(x: Double, y: Double) = Math.max(x, y)
    def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
  }
  implicit object TotalDoubleOrder extends TotalDoubleOrder
}
