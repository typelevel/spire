package spire.algebra

import scala.{ specialized => spec }

import spire.math.{ Real, Rational }

/**
 * A simple type class for numeric types that are a subset of the reals.
 */
trait IsReal[@spec A] extends Any with Order[A] with Signed[A] {

  /**
   * Rounds `a` the nearest integer that is greater than or equal to `a`.
   */
  def ceil(a: A): A

  /**
   * Rounds `a` the nearest integer that is less than or equal to `a`.
   */
  def floor(a: A): A

  /**
   * Rounds `a` to the nearest integer.
   */
  def round(a: A): A

  /**
   * Returns `true` iff `a` is a an integer.
   */
  def isWhole(a: A): Boolean

  /**
   * Approximates `a` as a `Double`.
   */
  def toDouble(a: A): Double
}

object IsReal {
  def apply[@spec A](implicit A: IsReal[A]): IsReal[A] = A
}

trait IsIntegral[@spec(Byte,Short,Int,Long) A] extends Any {
  def ceil(a: A): A = a
  def floor(a: A): A = a
  def round(a: A): A = a
  def isWhole(a: A): Boolean = true
}

object IsIntegral {
  def apply[@spec A](implicit A: IsIntegral[A]): IsIntegral[A] = A
}
