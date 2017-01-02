package spire
package algebra


import spire.math.{ Real, Algebraic, Rational }

/**
 * A simple type class for numeric types that are a subset of the reals.
 */
trait IsReal[@sp A] extends Any with Order[A] with Signed[A] {

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

  def toReal(a: A): Real
}

object IsReal {
  def apply[@sp A](implicit A: IsReal[A]): IsReal[A] = A
}

trait IsAlgebraic[@sp A] extends Any with IsReal[A] {
  def toAlgebraic(a: A): Algebraic
  def toReal(a: A): Real = toAlgebraic(a).toReal
}

object IsAlgebraic {
  def apply[@sp A](implicit A: IsAlgebraic[A]): IsAlgebraic[A] = A
}

trait IsRational[@sp A] extends Any with IsAlgebraic[A] {
  def toRational(a: A): Rational
  def toAlgebraic(a: A): Algebraic = Algebraic(toRational(a))
}

object IsRational {
  def apply[@sp A](implicit A: IsRational[A]): IsRational[A] = A
}

trait IsIntegral[@sp(Byte,Short,Int,Long) A] extends Any with IsRational[A] {
  def ceil(a: A): A = a
  def floor(a: A): A = a
  def round(a: A): A = a
  def isWhole(a: A): Boolean = true
  def toBigInt(a: A): BigInt
  def toRational(a: A): Rational = Rational(toBigInt(a))
}

object IsIntegral {
  def apply[@sp A](implicit A: IsIntegral[A]): IsIntegral[A] = A
}
