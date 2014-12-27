package spire.algebra

import scala.{ specialized => spec }

import spire.syntax.eq._

/** A partial monoid is a semigroupoid where left and right identity elements
  * are defined for every element, such that:
  * 
  *   (i) leftId(g) |+|! g === g
  *  (ii) g |+|! rightId(g) === g
  * 
  */
trait PartialMonoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Semigroupoid[A] {
  def isId(a: A)(implicit ev: Eq[A]): Boolean = leftId(a) === a
  def leftId(a: A): A
  def rightId(a: A): A
}

object PartialMonoid {
  @inline final def apply[A](implicit m: PartialMonoid[A]): PartialMonoid[A] = m
}
