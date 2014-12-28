package spire.algebra

import scala.{ specialized => spec }

import spire.util.Nullbox

/** A groupoid is a partial monoid, where every element has an inverse.
  *
  *   (i) `a.inverse |+|? a` and `a |+|? a.inverse` are always true
  *  (ii) if `a |+|? b`, then `a |+|! b |+|! b.inverse === a` and `a.inverse |+|! a |+|! b === b`
  */
trait Groupoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with PartialMonoid[A] {
  def inverse(a: A): A
  def opInverse(x: A, y: A): A = op(x, inverse(y))
  def opInverseIsDefined(x: A, y: A): Boolean = opIsDefined(x, inverse(y))
  def leftId(a: A): A = op(a, inverse(a))
  def rightId(a: A): A = op(inverse(a), a)
}

trait NullboxGroupoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with Groupoid[A] with NullboxPartialMonoid[A] {
  def partialOpInverse(x: A, y: A): Nullbox[A] = partialOp(x, inverse(y))
}

object Groupoid {
  @inline final def apply[A](implicit ev: Groupoid[A]): Groupoid[A] = ev
}
