package spire.algebra

import scala.{ specialized => spec }


/** A groupoid is a partial monoid, where every element has an inverse.
  *
  *   (i) `a.inverse |+|? a` and `a |+|? a.inverse` are always true
  *  (ii) if `a |+|? b`, then `a |+|! b |+|! b.inverse === a` and `a.inverse |+|! a |+|! b === b`
  */
trait Groupoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with PartialMonoid[A] {
  def inverse(a: A): A
  def partialOpInverse(x: A, y: A): Option[A] = partialOp(x, inverse(y))
  def forceOpInverse(x: A, y: A): A = forceOp(x, inverse(y))
  def isOpInverseDefined(x: A, y: A): Boolean = isOpDefined(x, inverse(y))
  def leftId(a: A): A = forceOp(a, inverse(a))
  def rightId(a: A): A = forceOp(inverse(a), a)
}

object Groupoid {
  @inline final def apply[A](implicit ev: Groupoid[A]): Groupoid[A] = ev
}
