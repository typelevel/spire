package spire
package algebra
package partial

import spire.util.Opt
import spire.syntax.eq._

/** A groupoid is a semigroupoid where inverse are defined for all elements,
  * and thus left and right identity elements such that:
  *
  *   (i) (leftId(g) |+|? g).get === g
  *  (ii) (g |+|? rightId(g)).get === g
  * (iii) `a.inverse |+|? a` and `a |+|? a.inverse` are always defined
  *  (iv) if `a |+|? b`, then `((a |+|? b).get |+|? b.inverse).get === a` and
  *       `((a.inverse |+|? a).get |+|? b) === b`
  *
  */
trait Groupoid[A] extends Any with Semigroupoid[A] {
  /** Tests if `a` is an identity. */
  def isId(a: A)(implicit ev: Eq[A]): Boolean = a === leftId(a)
  /** Returns the inverse element of `a` such that `(a |+|? a.inverse).get` is an identity. */
  def inverse(a: A): A
  /** Returns the left identity of `a`. */
  def leftId(a: A): A = partialOp(a, inverse(a)).get
  /** Returns the right identity of `a`. */
  def rightId(a: A): A = partialOp(inverse(a), a).get
  def partialOpInverse(x: A, y: A): Opt[A] = partialOp(x, inverse(y))
  def opInverseIsDefined(x: A, y: A): Boolean = opIsDefined(x, inverse(y))
}

trait GroupoidLowPriority {
  implicit def fromGroup[A](implicit group: Group[A]): Groupoid[A] =
    new Groupoid[A] {
      override def opIsDefined(x: A, y: A): Boolean = true
      override def opInverseIsDefined(x: A, y: A): Boolean = true
      def inverse(a: A): A = group.inverse(a)
      def partialOp(x: A, y: A): Opt[A] = Opt(group.combine(x, y))
      override def partialOpInverse(x: A, y: A): Opt[A] = Opt(group.remove(x, y))
    }
}

object Groupoid extends GroupoidLowPriority {
  @inline final def apply[A](implicit g: Groupoid[A]): Groupoid[A] = g
}
