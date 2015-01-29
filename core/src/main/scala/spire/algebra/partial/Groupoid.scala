package spire.algebra
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
trait Groupoid[A] extends Any with Semigroupoid[A] with HasIsId[A] with HasInverse[A] {
  def isId(a: A)(implicit ev: Eq[A]) = a === leftId(a)
  def leftId(a: A): A = partialOp(a, inverse(a)).get
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
      def partialOp(x: A, y: A): Opt[A] = Opt(group.op(x, y))
      override def partialOpInverse(x: A, y: A): Opt[A] = Opt(group.opInverse(x, y))
    }
}

object Groupoid extends GroupoidLowPriority {
  @inline final def apply[A](implicit g: Groupoid[A]): Groupoid[A] = g
}
