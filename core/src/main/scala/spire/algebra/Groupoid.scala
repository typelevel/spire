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

object NullboxGroupoid {
  implicit def enrichGroupoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A](implicit gd: Groupoid[A]): NullboxGroupoid[A] = gd match {
    case nbgd: NullboxGroupoid[A] => nbgd
    case gp: Group[A] => new NullboxGroupoid[A] {
      def inverse(a: A): A = gp.inverse(a)
      override def leftId(a: A): A = gp.id
      override def rightId(a: A): A = gp.id
      override def op(x: A, y: A): A = gp.op(x, y)
      override def opIsDefined(f: A, g: A) = true
      def partialOp(x: A, y: A): Nullbox[A] = Nullbox[A](gp.op(x, y))
      override def opInverse(x: A, y: A) = gp.opInverse(x, y)
      override def opInverseIsDefined(x: A, y: A) = true
      override def partialOpInverse(x: A, y: A): Nullbox[A] = Nullbox[A](gp.opInverse(x, y))
    }
    case _ => new NullboxGroupoid[A] {
      def inverse(a: A): A = gd.inverse(a)
      override def leftId(a: A): A = gd.leftId(a)
      override def rightId(a: A): A = gd.rightId(a)
      override def op(x: A, y: A): A = gd.op(x, y)
      override def opIsDefined(x: A, y: A) = gd.opIsDefined(x, y)
      def partialOp(x: A, y: A): Nullbox[A] =
        if (gd.opIsDefined(x, y)) Nullbox[A](gd.op(x, y)) else Nullbox.empty[A]
      override def opInverse(x: A, y: A): A = gd.opInverse(x, y)
      override def opInverseIsDefined(x: A, y: A) = gd.opInverseIsDefined(x, y)
      override def partialOpInverse(x: A, y: A): Nullbox[A] =
        if (gd.opInverseIsDefined(x, y)) Nullbox[A](gd.opInverse(x, y)) else Nullbox.empty[A]
    }
  }
}
