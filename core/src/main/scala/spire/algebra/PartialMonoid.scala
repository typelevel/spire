package spire.algebra

import scala.{ specialized => spec }

import spire.syntax.eq._

import spire.util.Nullbox

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

trait NullboxPartialMonoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with PartialMonoid[A] with NullboxSemigroupoid[A]

object PartialMonoid {
  @inline final def apply[A](implicit m: PartialMonoid[A]): PartialMonoid[A] = m
}

object NullboxPartialMonoid {
  implicit def enrichPartialMonoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A](implicit pmd: PartialMonoid[A]): NullboxPartialMonoid[A] = pmd match {
    case nbpmd: NullboxPartialMonoid[A] => nbpmd
    case m: Monoid[A] => new NullboxPartialMonoid[A] {
      def leftId(a: A): A = m.id
      def rightId(a: A): A = m.id
      override def op(x: A, y: A): A = m.op(x, y)
      override def opIsDefined(x: A, y: A) = true
      def partialOp(x: A, y: A): Nullbox[A] = Nullbox[A](m.op(x, y))
    }
    case _ => new NullboxPartialMonoid[A] {
      def leftId(a: A): A = pmd.leftId(a)
      def rightId(a: A): A = pmd.rightId(a)
      override def op(x: A, y: A): A = pmd.op(x, y)
      override def opIsDefined(x: A, y: A) = pmd.opIsDefined(x, y)
      def partialOp(x: A, y: A): Nullbox[A] =
        if (pmd.opIsDefined(x, y)) Nullbox[A](pmd.op(x, y)) else Nullbox.empty[A]
    }
  }
}
