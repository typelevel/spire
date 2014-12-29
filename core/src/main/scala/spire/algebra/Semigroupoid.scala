package spire.algebra

import scala.{ specialized => spec }
import scala.annotation.{ switch, tailrec }

import spire.util.Nullbox
/**
 * A semigroupoid is any set `A` with a partial binary associative operation (`partialOp`), 
  * which is associative in the following sense: if f,g,h are elements of the semigroupoid 
  * such that either:
  *   (i) f |+|? g is defined and g |+|? h is defined
  *  (ii) f |+|? g is defined and (f |+| g) |+|? h is defined
  * (iii) g |+|? h is defined and f |+|? (g |+| h) is defined
  * 
  * then all of f |+|? g, g |+|? h, (f |+|? g) |+|? h, f |+|? (g |+|? h)
  * are defined and (f |+| g) |+| h = f |+| (g |+| h).
  */
trait Semigroupoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any {
  def opIsDefined(f: A, g: A): Boolean
  def op(f: A, g: A): A
}

trait NullboxSemigroupoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Semigroupoid[A] {
  def partialOp(f: A, g: A): Nullbox[A]
  def opIsDefined(f: A, g: A): Boolean = partialOp(f, g).nonEmpty
  def op(f: A, g: A): A = {
    val res = partialOp(f, g)
    if (res.isEmpty) throw new IllegalArgumentException(s"$f |+| $g is not defined") else res.get
  }
}

object Semigroupoid {
  @inline final def apply[A](implicit s: Semigroupoid[A]) = s
}

object NullboxSemigroupoid {
  implicit def enrichSemigroupoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A](implicit sgd: Semigroupoid[A]): NullboxSemigroupoid[A] = sgd match {
    case nbsgd: NullboxSemigroupoid[A] => nbsgd
    case sg: Semigroup[A] => new NullboxSemigroupoid[A] {
      override def op(f: A, g: A): A = sg.op(f, g)
      override def opIsDefined(f: A, g: A) = true
      def partialOp(f: A, g: A): Nullbox[A] = Nullbox[A](sg.op(f, g))
    }
    case _ => new NullboxSemigroupoid[A] {
      override def opIsDefined(f: A, g: A) = sgd.opIsDefined(f, g)
      override def op(f: A, g: A): A = sgd.op(f, g)
      def partialOp(f: A, g: A): Nullbox[A] =
        if (sgd.opIsDefined(f, g)) Nullbox[A](sgd.op(f, g)) else Nullbox.empty[A]
    }
  }
}
