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
  def opIsDefined(x: A, y: A): Boolean
  def op(x: A, y: A): A
}

trait NullboxSemigroupoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Semigroupoid[A] {
  def partialOp(x: A, y: A): Nullbox[A]
  def opIsDefined(x: A, y: A): Boolean = partialOp(x, y).nonEmpty
  def op(x: A, y: A): A = {
    val res = partialOp(x, y)
    if (res.isEmpty) throw new IllegalArgumentException(s"$x |+| $y is not defined") else res.get
  }
}

object Semigroupoid {
  @inline final def apply[A](implicit s: Semigroupoid[A]) = s
}

final class EnrichedSemigroupoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A](sgd: Semigroupoid[A]) extends NullboxSemigroupoid[A] {
  override def opIsDefined(x: A, y: A) = sgd.opIsDefined(x, y)
  override def op(x: A, y: A): A = sgd.op(x, y)
  def partialOp(x: A, y: A): Nullbox[A] =
    if (sgd.opIsDefined(x, y)) Nullbox[A](sgd.op(x, y)) else Nullbox.empty[A]
}

final class EnrichedSemigroup[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A](sg: Semigroup[A]) extends NullboxSemigroupoid[A] {
  override def op(x: A, y: A): A = sg.op(x, y)
  override def opIsDefined(x: A, y: A) = true
  def partialOp(x: A, y: A): Nullbox[A] = Nullbox[A](sg.op(x, y))
}

object NullboxSemigroupoid {
  implicit def enrichSemigroupoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A](implicit sgd: Semigroupoid[A]): NullboxSemigroupoid[A] = sgd match {
    case nbsgd: NullboxSemigroupoid[A] => nbsgd
    case sg: Semigroup[A] => new EnrichedSemigroup[A](sg)
    case _ => new EnrichedSemigroupoid[A](sgd)
  }
}
