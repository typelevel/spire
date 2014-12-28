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
