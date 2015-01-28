package spire.algebra
package partial

import scala.{ specialized => spec }
import scala.annotation.{ switch, tailrec }

import spire.util.Nullbox

/**
 * A semigroupoid is any set `A` with a partial binary associative operation (`partialOp`), 
  * which is associative in the following sense: if f,g,h are elements of the semigroupoid 
  * such that either:
  *   (i) f |+|? g is defined and g |+|? h is defined
  *  (ii) f |+|? g is defined and (f |+|? g).get |+|? h is defined
  * (iii) g |+|? h is defined and f |+|? (g |+|? h).get is defined
  * 
  * then all of f |+|? g, g |+|? h, (f |+|? g).get |+|? h, f |+|? (g |+|? h).get
  * are defined and ((f |+|? g).get |+|? h).get = (f |+|? (g |+|? h).get).get
  */
trait Semigroupoid[A] extends Any {
  def opIsDefined(x: A, y: A): Boolean
  def partialOp(x: A, y: A): Nullbox[A]
}

object Semigroupoid {
  @inline final def apply[A](implicit s: Semigroupoid[A]) = s

  implicit def fromSemigroup[A](implicit semigroup: Semigroup[A]): Semigroupoid[A] =
    new Semigroupoid[A] {
      def opIsDefined(x: A, y: A): Boolean = true
      def partialOp(x: A, y: A): Nullbox[A] = Nullbox(semigroup.op(x, y))
    }
}
