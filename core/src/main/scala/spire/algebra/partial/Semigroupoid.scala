package spire
package algebra
package partial

import spire.util.Opt

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
  def opIsDefined(x: A, y: A): Boolean = partialOp(x, y).nonEmpty
  def partialOp(x: A, y: A): Opt[A]
}

trait SemigroupoidLowPriority {
  implicit def fromSemigroup[A](implicit semigroup: Semigroup[A]): Semigroupoid[A] =
    new Semigroupoid[A] {
      override def opIsDefined(x: A, y: A): Boolean = true
      def partialOp(x: A, y: A): Opt[A] = Opt(semigroup.combine(x, y))
    }
}

object Semigroupoid extends SemigroupoidLowPriority {
  @inline final def apply[A](implicit s: Semigroupoid[A]): Semigroupoid[A] = s
}
