package spire.algebra

import scala.{ specialized => spec }
import scala.annotation.{ switch, tailrec }

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
  def isOpDefined(f: A, g: A): Boolean = partialOp(f, g).nonEmpty
  def partialOp(f: A, g: A): Option[A]
  def forceOp(f: A, g: A): A = partialOp(f, g) match {
    case Some(result) => result
    case _ => throw new IllegalArgumentException(s"$f |+|! $g is not defined")
  }

  /**
   *  Given a sequence of `as`, combine them using the semigroup and return the total.
   * 
   *  If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
//  def combineOption(as: TraversableOnce[A]): Option[A] = as.reduceOption(op)
}

object Semigroupoid {
  @inline final def apply[A](implicit s: Semigroupoid[A]) = s
}
