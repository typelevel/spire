package spire.algebra

import scala.{ specialized => spec }

/**
 * A simple class for numeric types that are a subset of the reals.
 */
trait IsReal[@spec A] extends Order[A] with Signed[A] {
  def toDouble(a: A): Double
}

object IsReal {
  def apply[@spec A](implicit A: IsReal[A]): IsReal[A] = A
}
