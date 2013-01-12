package spire.algebra

import scala.{ specialized => spec }
import scala.collection.TraversableLike
import scala.collection.generic._

/**
 * A monoid is a semigroup with a special identity element.
 */
trait Monoid[@spec(Int,Long,Float,Double) A] extends Semigroup[A] {
  def id: A
}

object Monoid extends Monoid0 {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m

  implicit object StringMonoid extends StringMonoid
  implicit def SetMonoid[A]: Monoid[Set[A]] = new CollMonoid[A, Set[A]]
  implicit def ListMonoid[A]: Monoid[List[A]] = new CollMonoid[A, List[A]]
}

trait Monoid0 {
  implicit def group[A: Group]: Monoid[A] = Group[A]
}

trait StringMonoid extends Monoid[String] {
  def id = ""
  def op(x: String, y: String): String = x + y
}

final class CollMonoid[A, SA <: TraversableLike[A, SA]](implicit cbf: CanBuildFrom[SA, A, SA]) extends Monoid[SA] {
  def id: SA = cbf().result()
  def op(x: SA, y: SA): SA = x.++(y)(cbf)
}
