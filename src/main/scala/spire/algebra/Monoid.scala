package spire.algebra

import scala.{ specialized => spec }
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/**
 * A monoid is a semigroup with a special identity element.
 */
trait Monoid[@spec(Int,Long,Float,Double) A] extends Semigroup[A] {
  def id: A
}

object Monoid extends Monoid1 {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}

trait Monoid1 extends Monoid0 with MonoidProductImplicits {
  implicit object StringMonoid extends StringMonoid
}

trait Monoid0 {
  implicit def group[A: Group]: Monoid[A] = Group[A]

  implicit def traversable[A, CC[A] <: TraversableLike[A, CC[A]]](implicit
    cbf: CanBuildFrom[CC[A], A, CC[A]]): Monoid[CC[A]] = new CollMonoid[A, CC[A]]
}

final class CollMonoid[A, SA <: TraversableLike[A, SA]](implicit cbf: CanBuildFrom[SA, A, SA]) extends Monoid[SA] {
  def id: SA = cbf().result()
  def op(x: SA, y: SA): SA = x.++(y)(cbf)
}

final class ArrayMonoid[@spec A: ClassTag] extends Monoid[Array[A]] {
  def id = new Array[A](0)
  def op(x: Array[A], y: Array[A]) = {
    val z = new Array[A](x.length + y.length)
    System.arraycopy(x, 0, z, 0, x.length)
    System.arraycopy(y, 0, z, x.length, y.length)
    z
  }
}
