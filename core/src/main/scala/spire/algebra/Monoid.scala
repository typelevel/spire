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

  implicit def group[A: Group]: Monoid[A] = Group[A]
}

trait Monoid0 {
  implicit def MapMonoid[K, V: Semigroup] = new MapMonoid[K, V] {
    val scalar = Semigroup[V]
  }

  implicit def OptionMonoid[A: Semigroup] = new OptionMonoid[A] {
    val scalar = Semigroup[A]
  }

  implicit def traversable[A, CC[A] <: TraversableLike[A, CC[A]]](implicit
    cbf: CanBuildFrom[CC[A], A, CC[A]]): Monoid[CC[A]] = new CollMonoid[A, CC[A]]
}

trait Monoid1 extends Monoid0 with MonoidProductImplicits {
  implicit object StringMonoid extends StringMonoid
}

trait OptionMonoid[A] extends Monoid[Option[A]] {
  def scalar: Semigroup[A]

  def id: Option[A] = None
  def op(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
    case (Some(x), Some(y)) => Some(scalar.op(x, y))
    case (None, None) => None
    case (x, None) => x
    case (None, y) => y
  }
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
