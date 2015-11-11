package spire
package std

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

import spire.algebra.Monoid

@SerialVersionUID(0L)
final class IterableMonoid[A, SA <: TraversableLike[A, SA]](implicit cbf: CanBuildFrom[SA, A, SA])
extends Monoid[SA] with Serializable {
  def id: SA = cbf().result()
  def op(x: SA, y: SA): SA = x.++(y)(cbf)
}

trait IterableInstances {
  implicit def IterableMonoid[A, CC[A] <: TraversableLike[A, CC[A]]](implicit
    cbf: CanBuildFrom[CC[A], A, CC[A]]): Monoid[CC[A]] = new IterableMonoid[A, CC[A]]
}
