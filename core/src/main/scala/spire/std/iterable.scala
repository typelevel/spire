package spire
package std

import spire.scalacompat.{Factory, FactoryCompatOps, IterableLike}

import spire.algebra.Monoid

@SerialVersionUID(0L)
final class IterableMonoid[A, SA <: IterableLike[A, SA]](implicit cbf: Factory[A, SA])
extends Monoid[SA] with Serializable {
  def empty: SA = cbf.newBuilder.result
  def combine(x: SA, y: SA): SA = {
    val b = cbf.newBuilder
    b.++=(x)
    b.++=(y)
    b.result
  }

  override def combineAll(xs: TraversableOnce[SA]): SA = {
    val b = cbf.newBuilder
    xs.foreach(b ++= _)
    b.result()
  }
}

trait IterableInstances {
  implicit def IterableMonoid[A, CC[A] <: IterableLike[A, CC[A]]](implicit
    cbf: Factory[A, CC[A]]): Monoid[CC[A]] = new IterableMonoid[A, CC[A]]
}
