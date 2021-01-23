package spire
package std

import scala.collection.Factory

import spire.algebra.Monoid
import scala.collection.IterableOps

@SerialVersionUID(0L)
final class IterableMonoid[A, SA <: IterableOps[A, Iterable, SA]](implicit cbf: Factory[A, SA])
extends Monoid[SA] with Serializable {
  def empty: SA = cbf.newBuilder.result()
  def combine(x: SA, y: SA): SA = {
    val b = cbf.newBuilder
    b.++=(x)
    b.++=(y)
    b.result()
  }

  override def combineAll(xs: IterableOnce[SA]): SA = {
    val b = cbf.newBuilder
    xs.iterator.foreach(b ++= _)
    b.result()
  }
}

trait IterableInstances {
  implicit def IterableMonoid[A, CC[A] <: IterableOps[A, Iterable, CC[A]]](implicit
    cbf: Factory[A, CC[A]]): Monoid[CC[A]] = new IterableMonoid[A, CC[A]]
}
