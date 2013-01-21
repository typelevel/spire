package spire.algebra

import scala.{ specialized => spec }

trait RingAlgebra[V, @spec R] extends Module[V, R] with Rng[V]

object RingAlgebra {
  implicit def ZAlgebra[A](implicit ring: Ring[A]) = new ZAlgebra[A] {
    val vector = ring
  }
}

trait ZAlgebra[V] extends RingAlgebra[V, Int] with Ring[V] {
  implicit def vector: Ring[V]
  implicit def scalar: Ring[Int] = Ring.IntIsRing

  def zero: V = vector.zero
  def one: V = vector.one
  def negate(v: V): V = vector.negate(v)
  def plus(v: V, w: V): V = vector.plus(v, w)
  override def minus(v: V, w: V): V = vector.minus(v, w)
  def times(v: V, w: V): V = vector.times(v, w)

  def timesl(r: Int, v: V): V = vector.times(vector.fromInt(r), v)

  override def fromInt(n: Int): V = vector.fromInt(n)
}

trait FieldAlgebra[V, @spec(Float, Double) F] extends RingAlgebra[V, F] with VectorSpace[V, F]
