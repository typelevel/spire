package spire.algebra

import scala.{ specialized => spec }

/**
 * A module generalizes a vector space by requiring its scalar need only form
 * a ring, rather than a field.
 */
trait Module[V, @spec(Int,Long,Float,Double) R] extends AdditiveAbGroup[V] {
  implicit def scalar: Rng[R]

  def timesl(r: R, v: V): V
  def timesr(v: V, r: R): V = timesl(r, v)
}

object Module {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: Module[V, R]) = V

  implicit def IdentityModule[@spec(Int,Long,Float,Double) V](implicit ring: Ring[V]) = {
    new IdentityModule[V] {
      val scalar = ring
    }
  }
}

private[algebra] trait IdentityModule[@spec(Int,Long,Float,Double) V] extends Module[V, V] {
  def zero = scalar.zero
  def negate(v: V) = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}

final case class ZModule[V](vector: Group[V]) extends Module[V, Int] {
  def scalar = spire.std.int.IntAlgebra

  def zero: V = vector.id
  def negate(v: V): V = vector.inverse(v)
  def plus(v: V, w: V): V = vector.op(v, w)
  def timesl(k: Int, v: V): V = vector.combinen(v, k)
}
