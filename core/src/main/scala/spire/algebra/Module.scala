package spire.algebra

import scala.{ specialized => spec }

/**
 * A module generalizes a vector space by requiring its scalar need only form
 * a ring, rather than a field.
 */
trait Module[V, @spec(Int,Long,Float,Double) R] extends AdditiveAbGroup[V] {
  implicit def scalar: Ring[R] // TODO: Can this be Rng[R] instead?

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
