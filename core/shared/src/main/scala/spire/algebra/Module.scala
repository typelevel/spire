package spire
package algebra

/**
 * Left modules only allow action on the left.
 */
trait LeftModule[V, @sp(Int,Long,Float,Double) R] extends Any with AdditiveAbGroup[V] {
  implicit def scalar: Rng[R]

  def timesl(r: R, v: V): V
}
object LeftModule {
  @inline final def apply[@sp(Int,Long,Float,Double) R, V](implicit V: LeftModule[V, R]): LeftModule[V, R] = V

  implicit def IdentityLeftModule[@sp(Int,Long,Float,Double) V](implicit ring: Ring[V]): IdentityLeftModule[V] = {
    new IdentityLeftModule[V] {
      val scalar = ring
    }
  }
}

/**
 * Right modules only allow action on the right.
 */
trait RightModule[V, @sp(Int,Long,Float,Double) R] extends Any with AdditiveAbGroup[V] {
  implicit def scalar: Rng[R]

  def timesr(v: V, r: R): V
}
object RightModule {
  @inline final def apply[V, @sp(Int,Long,Float,Double) R](implicit V: RightModule[V, R]): RightModule[V, R] = V

  implicit def IdentityRightModule[@sp(Int,Long,Float,Double) V](implicit ring: Ring[V]): IdentityRightModule[V] = {
    new IdentityRightModule[V] {
      val scalar = ring
    }
  }
}

/**
 * A bimodule admits both a left and right action by the scalar ring.
 */
trait Bimodule[V, @sp(Int,Long,Float,Double) R] extends Any with LeftModule[V, R] with RightModule[V, R] {
  implicit override def scalar: Rng[R]

  def timesl(r: R, v: V): V
  def timesr(v: V, r: R): V
}

object Bimodule {
  @inline final def apply[V, @sp(Int,Long,Float,Double) R](implicit V: Bimodule[V, R]): Bimodule[V, R] = V

  implicit def IdentityBimodule[@sp(Int,Long,Float,Double) V](implicit ring: Ring[V]): IdentityBimodule[V] = {
    new IdentityBimodule[V] {
      val scalar = ring
    }
  }
}

/**
 * Modules over commutative rings generalize vector spaces by allowing the scalars
 * to belong to an arbitrary commutative ring rather than just a field.
 */
trait Module[V, @sp(Int,Long,Float,Double) R] extends Any with Bimodule[V, R] {
  implicit override def scalar: CRing[R]

  def timesl(r: R, v: V): V
  def timesr(v: V, r: R): V = timesl(r, v)
}
object Module {
  @inline final def apply[V, @sp(Int,Long,Float,Double) R](implicit V: Module[V, R]): Module[V, R] = V

  implicit def IdentityModule[@sp(Int,Long,Float,Double) V](implicit ring: CRing[V]): IdentityModule[V] = {
    new IdentityModule[V] {
      val scalar = ring
    }
  }
}

private[algebra] trait IdentityModule[@sp(Int,Long,Float,Double) V] extends Any with Module[V, V] {
  def zero: V = scalar.zero
  def negate(v: V): V = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}

private[algebra] trait IdentityBimodule[@sp(Int,Long,Float,Double) V] extends Any with Bimodule[V, V] {
  def zero: V = scalar.zero
  def negate(v: V): V = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
  def timesr(v: V, r: V): V = scalar.times(v, r)
}

private[algebra] trait IdentityLeftModule[@sp(Int,Long,Float,Double) V] extends Any with LeftModule[V, V] {
  def zero: V = scalar.zero
  def negate(v: V): V = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}

private[algebra] trait IdentityRightModule[@sp(Int,Long,Float,Double) V] extends Any with RightModule[V, V] {
  def zero: V = scalar.zero
  def negate(v: V): V = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesr(v: V, r: V): V = scalar.times(v, r)

}

final case class ZModule[V](vector: Group[V]) extends Module[V, Int] {
  def scalar: CRing[Int] = spire.std.int.IntAlgebra

  def zero: V = vector.empty
  def negate(v: V): V = vector.inverse(v)
  def plus(v: V, w: V): V = vector.combine(v, w)
  def timesl(k: Int, v: V): V = vector.combineN(v, k)
}
