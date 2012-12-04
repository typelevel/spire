package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }

/**
 * A module generalizes a vector space by requiring its scalar need only form
 * a ring, rather than a field. In Spire, a `Module` is a left R-Module.
 */
trait Module[V, @spec(Int,Long,Float,Double) R] extends AdditiveGroup[V] {
  implicit def scalar: Ring[R]

  def timesl(r: R, v: V): V
}

/**
 * An R-module whose scalar multiplication comes from the right.
 */
trait RightModule[V, @spec(Int,Long,Float,Double) R] extends AdditiveGroup[V] {
  implicit def scalar: Ring[R]

  def timesr(v: V, r: R): V
}

trait Module0 {
  implicit def IdentityModule[@spec(Int,Long,Float,Double) V](implicit ring: Ring[V]) = {
    new IdentityModule[V] {
      val scalar = ring
    }
  }

  implicit def RingAlgebraIsModule[V,@spec(Int,Long,Float,Double) R](implicit
    alg: RingAlgebra[V, R]): Module[V, R] = alg
}

trait Module1 extends Module0 {
  implicit def VectorSpaceIsModule[V,@spec(Int,Long,Float,Double) R](implicit
    vs: VectorSpace[V, R]): Module[V, R] = vs
}

object Module extends Module1

trait IdentityModule[@spec(Int,Long,Float,Double) V] extends Module[V, V] {
  def zero = scalar.zero
  def negate(v: V) = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}

final class ModuleOps[V, F](rhs: V)(implicit ev: Module[V, F]) {
  def *: (lhs:F): V = macro Ops.rbinop[F, V]
}

final class RightModuleOps[V, F](lhs: V)(implicit ev: Module[V, F]) {
  def :* (rhs:F): V = macro Ops.binop[F, V]
}
