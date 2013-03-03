package spire.algebra

import spire.macrosk.Ops
import spire.NoImplicit

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/**
 * A module generalizes a vector space by requiring its scalar need only form
 * a ring, rather than a field. In Spire, a `Module` is a left R-Module.
 */
trait Module[V, @spec(Int,Long,Float,Double) R] extends LeftModule[V, R] with RightModule[V, R] {
  implicit def scalar: Ring[R] // TODO: Can this be Rng[R] instead?

  def timesl(r: R, v: V): V
  def timesr(v: V, r: R): V = timesl(r, v)
}

/** 
 * An R-module whose scalar multiplication comes from the left.
 */
trait LeftModule[V, @spec(Int,Long,Float,Double) R] extends AdditiveAbGroup[V] {
  implicit def scalar: Ring[R]

  def timesl(r: R, v: V): V
}

/**
 * An R-module whose scalar multiplication comes from the right.
 */
trait RightModule[V, @spec(Int,Long,Float,Double) R] extends AdditiveAbGroup[V] {
  implicit def scalar: Ring[R]

  def timesr(v: V, r: R): V
}

object Module {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: Module[V, R]) = V

  implicit def IdentityModule[@spec(Int,Long,Float,Double) V](implicit ring: Ring[V]) = {
    new IdentityModule[V] {
      val scalar = ring
    }
  }
}

object LeftModule {
  implicit def module[V, @spec(Int,Long,Float,Double) R](implicit
      module: Module[V, R]): LeftModule[V, R] = module
}

object RightModule {
  implicit def module[V, @spec(Int,Long,Float,Double) R](implicit
      module: Module[V, R]): RightModule[V, R] = module
}

final class LeftModuleOps[V, F](rhs: V)(implicit ev: LeftModule[V, F]) {
  def *: (lhs:F): V = macro Ops.rbinop[F, V]
}

final class RightModuleOps[V, F](lhs: V)(implicit ev: RightModule[V, F]) {
  def :* (rhs:F): V = macro Ops.binop[F, V]
}

trait IdentityModule[@spec(Int,Long,Float,Double) V] extends Module[V, V] {
  def zero = scalar.zero
  def negate(v: V) = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}
