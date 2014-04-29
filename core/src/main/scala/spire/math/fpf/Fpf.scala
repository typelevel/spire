package spire.math
package fpf

import scala.language.experimental.macros

import scala.reflect.macros.Context

import spire.algebra._
import spire.syntax.isReal._

/**
 * A Floating Point Filter [1] provides a `Numeric` type that wraps another
 * `Numeric` type, but defers its computation, instead providing a floating
 * point (`Double`) approximation. For some operations, like `signum`,
 * comparisons, equality checks, toFloat, etc, the `Double` approximation may
 * be used to compute the result, rather than having to compute the exact value.
 *
 * An `Fpf` can generally be used with any [[Ring]] numeric type (also
 * supports [[EuclideanRing]], [[Field]], and [[NRoot]]). However, it should be
 * kept in mind that `Fpf` knows nothing about the type its wrapping and
 * assumes that, generally, it is more accurate than it is. When an `Fpf`
 * cannot determine an answer to some predicate exactly, it will defer to the
 * wrapped value, so it probably doesn't make sense to wrap `Int`s, when an
 * `Int` will overflow before a `Double`!
 *
 * Good candidates to wrap in `Fpf` are [[BigInt]]s, [[Rational]]s,
 * [[BigDecimal]]s, and [[Algebraic]]. Note that while [[Algebraic]] has an
 * internal floating point filter, this still provides benefits. Namely, the
 * operator-fusion and allocation removal provided by the macros can make for
 * much faster hot paths.
 *
 * Note: Both equals and hashCode will generally force the exact computation.
 *       They should be avoided (prefer `===` for equals)... otherwise why use
 *       bother?
 *
 * [1] Burnikel, Funke, Seel. Exact Geometric Computation Using Cascading. SoCG 1998.
 */
final class Fpf[A](val apx: Double, val mes: Double, val ind: Int, exact0: => A) {
  def abs(implicit ev: Signed[A]): Fpf[A] = macro Fpf.absImpl[A]
  def unary_- (implicit ev: Rng[A]) : Fpf[A] = macro Fpf.negateImpl[A]
  def +(rhs: Fpf[A])(implicit ev: Semiring[A]): Fpf[A] = macro Fpf.plusImpl[A]
  def -(rhs: Fpf[A])(implicit ev: Rng[A]): Fpf[A] = macro Fpf.minusImpl[A]
  def *(rhs: Fpf[A])(implicit ev: Semiring[A]): Fpf[A] = macro Fpf.timesImpl[A]
  def /(rhs: Fpf[A])(implicit ev: Field[A]): Fpf[A] = macro Fpf.divideImpl[A]
  def sqrt(implicit ev: NRoot[A]): Fpf[A] = macro Fpf.sqrtImpl[A]

  def <(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.ltImpl[A]
  def >(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.gtImpl[A]
  def <=(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.ltEqImpl[A]
  def >=(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.gtEqImpl[A]
  def ===(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.eqImpl[A]

  def signum(implicit ev: Signed[A]): Int = macro Fpf.signImpl[A]

  // Avoid using this.
  override def equals(that: Any): Boolean = that match {
    case that: Fpf[_] =>
      if (this.error == 0 && that.error == 0) this.apx == that.apx
      else this.exact == that.exact
    case _ =>
      false
  }

  // Avoid using this.
  override def hashCode: Int = 23 * exact.hashCode

  def error: Double = mes * ind * Fpf.Eps

  lazy val exact: A = exact0
}

final class FpfApprox[A](val exact: A) extends AnyVal {
  def abs(implicit ev: Signed[A]): Fpf[A] = macro Fpf.absImpl[A]
  def unary_- (implicit ev: Rng[A]) : Fpf[A] = macro Fpf.negateImpl[A]
  def +(rhs: Fpf[A])(implicit ev: Semiring[A]): Fpf[A] = macro Fpf.plusImpl[A]
  def -(rhs: Fpf[A])(implicit ev: Rng[A]): Fpf[A] = macro Fpf.minusImpl[A]
  def *(rhs: Fpf[A])(implicit ev: Semiring[A]): Fpf[A] = macro Fpf.timesImpl[A]
  def /(rhs: Fpf[A])(implicit ev: Field[A]): Fpf[A] = macro Fpf.divideImpl[A]
  def sqrt(implicit ev: NRoot[A]): Fpf[A] = macro Fpf.sqrtImpl[A]
  def <(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.ltImpl[A]
  def >(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.gtImpl[A]
  def <=(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.ltEqImpl[A]
  def >=(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.gtEqImpl[A]
  def ===(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.eqImpl[A]
}

object FpfApprox {
  implicit def liftApprox[A: IsReal](approx: FpfApprox[A]) = {
    val apx = IsReal[A].toDouble(approx.exact)
    new Fpf[A](apx, spire.math.abs(apx), 1, approx.exact)
  }
}

final class FpfExact[A](val value: Double) extends AnyVal {
  def abs(implicit ev: Signed[A]): Fpf[A] = macro Fpf.absImpl[A]
  def unary_- : FpfExact[A] = new FpfExact[A](-value)
  def +(rhs: Fpf[A])(implicit ev: Field[A]): Fpf[A] = macro Fpf.plusImpl[A]
  def -(rhs: Fpf[A])(implicit ev: Field[A]): Fpf[A] = macro Fpf.minusImpl[A]
  def *(rhs: Fpf[A])(implicit ev: Field[A]): Fpf[A] = macro Fpf.timesImpl[A]
  def /(rhs: Fpf[A])(implicit ev: Field[A]): Fpf[A] = macro Fpf.divideImpl[A]
  def sqrt(implicit ev: NRoot[A]): Fpf[A] = macro Fpf.sqrtImpl[A]
  def <(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.ltImpl[A]
  def >(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.gtImpl[A]
  def <=(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.ltEqImpl[A]
  def >=(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.gtEqImpl[A]
  def ===(rhs: Fpf[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro Fpf.eqImpl[A]
}

object FpfExact {
  implicit def liftExact[A: Field](exact: FpfExact[A]) =
    new Fpf(exact.value, spire.math.abs(exact.value), 0, Field[A].fromDouble(exact.value))
}

object Fpf {
  final val Eps = java.lang.Double.longBitsToDouble((1023L - 52) << 52)

  @inline final def exact[A](value: Double) = new FpfExact[A](value)

  @inline final def approx[A](exact: A) = new FpfApprox[A](exact)

  @inline final def apply[A](apx: Double, mes: Double, ind: Int, exact: => A) = new Fpf[A](apx, mes, ind, exact)

  def negateImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Rng[A]]): c.Expr[Fpf[A]] =
    c.Expr[Fpf[A]](Fuser[c.type, A](c).negate(c.prefix.tree)(ev.tree).expr)

  def absImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Signed[A]]): c.Expr[Fpf[A]] =
    c.Expr[Fpf[A]](Fuser[c.type, A](c).abs(c.prefix.tree, ev.tree).expr)

  def sqrtImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[NRoot[A]]): c.Expr[Fpf[A]] =
    c.Expr[Fpf[A]](Fuser[c.type, A](c).sqrt(c.prefix.tree)(ev.tree).expr)

  def plusImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev: c.Expr[Semiring[A]]): c.Expr[Fpf[A]] =
    c.Expr[Fpf[A]](Fuser[c.type, A](c).plus(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def minusImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev: c.Expr[Rng[A]]): c.Expr[Fpf[A]] =
    c.Expr[Fpf[A]](Fuser[c.type, A](c).minus(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def timesImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev: c.Expr[Semiring[A]]): c.Expr[Fpf[A]] =
    c.Expr[Fpf[A]](Fuser[c.type, A](c).times(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def divideImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev: c.Expr[Field[A]]): c.Expr[Fpf[A]] =
    c.Expr[Fpf[A]](Fuser[c.type, A](c).divide(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def signImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Signed[A]]): c.Expr[Int] =
    c.Expr[Int](Fuser[c.type, A](c).sign(c.prefix.tree)(ev.tree))

  def ltImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.Lt))

  def gtImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.Gt))

  def ltEqImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.LtEq))

  def gtEqImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.GtEq))

  def eqImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[Fpf[A]])(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.Eq))
}
