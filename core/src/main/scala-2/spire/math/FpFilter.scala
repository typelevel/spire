/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.algebra.{Field, IsReal, NRoot, Rng, Semiring, Signed}
import spire.macros.compat.Context
import spire.macros.fpf._

/**
 * A Floating-point Filter [1] provides a `Numeric` type that wraps another `Numeric` type, but defers its computation,
 * instead providing a floating point (`Double`) approximation. For some operations, like `signum`, comparisons,
 * equality checks, toFloat, etc, the `Double` approximation may be used to compute the result, rather than having to
 * compute the exact value.
 *
 * An `FpFilter` can generally be used with any [[Ring]] numeric type (also supports [[EuclideanRing]], [[Field]], and
 * [[NRoot]]). However, it should be kept in mind that `FpFilter` knows nothing about the type its wrapping and assumes
 * that, generally, it is more accurate than it is. When an `FpFilter` cannot determine an answer to some predicate
 * exactly, it will defer to the wrapped value, so it probably doesn't make sense to wrap `Int`s, when an `Int` will
 * overflow before a `Double`!
 *
 * Good candidates to wrap in `FpFilter` are [[BigInt]]s, [[Rational]]s, [[BigDecimal]]s, and [[Algebraic]]. Note that
 * while [[Algebraic]] has an internal floating-point filter, this still provides benefits. Namely, the operator-fusion
 * and allocation removal provided by the macros can make for much faster hot paths.
 *
 * Note: Both equals and hashCode will generally force the exact computation. They should be avoided (prefer `===` for
 * equals)... otherwise why use bother?
 *
 * [1] Burnikel, Funke, Seel. Exact Geometric Computation Using Cascading. SoCG 1998.
 */
final class FpFilter[A](val apx: Double, val mes: Double, val ind: Int, exact0: => A) {
  def abs(implicit ev: Signed[A]): FpFilter[A] = macro FpFilter.absImpl[A]
  def unary_-(implicit ev: Rng[A]): FpFilter[A] = macro FpFilter.negateImpl[A]
  def +(rhs: FpFilter[A])(implicit ev: Semiring[A]): FpFilter[A] = macro FpFilter.plusImpl[A]
  def -(rhs: FpFilter[A])(implicit ev: Rng[A]): FpFilter[A] = macro FpFilter.minusImpl[A]
  def *(rhs: FpFilter[A])(implicit ev: Semiring[A]): FpFilter[A] = macro FpFilter.timesImpl[A]
  def /(rhs: FpFilter[A])(implicit ev: Field[A]): FpFilter[A] = macro FpFilter.divideImpl[A]
  def sqrt(implicit ev: NRoot[A]): FpFilter[A] = macro FpFilter.sqrtImpl[A]

  def <(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.ltImpl[A]
  def >(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.gtImpl[A]
  def <=(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.ltEqImpl[A]
  def >=(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.gtEqImpl[A]
  def ===(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.eqImpl[A]

  def signum(implicit ev: Signed[A]): Int = macro FpFilter.signImpl[A]

  // Avoid using this.
  override def equals(that: Any): Boolean = that match {
    case that: FpFilter[?] =>
      if (this.error == 0 && that.error == 0) this.apx == that.apx
      else this.exact == that.exact
    case _ =>
      false
  }

  // Avoid using this.
  override def hashCode: Int = 23 * exact.hashCode

  def error: Double = mes * ind * FpFilter.Eps

  lazy val exact: A = exact0
}

final class FpFilterApprox[A](val exact: A) extends AnyVal {
  def abs(implicit ev: Signed[A]): FpFilter[A] = macro FpFilter.absImpl[A]
  def unary_-(implicit ev: Rng[A]): FpFilter[A] = macro FpFilter.negateImpl[A]
  def +(rhs: FpFilter[A])(implicit ev: Semiring[A]): FpFilter[A] = macro FpFilter.plusImpl[A]
  def -(rhs: FpFilter[A])(implicit ev: Rng[A]): FpFilter[A] = macro FpFilter.minusImpl[A]
  def *(rhs: FpFilter[A])(implicit ev: Semiring[A]): FpFilter[A] = macro FpFilter.timesImpl[A]
  def /(rhs: FpFilter[A])(implicit ev: Field[A]): FpFilter[A] = macro FpFilter.divideImpl[A]
  def sqrt(implicit ev: NRoot[A]): FpFilter[A] = macro FpFilter.sqrtImpl[A]
  def <(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.ltImpl[A]
  def >(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.gtImpl[A]
  def <=(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.ltEqImpl[A]
  def >=(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.gtEqImpl[A]
  def ===(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.eqImpl[A]
}

object FpFilterApprox {
  implicit def liftApprox[A: IsReal](approx: FpFilterApprox[A]): FpFilter[A] = {
    val apx = IsReal[A].toDouble(approx.exact)
    new FpFilter[A](apx, spire.math.abs(apx), 1, approx.exact)
  }
}

final class FpFilterExact[A](val value: Double) extends AnyVal {
  def abs(implicit ev: Signed[A]): FpFilter[A] = macro FpFilter.absImpl[A]
  def unary_- : FpFilterExact[A] = new FpFilterExact[A](-value)
  def +(rhs: FpFilter[A])(implicit ev: Field[A]): FpFilter[A] = macro FpFilter.plusImpl[A]
  def -(rhs: FpFilter[A])(implicit ev: Field[A]): FpFilter[A] = macro FpFilter.minusImpl[A]
  def *(rhs: FpFilter[A])(implicit ev: Field[A]): FpFilter[A] = macro FpFilter.timesImpl[A]
  def /(rhs: FpFilter[A])(implicit ev: Field[A]): FpFilter[A] = macro FpFilter.divideImpl[A]
  def sqrt(implicit ev: NRoot[A]): FpFilter[A] = macro FpFilter.sqrtImpl[A]
  def <(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.ltImpl[A]
  def >(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.gtImpl[A]
  def <=(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.ltEqImpl[A]
  def >=(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.gtEqImpl[A]
  def ===(rhs: FpFilter[A])(implicit ev0: Signed[A], ev1: Rng[A]): Boolean = macro FpFilter.eqImpl[A]
}

object FpFilterExact {
  implicit def liftExact[A: Field](exact: FpFilterExact[A]): FpFilter[A] =
    new FpFilter(exact.value, spire.math.abs(exact.value), 0, Field[A].fromDouble(exact.value))
}

object FpFilter {
  final val Eps = java.lang.Double.longBitsToDouble((1023L - 52) << 52)

  @inline final def exact[A](value: Double): FpFilterExact[A] = new FpFilterExact[A](value)

  @inline final def approx[A](exact: A): FpFilterApprox[A] = new FpFilterApprox[A](exact)

  @inline final def apply[A](apx: Double, mes: Double, ind: Int, exact: => A): FpFilter[A] =
    new FpFilter[A](apx, mes, ind, exact)

  def apply[A](approx: Double, exact: => A): FpFilter[A] =
    new FpFilter[A](approx, spire.math.abs(approx), 1, exact)

  def negateImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Rng[A]]): c.Expr[FpFilter[A]] =
    c.Expr[FpFilter[A]](Fuser[c.type, A](c).negate(c.prefix.tree)(ev.tree).expr)

  def absImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Signed[A]]): c.Expr[FpFilter[A]] =
    c.Expr[FpFilter[A]](Fuser[c.type, A](c).abs(c.prefix.tree, ev.tree).expr)

  def sqrtImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[NRoot[A]]): c.Expr[FpFilter[A]] =
    c.Expr[FpFilter[A]](Fuser[c.type, A](c).sqrt(c.prefix.tree)(ev.tree).expr)

  def plusImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[FpFilter[A]])(ev: c.Expr[Semiring[A]]): c.Expr[FpFilter[A]] =
    c.Expr[FpFilter[A]](Fuser[c.type, A](c).plus(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def minusImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[FpFilter[A]])(ev: c.Expr[Rng[A]]): c.Expr[FpFilter[A]] =
    c.Expr[FpFilter[A]](Fuser[c.type, A](c).minus(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def timesImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[FpFilter[A]])(ev: c.Expr[Semiring[A]]): c.Expr[FpFilter[A]] =
    c.Expr[FpFilter[A]](Fuser[c.type, A](c).times(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def divideImpl[A: c.WeakTypeTag](c: Context)(rhs: c.Expr[FpFilter[A]])(ev: c.Expr[Field[A]]): c.Expr[FpFilter[A]] =
    c.Expr[FpFilter[A]](Fuser[c.type, A](c).divide(c.prefix.tree, rhs.tree)(ev.tree).expr)

  def signImpl[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Signed[A]]): c.Expr[Int] =
    c.Expr[Int](Fuser[c.type, A](c).sign(c.prefix.tree)(ev.tree))

  def ltImpl[A: c.WeakTypeTag](c: Context)(
    rhs: c.Expr[FpFilter[A]]
  )(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.Lt))

  def gtImpl[A: c.WeakTypeTag](c: Context)(
    rhs: c.Expr[FpFilter[A]]
  )(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.Gt))

  def ltEqImpl[A: c.WeakTypeTag](c: Context)(
    rhs: c.Expr[FpFilter[A]]
  )(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.LtEq))

  def gtEqImpl[A: c.WeakTypeTag](c: Context)(
    rhs: c.Expr[FpFilter[A]]
  )(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.GtEq))

  def eqImpl[A: c.WeakTypeTag](c: Context)(
    rhs: c.Expr[FpFilter[A]]
  )(ev0: c.Expr[Signed[A]], ev1: c.Expr[Rng[A]]): c.Expr[Boolean] =
    c.Expr[Boolean](Fuser[c.type, A](c).comp(c.prefix.tree, rhs.tree)(ev0.tree, ev1.tree)(Cmp.Eq))
}
