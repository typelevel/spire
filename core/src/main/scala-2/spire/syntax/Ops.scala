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
package syntax

import spire.algebra._
import spire.algebra.lattice._
import spire.algebra.partial._
import spire.macros.Ops
import spire.math.{BitString, ConvertableFrom, ConvertableTo, Interval, Number, Rational}
import spire.util.Opt

final class EqOps[A: Eq](lhs: A) {
  def ===[B](rhs: B)(implicit ev: B =:= A): Boolean = macro Ops.eqv[A, B]
  def =!=[B](rhs: B)(implicit ev: B =:= A): Boolean = macro Ops.neqv[A, B]
}

final class PartialOrderOps[A: PartialOrder](lhs: A) {
  def >(rhs: A): Boolean = macro Ops.binop[A, Boolean]
  def >=(rhs: A): Boolean = macro Ops.binop[A, Boolean]
  def <(rhs: A): Boolean = macro Ops.binop[A, Boolean]
  def <=(rhs: A): Boolean = macro Ops.binop[A, Boolean]

  def partialCompare(rhs: A): Double = macro Ops.binop[A, Double]
  def tryCompare(rhs: A): Option[Int] = macro Ops.binop[A, Option[Int]]
  def pmin(rhs: A): Option[A] = macro Ops.binop[A, A]
  def pmax(rhs: A): Option[A] = macro Ops.binop[A, A]

  def >(rhs: Int)(implicit ev1: Ring[A]): Boolean = macro Ops.binopWithLift[Int, Ring[A], A]
  def >=(rhs: Int)(implicit ev1: Ring[A]): Boolean = macro Ops.binopWithLift[Int, Ring[A], A]
  def <(rhs: Int)(implicit ev1: Ring[A]): Boolean = macro Ops.binopWithLift[Int, Ring[A], A]
  def <=(rhs: Int)(implicit ev1: Ring[A]): Boolean = macro Ops.binopWithLift[Int, Ring[A], A]

  def >(rhs: Double)(implicit ev1: Field[A]): Boolean = macro Ops.binopWithLift[Int, Field[A], A]
  def >=(rhs: Double)(implicit ev1: Field[A]): Boolean = macro Ops.binopWithLift[Int, Field[A], A]
  def <(rhs: Double)(implicit ev1: Field[A]): Boolean = macro Ops.binopWithLift[Int, Field[A], A]
  def <=(rhs: Double)(implicit ev1: Field[A]): Boolean = macro Ops.binopWithLift[Int, Field[A], A]

  def >(rhs: Number)(implicit c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) > rhs
  def >=(rhs: Number)(implicit c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) >= rhs
  def <(rhs: Number)(implicit c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) < rhs
  def <=(rhs: Number)(implicit c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) <= rhs
}

final class OrderOps[A: Order](lhs: A) {
  def compare(rhs: A): Int = macro Ops.binop[A, Int]
  def min(rhs: A): A = macro Ops.binop[A, A]
  def max(rhs: A): A = macro Ops.binop[A, A]

  def compare(rhs: Int)(implicit ev1: Ring[A]): Int = macro Ops.binopWithLift[Int, Ring[A], A]
  def min(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def max(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]

  def compare(rhs: Double)(implicit ev1: Field[A]): Int = macro Ops.binopWithLift[Int, Field[A], A]
  def min(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Int, Field[A], A]
  def max(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Int, Field[A], A]

  def compare(rhs: Number)(implicit c: ConvertableFrom[A]): Int = c.toNumber(lhs).compare(rhs)
  def min(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs).min(rhs)
  def max(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs).max(rhs)
}

final class SignedOps[A: Signed](lhs: A) {
  def abs: A = macro Ops.unop[A]
  def sign: Sign = macro Ops.unop[Sign]
  def signum: Int = macro Ops.unop[Int]

  def isSignZero: Boolean = macro Ops.unop[Boolean]
  def isSignPositive: Boolean = macro Ops.unop[Boolean]
  def isSignNegative: Boolean = macro Ops.unop[Boolean]

  def isSignNonZero: Boolean = macro Ops.unop[Boolean]
  def isSignNonPositive: Boolean = macro Ops.unop[Boolean]
  def isSignNonNegative: Boolean = macro Ops.unop[Boolean]
}

final class TruncatedDivisionOps[A: TruncatedDivision](lhs: A) {
  def toBigIntOpt: Opt[BigInt] = macro Ops.unop[Opt[BigInt]]
  def tquot(rhs: A): A = macro Ops.binop[A, A]
  def tmod(rhs: A): A = macro Ops.binop[A, A]
  def tquotmod(rhs: A): (A, A) = macro Ops.binop[A, (A, A)]

  def fquot(rhs: A): A = macro Ops.binop[A, A]
  def fmod(rhs: A): A = macro Ops.binop[A, A]
  def fquotmod(rhs: A): (A, A) = macro Ops.binop[A, (A, A)]
}

final class InvolutionOps[A: Involution](lhs: A) {
  def adjoint: A = macro Ops.unop[A]
}

final class SemigroupoidOps[A: Semigroupoid](lhs: A) {
  def |+|?(rhs: A): Opt[A] = macro Ops.binop[A, Opt[A]]
  def |+|??(rhs: A): Boolean = macro Ops.binop[A, Boolean]
}

final class GroupoidOps[A: Groupoid](lhs: A) {
  def leftId: A = macro Ops.unop[A]
  def rightId: A = macro Ops.unop[A]
  def |-|?(rhs: A): Opt[A] = macro Ops.binop[A, Option[A]]
  def |-|??(rhs: A): Boolean = macro Ops.binop[A, Boolean]
}

final class SemigroupOps[A: Semigroup](lhs: A) {
  def |+|(rhs: A): A = macro Ops.binop[A, A]
}

final class MonoidOps[A: Monoid](lhs: A) {
  def isEmpty(implicit ev1: Eq[A]): Boolean = macro Ops.unopWithEv2[Eq[A], Boolean]
}

final class GroupOps[A: Group](lhs: A) {
  def inverse: A = macro Ops.unop[A]
  def |-|(rhs: A): A = macro Ops.binop[A, A]
}

final class AdditiveSemigroupOps[A: AdditiveSemigroup](lhs: A) {
  def +(rhs: A): A = macro Ops.binop[A, A]
  def +(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def +(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def +(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) + rhs
}

final class AdditiveMonoidOps[A: AdditiveMonoid](lhs: A) {
  def isZero(implicit ev1: Eq[A]): Boolean = macro Ops.unopWithEv2[Eq[A], Boolean]
}

final class AdditiveGroupOps[A: AdditiveGroup](lhs: A) {
  def unary_- : A = macro Ops.unop0[A]
  def -(rhs: A): A = macro Ops.binop[A, A]
  def -(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def -(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def -(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) - rhs
}

final class MultiplicativeSemigroupOps[A: MultiplicativeSemigroup](lhs: A) {
  def *(rhs: A): A = macro Ops.binop[A, A]
  def *(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def *(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def *(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) * rhs
}

final class MultiplicativeMonoidOps[A: MultiplicativeMonoid](lhs: A) {
  def isOne(implicit ev1: Eq[A]): Boolean = macro Ops.unopWithEv2[Eq[A], Boolean]
}

final class MultiplicativeGroupOps[A: MultiplicativeGroup](lhs: A) {
  def reciprocal: A = macro Ops.unop[A]
  def /(rhs: A): A = macro Ops.binop[A, A]
  def /(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def /(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def /(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) / rhs
}

final class SemiringOps[A: Semiring](lhs: A) {
  def pow(rhs: Int): A = macro Ops.binop[Int, A]
  def **(rhs: Int): A = macro Ops.binop[Int, A]
}

final class GCDRingOps[A: GCDRing](lhs: A) {
  def gcd(rhs: A)(implicit ev1: Eq[A]): A = macro Ops.binopWithEv2[A, Eq[A], A]
  def lcm(rhs: A)(implicit ev1: Eq[A]): A = macro Ops.binopWithEv2[A, Eq[A], A]
}

final class EuclideanRingOps[A: EuclideanRing](lhs: A) {
  def euclideanFunction: BigInt = macro Ops.unop[BigInt]
  def equot(rhs: A): A = macro Ops.binop[A, A]
  def emod(rhs: A): A = macro Ops.binop[A, A]
  def equotmod(rhs: A): (A, A) = macro Ops.binop[A, (A, A)]

  // TODO: This is a bit
  def equot(rhs: Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def emod(rhs: Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def equotmod(rhs: Int): (A, A) = macro Ops.binopWithSelfLift[Int, Ring[A], (A, A)]

  def equot(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def emod(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def equotmod(rhs: Double)(implicit ev1: Field[A]): (A, A) = macro Ops.binopWithLift[Double, Field[A], (A, A)]

  /* TODO: move to TruncatedDivision
  def /~(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) /~ rhs
  def %(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) % rhs
  def /%(rhs:Number)(implicit c:ConvertableFrom[A]): (Number, Number) = c.toNumber(lhs) /% rhs
   */
}

final class IsRealOps[A: IsReal](lhs: A) {
  def isWhole: Boolean = macro Ops.unop[Boolean]
  def ceil: A = macro Ops.unop[A]
  def floor: A = macro Ops.unop[A]
  def round: A = macro Ops.unop[A]
  // def toDouble(): Double = macro Ops.unop[Double]
}

final class NRootOps[A](lhs: A)(implicit ev: NRoot[A]) {
  def nroot(rhs: Int): A = macro Ops.binop[Int, A]
  def sqrt: A = macro Ops.unop[A]
  def fpow(rhs: A): A = macro Ops.binop[A, A]

  // TODO: should be macros
  def pow(rhs: Double)(implicit c: Field[A]): A = ev.fpow(lhs, c.fromDouble(rhs))
  def **(rhs: Double)(implicit c: Field[A]): A = ev.fpow(lhs, c.fromDouble(rhs))

  def pow(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs).pow(rhs)
  def **(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) ** rhs
}

final class TrigOps[A](lhs: A)(implicit ev: Trig[A]) {
  def exp: A = macro Ops.unop[A]
  def log: A = macro Ops.unop[A]

  def log(base: Int)(implicit f: Field[A]): A =
    f.div(ev.log(lhs), ev.log(f.fromInt(base)))
}

final class MeetOps[A: MeetSemilattice](lhs: A) {
  def meet(rhs: A): A = macro Ops.binop[A, A]
  def ∧(rhs: A): A = macro Ops.binop[A, A]

  def meet(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def ∧(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
}

final class JoinOps[A: JoinSemilattice](lhs: A) {
  def join(rhs: A): A = macro Ops.binop[A, A]
  def ∨(rhs: A): A = macro Ops.binop[A, A]

  def join(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def ∨(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
}

final class HeytingOps[A: Heyting](lhs: A) {
  def unary_~ : A = macro Ops.unop0[A]
  def imp(rhs: A): A = macro Ops.binop[A, A]

  def &(rhs: A): A = macro Ops.binop[A, A]
  def |(rhs: A): A = macro Ops.binop[A, A]

  def &(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def |(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
}

final class LogicOps[A](lhs: A)(implicit logic: Logic[A]) {
  def unary_! : A = logic.not(lhs)

  def &(rhs: A): A = macro Ops.binop[A, A]
  def |(rhs: A): A = macro Ops.binop[A, A]

  def &(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def |(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
}

final class BoolOps[A: Bool](lhs: A) {
  def ^(rhs: A): A = macro Ops.binop[A, A]
  def nand(rhs: A): A = macro Ops.binop[A, A]
  def nor(rhs: A): A = macro Ops.binop[A, A]
  def nxor(rhs: A): A = macro Ops.binop[A, A]

  def ^(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]

  def ^(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) ^ rhs
}

final class LeftModuleOps[V](x: V) {
  def *:[F](lhs: F)(implicit ev: LeftModule[V, F]): V = macro Ops.rbinopWithEv[F, LeftModule[V, F], V]
  // TODO: Are macros worth it here?
  def *:[F](lhs: Int)(implicit ev: LeftModule[V, F], F: Ring[F]): V = ev.timesl(F.fromInt(lhs), x)
}

final class RightModuleOps[V](x: V) {
  def :*[F](rhs: F)(implicit ev: RightModule[V, F]): V = macro Ops.binopWithEv[F, RightModule[V, F], V]
  // TODO: Are macros worth it here?
  def :*[F](rhs: Int)(implicit ev: RightModule[V, F], F: Ring[F]): V = ev.timesr(x, F.fromInt(rhs))
}

@deprecated("Unbound syntax will be removed", "spire 0.18.0")
final class ModuleUnboundOps[F: ({ type F[A] = CModule[?, A] })#F](lhs: F) {
  def +(rhs: F): F = macro Ops.binopWithScalar[F, F]
  def -(rhs: F): F = macro Ops.binopWithScalar[F, F]
  def unary_- : F = macro Ops.unopWithScalar0[F]

  def *(rhs: F): F = macro Ops.binopWithScalar[F, F]

  def pow(rhs: Int): F = macro Ops.binopWithScalar[Int, F]
  def **(rhs: Int): F = macro Ops.binopWithScalar[Int, F]
}

final class VectorSpaceOps[V](x: V) {
  def :/[F](rhs: F)(implicit ev: VectorSpace[V, F]): V = macro Ops.binopWithEv[F, VectorSpace[V, F], V]

  // def *:[F](lhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesl(ev.scalar.fromDouble(lhs), x)
  // def :*[F](rhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesr(x, ev.scalar.fromDouble(rhs))

  def :/[F](rhs: Int)(implicit ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromInt(rhs))
  def :/[F](rhs: Double)(implicit ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromDouble(rhs))
}

@deprecated("Unbound syntax will be removed", "spire 0.18.0")
final class VectorSpaceUnboundOps[F: ({ type F[A] = VectorSpace[?, A] })#F](lhs: F) {
  def /(rhs: F): F = macro Ops.binopWithScalar[F, F]
  def reciprocal: F = macro Ops.unopWithScalar[F]
}

final class InnerProductSpaceOps[V](lhs: V) {
  def dot[F](rhs: V)(implicit ev: InnerProductSpace[V, F]): F =
    macro Ops.binopWithEv[V, InnerProductSpace[V, F], F]
  def ⋅[F](rhs: V)(implicit ev: InnerProductSpace[V, F]): F =
    macro Ops.binopWithEv[V, InnerProductSpace[V, F], F]
}

final class CoordinateSpaceOps[V](v: V) {
  def _x[F](implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.unopWithEv[CoordinateSpace[V, F], F]

  def _y[F](implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.unopWithEv[CoordinateSpace[V, F], F]

  def _z[F](implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.unopWithEv[CoordinateSpace[V, F], F]

  def coord[F](rhs: Int)(implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.binopWithEv[Int, CoordinateSpace[V, F], F]

  def dimensions[F](implicit ev: CoordinateSpace[V, F]): Int =
    macro Ops.unopWithEv[CoordinateSpace[V, F], Int]
}

final class MetricSpaceOps[V](lhs: V) {
  def distance[F](rhs: V)(implicit ev: MetricSpace[V, F]): F =
    macro Ops.binopWithEv[V, MetricSpace[V, F], F]
}

final class NormedVectorSpaceOps[V](lhs: V) {
  def norm[F](implicit ev: NormedVectorSpace[V, F]): F =
    macro Ops.unopWithEv[NormedVectorSpace[V, F], F]

  def normalize[F](implicit ev: NormedVectorSpace[V, F]): V =
    macro Ops.unopWithEv[NormedVectorSpace[V, F], V]
}

final class ConvertableFromOps[A: ConvertableFrom](lhs: A) {
  override def toString: String = macro Ops.unop[String]
  def toByte: Byte = macro Ops.unop[Byte]
  def toShort: Short = macro Ops.unop[Short]
  def toInt: Int = macro Ops.unop[Int]
  def toLong: Long = macro Ops.unop[Long]
  def toFloat: Float = macro Ops.unop[Float]
  def toDouble: Double = macro Ops.unop[Double]
  def toBigInt: BigInt = macro Ops.unop[BigInt]
  def toBigDecimal: BigDecimal = macro Ops.unop[BigDecimal]
  def toRational: Rational = macro Ops.unop[Rational]
}

final class BitStringOps[A: BitString](lhs: A) {
  def <<(rhs: Int): A = macro Ops.binop[Int, A]
  def >>(rhs: Int): A = macro Ops.binop[Int, A]
  def >>>(rhs: Int): A = macro Ops.binop[Int, A]

  def bitCount: Int = macro Ops.unop[Int]
  def highestOneBit: A = macro Ops.unop[A]
  def lowestOneBit: A = macro Ops.unop[A]
  def numberOfLeadingZeros: Int = macro Ops.unop[Int]
  def numberOfTrailingZeros: Int = macro Ops.unop[Int]

  def toHexString: String = macro Ops.unop[String]

  def rotateLeft(rhs: Int): A = macro Ops.binop[Int, A]
  def rotateRight(rhs: Int): A = macro Ops.binop[Int, A]
}

final class LeftPartialActionOps[G](lhs: G) {
  def ?|+|>[P](rhs: P)(implicit ev: LeftPartialAction[P, G]): Opt[P] =
    macro Ops.binopWithEv[P, LeftPartialAction[P, G], Opt[P]]
  def ??|+|>[P](rhs: P)(implicit ev: LeftPartialAction[P, G]): Boolean =
    macro Ops.binopWithEv[P, LeftPartialAction[P, G], Boolean]
}

final class RightPartialActionOps[P](lhs: P) {
  def <|+|?[G](rhs: G)(implicit ev: RightPartialAction[P, G]): Opt[P] =
    macro Ops.binopWithEv[G, RightPartialAction[P, G], Opt[P]]
  def <|+|??[G](rhs: G)(implicit ev: RightPartialAction[P, G]): Boolean =
    macro Ops.binopWithEv[G, RightPartialAction[P, G], Boolean]
}

final class LeftActionOps[G](lhs: G) {
  def |+|>[P](rhs: P)(implicit ev: LeftAction[P, G]): P =
    macro Ops.binopWithEv[P, Action[P, G], P]
  def +>[P](rhs: P)(implicit ev: AdditiveAction[P, G]): P =
    macro Ops.binopWithEv[P, AdditiveAction[P, G], P]
  def *>[P](rhs: P)(implicit ev: MultiplicativeAction[P, G]): P =
    macro Ops.binopWithEv[P, MultiplicativeAction[P, G], P]
}

final class RightActionOps[P](lhs: P) {
  def <|+|[G](rhs: G)(implicit ev: RightAction[P, G]): P =
    macro Ops.binopWithEv[G, Action[P, G], P]
  def <+[G](rhs: G)(implicit ev: AdditiveAction[P, G]): P =
    macro Ops.binopWithEv[G, AdditiveAction[P, G], P]
  def <*[G](rhs: G)(implicit ev: MultiplicativeAction[P, G]): P =
    macro Ops.binopWithEv[G, MultiplicativeAction[P, G], P]
}

@deprecated("Unbound syntax will be removed", "spire 0.18.0")
final class ActionUnboundOps[G: ({ type F[A] = Action[?, A] })#F](lhs: G) {
  def |+|(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def |-|(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def inverse: G = macro Ops.unopWithScalar[G]
}

@deprecated("Unbound syntax will be removed", "spire 0.18.0")
final class AdditiveActionUnboundOps[G: ({ type F[A] = AdditiveAction[?, A] })#F](lhs: G) {
  def +(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def -(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def unary_- : G = macro Ops.unopWithScalar0[G]
}

@deprecated("Unbound syntax will be removed", "spire 0.18.0")
final class MultiplicativeActionUnboundOps[G: ({ type F[A] = MultiplicativeAction[?, A] })#F](lhs: G) {
  def *(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def /(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def reciprocal: G = macro Ops.unopWithScalar[G]
}

final class TorsorPointOps[P](lhs: P) {
  def <->[G](rhs: P)(implicit ev: AdditiveTorsor[P, G]): G =
    macro Ops.binopWithEv[P, AdditiveTorsor[P, G], G]
  def </>[G](rhs: P)(implicit ev: MultiplicativeTorsor[P, G]): G =
    macro Ops.binopWithEv[P, MultiplicativeTorsor[P, G], G]
}

final class IntervalPointOps[A](lhs: A)(implicit o: Order[A], ev: AdditiveGroup[A]) {
  def ±(rhs: A): Interval[A] =
    Interval(ev.minus(lhs, rhs), ev.plus(lhs, rhs))
  def +/-(rhs: A): Interval[A] =
    Interval(ev.minus(lhs, rhs), ev.plus(lhs, rhs))
}
