package spire
package syntax

import spire.algebra._
import spire.algebra.partial._
import spire.algebra.lattice._
import spire.math._
import spire.util.Opt

final class EqOps[A](lhs: A)(using ev: Eq[A]):
  def ===[B](rhs: B)(using ev1: B =:= A): Boolean = ev.eqv(lhs, ev1(rhs))
  def =!=[B](rhs: B)(using ev1: B =:= A): Boolean = ev.neqv(lhs, ev1(rhs))

final class PartialOrderOps[A](lhs: A)(using ev: PartialOrder[A]):
  def >(rhs: A): Boolean = ev.gt(lhs, rhs)
  def >=(rhs: A): Boolean = ev.gteqv(lhs, rhs)
  def <(rhs: A): Boolean = ev.lt(lhs, rhs)
  def <=(rhs: A): Boolean = ev.lteqv(lhs, rhs)

  def partialCompare(rhs: A): Double = ev.partialCompare(lhs, rhs)
  def tryCompare(rhs: A): Option[Int] = ev.tryCompare(lhs, rhs)
  def pmin(rhs: A): Option[A] = ev.pmin(lhs, rhs)
  def pmax(rhs: A): Option[A] = ev.pmax(lhs, rhs)

  def >(rhs: Int)(using ev1: Ring[A]): Boolean = ev.gt(lhs, ev1.fromInt(rhs))
  def >=(rhs: Int)(using ev1: Ring[A]): Boolean = ev.gteqv(lhs, ev1.fromInt(rhs))
  def <(rhs: Int)(using ev1: Ring[A]): Boolean = ev.lt(lhs, ev1.fromInt(rhs))
  def <=(rhs: Int)(using ev1: Ring[A]): Boolean = ev.lteqv(lhs, ev1.fromInt(rhs))

  def >(rhs: Double)(using ev1: Field[A]): Boolean = ev.gt(lhs, ev1.fromDouble(rhs))
  def >=(rhs: Double)(using ev1: Field[A]): Boolean = ev.gteqv(lhs, ev1.fromDouble(rhs))
  def <(rhs: Double)(using ev1: Field[A]): Boolean = ev.lt(lhs, ev1.fromDouble(rhs))
  def <=(rhs: Double)(using ev1: Field[A]): Boolean = ev.lteqv(lhs, ev1.fromDouble(rhs))

  def >(rhs: Number)(using c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) > rhs
  def >=(rhs: Number)(using c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) >= rhs
  def <(rhs: Number)(using c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) < rhs
  def <=(rhs: Number)(using c: ConvertableFrom[A]): Boolean = c.toNumber(lhs) <= rhs

final class OrderOps[A](lhs: A)(using o: Order[A]):
  def compare(rhs: A): Int = o.compare(lhs, rhs)
  def min(rhs: A): A = o.min(lhs, rhs)
  def max(rhs: A): A = o.max(lhs, rhs)

  def compare(rhs: Int)(using ev1: Ring[A]): Int = o.compare(lhs, ev1.fromInt(rhs))
  def min(rhs: Int)(using ev1: Ring[A]): A = o.min(lhs, ev1.fromInt(rhs))
  def max(rhs: Int)(using ev1: Ring[A]): A = o.max(lhs, ev1.fromInt(rhs))

  def compare(rhs: Double)(using ev1: Field[A]): Int = o.compare(lhs, ev1.fromDouble(rhs))
  def min(rhs: Double)(using ev1: Field[A]): A = o.min(lhs, ev1.fromDouble(rhs))
  def max(rhs: Double)(using ev1: Field[A]): A = o.max(lhs, ev1.fromDouble(rhs))

  def compare(rhs: Number)(using c: ConvertableFrom[A]): Int = c.toNumber(lhs).compare(rhs)
  def min(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs).min(rhs)
  def max(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs).max(rhs)

final class SignedOps[A](a: A)(using s: Signed[A]):
  def abs: A = s.abs(a)
  def sign: Sign = s.sign(a)
  def signum: Int = s.signum(a)

  def isSignZero: Boolean = s.isSignZero(a)
  def isSignPositive: Boolean = s.isSignPositive(a)
  def isSignNegative: Boolean = s.isSignNegative(a)

  def isSignNonZero: Boolean = s.isSignNonZero(a)
  def isSignNonPositive: Boolean = s.isSignNonPositive(a)
  def isSignNonNegative: Boolean = s.isSignNonNegative(a)

final class TruncatedDivisionOps[A](lhs: A)(using ev: TruncatedDivision[A]):
  def toBigIntOpt: Opt[BigInt] = ev.toBigIntOpt(lhs)
  def tquot(rhs: A): A = ev.tquot(lhs, rhs)
  def tmod(rhs: A): A = ev.tmod(lhs, rhs)
  def tquotmod(rhs: A): (A, A) = ev.tquotmod(lhs, rhs)

  def fquot(rhs: A): A = ev.fquot(lhs, rhs)
  def fmod(rhs: A): A = ev.fmod(lhs, rhs)
  def fquotmod(rhs: A): (A, A) = ev.fquotmod(lhs, rhs)

final class InvolutionOps[A](lhs: A)(using ev: Involution[A]):
  def adjoint: A = ev.adjoint(lhs)

final class IsRealOps[A](lhs: A)(using is: IsReal[A]):
  def isWhole: Boolean = is.isWhole(lhs)
  def ceil: A = is.ceil(lhs)
  def floor: A = is.floor(lhs)
  def round: A = is.round(lhs)

final class SemigroupoidOps[A](lhs: A)(using ev: Semigroupoid[A]):
  def |+|?(rhs: A): Opt[A] = ev.partialOp(lhs, rhs)
  def |+|??(rhs: A): Boolean = ev.opIsDefined(lhs, rhs)

final class GroupoidOps[A](lhs: A)(using ev: Groupoid[A]):
  def leftId: A = ev.leftId(lhs)
  def rightId: A = ev.rightId(lhs)
  def |-|?(rhs: A): Opt[A] = ev.partialOpInverse(lhs, rhs)
  def |-|??(rhs: A): Boolean = ev.opInverseIsDefined(lhs, rhs)

final class SemigroupOps[A](lhs: A)(using ev: Semigroup[A]):
  def |+|(rhs: A): A = ev.combine(lhs, rhs)

final class MonoidOps[A](lhs: A)(using ev: Monoid[A]):
  def isEmpty(using ev1: Eq[A]): Boolean = ev.isEmpty(lhs)

final class GroupOps[A](lhs: A)(using ev: Group[A]):
  def inverse: A = ev.inverse(lhs)
  def |-|(rhs: A): A = ev.remove(lhs, rhs)

final class AdditiveSemigroupOps[A](lhs: A)(using ev: AdditiveSemigroup[A]):
  def +(rhs: A): A = ev.plus(lhs, rhs)
  def +(rhs: Int)(using ev1: Ring[A]): A = ev.plus(lhs, ev1.fromInt(rhs))
  def +(rhs: Double)(using ev1: Field[A]): A = ev.plus(lhs, ev1.fromDouble(rhs))
  def +(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs) + rhs

final class AdditiveMonoidOps[A](lhs: A)(using ev: AdditiveMonoid[A]):
  def isZero(using ev1: Eq[A]): Boolean = ev.isZero(lhs)

final class AdditiveGroupOps[A](lhs: A)(using ev: AdditiveGroup[A]):
  def unary_- : A = ev.negate(lhs)
  def -(rhs: A): A = ev.minus(lhs, rhs)
  def -(rhs: Int)(using ev1: Ring[A]): A = ev.minus(lhs, ev1.fromInt(rhs))
  def -(rhs: Double)(using ev1: Field[A]): A = ev.minus(lhs, ev1.fromDouble(rhs))
  def -(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs) - rhs

final class MultiplicativeSemigroupOps[A](lhs: A)(using ev: MultiplicativeSemigroup[A]):
  def *(rhs: A): A = ev.times(lhs, rhs)
  def *(rhs: Int)(using ev1: Ring[A]): A = ev.times(lhs, ev1.fromInt(rhs))
  def *(rhs: Double)(using ev1: Field[A]): A = ev.times(lhs, ev1.fromDouble(rhs))
  def *(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs) * rhs

final class MultiplicativeMonoidOps[A](lhs: A)(using ev: MultiplicativeMonoid[A]):
  def isOne(using ev1: Eq[A]): Boolean = ev.isOne(lhs)

final class MultiplicativeGroupOps[A](lhs: A)(using ev: MultiplicativeGroup[A]):
  def reciprocal: A = ev.reciprocal(lhs)
  def /(rhs: A): A = ev.div(lhs, rhs)
  def /(rhs: Int)(using ev1: Ring[A]): A = ev.div(lhs, ev1.fromInt(rhs))
  def /(rhs: Double)(using ev1: Field[A]): A = ev.div(lhs, ev1.fromDouble(rhs))
  def /(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs) / rhs

final class SemiringOps[A](lhs: A)(using ev: Semiring[A]):
  def pow(rhs: Int): A = ev.pow(lhs, rhs)
  def **(rhs: Int): A = pow(rhs)

final class GCDRingOps[A](lhs: A)(using ev: GCDRing[A]):
  def gcd(rhs: A)(using Eq[A]): A = ev.gcd(lhs, rhs)
  def lcm(rhs: A)(using Eq[A]): A = ev.lcm(lhs, rhs)

final class EuclideanRingOps[A](lhs: A)(using ev: EuclideanRing[A]):
  def euclideanFunction: BigInt = ev.euclideanFunction(lhs)
  def equot(rhs: A): A = ev.equot(lhs, rhs)
  def emod(rhs: A): A = ev.emod(lhs, rhs)
  def equotmod(rhs: A): (A, A) = ev.equotmod(lhs, rhs)
  // Added typeclasses
  def equot(rhs: Int)(using ev1: Ring[A]): A = ev.equot(lhs, ev1.fromInt(rhs))
  def emod(rhs: Int)(using ev1: Ring[A]): A = ev.emod(lhs, ev1.fromInt(rhs))
  def equotmod(rhs: Int)(using ev1: Ring[A]): (A, A) = ev.equotmod(lhs, ev1.fromInt(rhs))
  //
  def equot(rhs: Double)(using ev1: Field[A]): A = ev.equot(lhs, ev1.fromDouble(rhs))
  def emod(rhs: Double)(using ev1: Field[A]): A = ev.emod(lhs, ev1.fromDouble(rhs))
  def equotmod(rhs: Double)(using ev1: Field[A]): (A, A) = ev.equotmod(lhs, ev1.fromDouble(rhs))

final class NRootOps[A](lhs: A)(using ev: NRoot[A]):
  def nroot(rhs: Int): A = ev.nroot(lhs, rhs)
  def sqrt: A = ev.sqrt(lhs)
  def fpow(rhs: A): A = ev.fpow(lhs, rhs)

  def pow(rhs: Double)(using c: Field[A]): A = ev.fpow(lhs, c.fromDouble(rhs))
  def **(rhs: Double)(using c: Field[A]): A = ev.fpow(lhs, c.fromDouble(rhs))

  def pow(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs).pow(rhs)
  def **(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs) ** rhs

final class TrigOps[A](lhs: A)(implicit ev: Trig[A]):
  def exp: A = ev.exp(lhs)
  def log: A = ev.log(lhs)

  def log(base: Int)(using f: Field[A]): A =
    f.div(ev.log(lhs), ev.log(f.fromInt(base)))

final class MeetOps[A](lhs: A)(using ev: MeetSemilattice[A]):
  def meet(rhs: A): A = ev.meet(lhs, rhs)
  def ∧(rhs: A): A = ev.meet(lhs, rhs)

  def meet(rhs: Int)(using ev1: Ring[A]): A = ev.meet(lhs, ev1.fromInt(rhs))
  def ∧(rhs: Int)(using ev1: Ring[A]): A = ev.meet(lhs, ev1.fromInt(rhs))

final class JoinOps[A](lhs: A)(using ev: JoinSemilattice[A]):
  def join(rhs: A): A = ev.join(lhs, rhs)
  def ∨(rhs: A): A = ev.join(lhs, rhs)

  def join(rhs: Int)(using ev1: Ring[A]): A = ev.join(lhs, ev1.fromInt(rhs))
  def ∨(rhs: Int)(using ev1: Ring[A]): A = ev.join(lhs, ev1.fromInt(rhs))

final class HeytingOps[A](lhs: A)(using ev: Heyting[A]):
  def unary_~ : A = ev.complement(lhs)
  def imp(rhs: A): A = ev.imp(lhs, rhs)

  def &(rhs: A): A = ev.and(lhs, rhs)
  def |(rhs: A): A = ev.or(lhs, rhs)

  def &(rhs: Int)(using ev1: Ring[A]): A = ev.and(lhs, ev1.fromInt(rhs))
  def |(rhs: Int)(using ev1: Ring[A]): A = ev.or(lhs, ev1.fromInt(rhs))

final class LogicOps[A](lhs: A)(using ev: Logic[A]):
  def unary_! : A = ev.not(lhs)

  def &(rhs: A): A = ev.and(lhs, rhs)
  def |(rhs: A): A = ev.or(lhs, rhs)

  def &(rhs: Int)(using ev1: Ring[A]): A = ev.and(lhs, ev1.fromInt(rhs))
  def |(rhs: Int)(using ev1: Ring[A]): A = ev.or(lhs, ev1.fromInt(rhs))

final class BoolOps[A: Bool](lhs: A)(using ev: Bool[A]):
  def ^(rhs: A): A = ev.xor(lhs, rhs)
  def nand(rhs: A): A = ev.nand(lhs, rhs)
  def nor(rhs: A): A = ev.nor(lhs, rhs)
  def nxor(rhs: A): A = ev.nxor(lhs, rhs)

  def ^(rhs: Int)(using ev1: Ring[A]): A = ev.xor(lhs, ev1.fromInt(rhs))
  def ^(rhs: Number)(using c: ConvertableFrom[A]): Number = c.toNumber(lhs) ^ rhs

final class BitStringOps[A](lhs: A)(using ev: BitString[A]):
  def <<(rhs: Int): A = ev.leftShift(lhs, rhs)
  def >>(rhs: Int): A = ev.signedRightShift(lhs, rhs)
  def >>>(rhs: Int): A = ev.rightShift(lhs, rhs)

  def bitCount: Int = ev.bitCount(lhs)
  def highestOneBit: A = ev.highestOneBit(lhs)
  def lowestOneBit: A = ev.lowestOneBit(lhs)
  def numberOfLeadingZeros: Int = ev.numberOfLeadingZeros(lhs)
  def numberOfTrailingZeros: Int = ev.numberOfTrailingZeros(lhs)

  def toHexString: String = ev.toHexString(lhs)

  def rotateLeft(rhs: Int): A = ev.rotateLeft(lhs, rhs)
  def rotateRight(rhs: Int): A = ev.rotateRight(lhs, rhs)

final class LeftPartialActionOps[G](lhs: G) extends AnyVal:
  def ?|+|>[P](rhs: P)(using ev: LeftPartialAction[P, G]): Opt[P] =
    ev.partialActl(lhs, rhs)
  def ??|+|>[P](rhs: P)(using ev: LeftPartialAction[P, G]): Boolean =
    ev.actlIsDefined(lhs, rhs)

final class RightPartialActionOps[P](lhs: P) extends AnyVal:
  def <|+|?[G](rhs: G)(using ev: RightPartialAction[P, G]): Opt[P] =
    ev.partialActr(lhs, rhs)
  def <|+|??[G](rhs: G)(using ev: RightPartialAction[P, G]): Boolean =
    ev.actrIsDefined(lhs, rhs)

final class LeftActionOps[G](lhs: G) extends AnyVal:
  def |+|>[P](rhs: P)(using ev: LeftAction[P, G]): P =
    ev.actl(lhs, rhs)
  def +>[P](rhs: P)(using ev: AdditiveAction[P, G]): P =
    ev.gplusl(lhs ,rhs)
  def *>[P](rhs: P)(using ev: MultiplicativeAction[P, G]): P =
    ev.gtimesl(lhs, rhs)

final class RightActionOps[P](lhs: P) extends AnyVal:
  def <|+|[G](rhs: G)(using ev: RightAction[P, G]): P =
    ev.actr(lhs, rhs)
  def <+[G](rhs: G)(using ev: AdditiveAction[P, G]): P =
    ev.gplusr(lhs ,rhs)
  def <*[G](rhs: G)(using ev: MultiplicativeAction[P, G]): P =
    ev.gtimesr(lhs, rhs)

final class LeftModuleOps[V](x: V) extends AnyVal:
  def *:[F](lhs: F)(using ev: LeftModule[V, F]): V = ev.timesl(lhs, x)
  def *:[F](lhs: Int)(using ev: LeftModule[V, F], F: Ring[F]): V = ev.timesl(F.fromInt(lhs), x)

final class RightModuleOps[V](x: V) extends AnyVal:
  def :*[F](rhs: F)(using ev: RightModule[V, F]): V = ev.timesr(x, rhs)
  def :*[F](rhs: Int)(using ev: RightModule[V, F], F: Ring[F]): V = ev.timesr(x, F.fromInt(rhs))

final class VectorSpaceOps[V](x: V) extends AnyVal:
  def :/[F](rhs: F)(using ev: VectorSpace[V, F]): V = ev.divr(x, rhs)

  //def *:[F](lhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesl(ev.scalar.fromDouble(lhs), x)
  //def :*[F](rhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesr(x, ev.scalar.fromDouble(rhs))

  def :/[F](rhs: Int)(using ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromInt(rhs))
  def :/[F](rhs: Double)(using ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromDouble(rhs))

final class InnerProductSpaceOps[V](lhs: V) extends AnyVal:
  def dot[F](rhs: V)(using ev: InnerProductSpace[V, F]): F =
    ev.dot(lhs, rhs)
  def ⋅[F](rhs: V)(using ev: InnerProductSpace[V, F]): F =
    ev.dot(lhs, rhs)

final class CoordinateSpaceOps[V](v: V) extends AnyVal:
  def _x[F](using ev: CoordinateSpace[V, F]): F =
    ev._x(v)

  def _y[F](using ev: CoordinateSpace[V, F]): F =
    ev._y(v)

  def _z[F](using ev: CoordinateSpace[V, F]): F =
    ev._z(v)

  def coord[F](rhs: Int)(using ev: CoordinateSpace[V, F]): F =
    ev.coord(v, rhs)

  def dimensions[F](using ev: CoordinateSpace[V, F]): Int =
    ev.dimensions

final class MetricSpaceOps[V](lhs: V) extends AnyVal:
  def distance[F](rhs: V)(using ev: MetricSpace[V, F]): F =
    ev.distance(lhs, rhs)

final class NormedVectorSpaceOps[V](lhs: V) extends AnyVal:
  def norm[F](using ev: NormedVectorSpace[V, F]): F =
    ev.norm(lhs)

  def normalize[F](using ev: NormedVectorSpace[V, F]): V =
    ev.normalize(lhs)

final class IntervalPointOps[A](lhs: A)(using o: Order[A], ev: AdditiveGroup[A]):
  def ±(rhs: A): Interval[A] =
    Interval(ev.minus(lhs, rhs), ev.plus(lhs, rhs))
  def +/-(rhs: A): Interval[A] =
    Interval(ev.minus(lhs, rhs), ev.plus(lhs, rhs))

final class TorsorPointOps[P](lhs: P) extends AnyVal:
  def <->[G](rhs: P)(using ev: AdditiveTorsor[P, G]): G =
    ev.pminus(lhs, rhs)
  def </>[G](rhs: P)(using ev: MultiplicativeTorsor[P, G]): G =
    ev.pdiv(lhs, rhs)
