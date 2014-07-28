package spire.syntax

import spire.algebra._
import spire.macrosk.Ops
import spire.math.{BitString, ConvertableTo, ConvertableFrom, Rational, Number}

final class EqOps[A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A): Boolean = macro Ops.binop[A, Boolean]
  def =!=(rhs:A): Boolean = macro Ops.binop[A, Boolean]
}

final class PartialOrderOps[A](lhs: A)(implicit ev: PartialOrder[A]) {
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

  def >(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) > rhs
  def >=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) >= rhs
  def <(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) < rhs
  def <=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) <= rhs
}

final class OrderOps[A](lhs: A)(implicit ev: Order[A]) {
  def compare(rhs: A): Int = macro Ops.binop[A, Int]
  def min(rhs: A): A = macro Ops.binop[A, A]
  def max(rhs: A): A = macro Ops.binop[A, A]

  def compare(rhs: Int)(implicit ev1: Ring[A]): Int = macro Ops.binopWithLift[Int, Ring[A], A]
  def min(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def max(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]

  def compare(rhs: Double)(implicit ev1: Field[A]): Int = macro Ops.binopWithLift[Int, Field[A], A]
  def min(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Int, Field[A], A]
  def max(rhs: Double)(implicit ev1: Field[A]): A = macro Ops.binopWithLift[Int, Field[A], A]

  def compare(rhs:Number)(implicit c:ConvertableFrom[A]): Int = c.toNumber(lhs) compare rhs
  def min(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) min rhs
  def max(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) max rhs
}

final class LiteralIntOrderOps(val lhs: Int) extends AnyVal {
  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.lt(c.fromInt(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.lteqv(c.fromInt(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.gt(c.fromInt(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.gteqv(c.fromInt(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Int = ev.compare(c.fromInt(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): A = ev.min(c.fromInt(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): A = ev.max(c.fromInt(lhs), rhs)
}

final class LiteralLongOrderOps(val lhs: Long) extends AnyVal {
  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.lt(c.fromLong(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.lteqv(c.fromLong(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.gt(c.fromLong(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.gteqv(c.fromLong(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Int = ev.compare(c.fromLong(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): A = ev.min(c.fromLong(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): A = ev.max(c.fromLong(lhs), rhs)
}

final class LiteralDoubleOrderOps(val lhs: Double) extends AnyVal {
  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.lt(c.fromDouble(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.lteqv(c.fromDouble(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.gt(c.fromDouble(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Boolean = ev.gteqv(c.fromDouble(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): Int = ev.compare(c.fromDouble(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): A = ev.min(c.fromDouble(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]): A = ev.max(c.fromDouble(lhs), rhs)
}

final class SignedOps[A:Signed](lhs: A) {
  def abs(): A = macro Ops.unop[A]
  def sign(): Sign = macro Ops.unop[Sign]
  def signum(): Int = macro Ops.unop[Int]
  def isZero(): Boolean = macro Ops.unop[Boolean]
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A): A = macro Ops.binop[A, A]
}

final class GroupOps[A](lhs:A)(implicit ev:Group[A]) {
  def inverse(): A = macro Ops.unop[A]
  def |-|(rhs:A): A = macro Ops.binop[A, A]
}

final class AdditiveSemigroupOps[A](lhs:A)(implicit ev:AdditiveSemigroup[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
  def +(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def +(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def +(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) + rhs
}

final class LiteralIntAdditiveSemigroupOps(val lhs: Int) extends AnyVal {
  def +[A](rhs:A)(implicit ev: Ring[A]): A = ev.plus(ev.fromInt(lhs), rhs)
}

final class LiteralLongAdditiveSemigroupOps(val lhs: Long) extends AnyVal {
  def +[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]): A = ev.plus(c.fromLong(lhs), rhs)
}

final class LiteralDoubleAdditiveSemigroupOps(val lhs: Double) extends AnyVal {
  def +[A](rhs:A)(implicit ev:Field[A]): A = ev.plus(ev.fromDouble(lhs), rhs)
}

final class AdditiveGroupOps[A](lhs:A)(implicit ev:AdditiveGroup[A]) {
  def unary_-(): A = macro Ops.unop[A]
  def -(rhs:A): A = macro Ops.binop[A, A]
  def -(rhs:Int)(implicit ev1: Ring[A]): A =  macro Ops.binopWithLift[Int, Ring[A], A]
  def -(rhs:Double)(implicit ev1:Field[A]): A =  macro Ops.binopWithLift[Double, Field[A], A]
  def -(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) - rhs
}

final class LiteralIntAdditiveGroupOps(val lhs: Int) extends AnyVal {
  def -[A](rhs:A)(implicit ev: Ring[A]): A = ev.minus(ev.fromInt(lhs), rhs)
}

final class LiteralLongAdditiveGroupOps(val lhs: Long) extends AnyVal {
  def -[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]): A = ev.minus(c.fromLong(lhs), rhs)
}

final class LiteralDoubleAdditiveGroupOps(val lhs: Double) extends AnyVal {
  def -[A](rhs:A)(implicit ev:Field[A]): A = ev.minus(ev.fromDouble(lhs), rhs)
}

final class MultiplicativeSemigroupOps[A](lhs:A)(implicit ev:MultiplicativeSemigroup[A]) {
  def *(rhs:A): A = macro Ops.binop[A, A]
  def *(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def *(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def *(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) * rhs
}

final class LiteralIntMultiplicativeSemigroupOps(val lhs: Int) extends AnyVal {
  def *[A](rhs:A)(implicit ev: Ring[A]): A = ev.times(ev.fromInt(lhs), rhs)
}

final class LiteralLongMultiplicativeSemigroupOps(val lhs: Long) extends AnyVal {
  def *[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]): A = ev.times(c.fromLong(lhs), rhs)
}

final class LiteralDoubleMultiplicativeSemigroupOps(val lhs: Double) extends AnyVal {
  def *[A](rhs:A)(implicit ev:Field[A]): A = ev.times(ev.fromDouble(lhs), rhs)
}

final class MultiplicativeGroupOps[A](lhs:A)(implicit ev:MultiplicativeGroup[A]) {
  def reciprocal(): A = macro Ops.unop[A]
  def /(rhs:A): A = macro Ops.binop[A, A]
  def /(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def /(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def /(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) / rhs
}

final class LiteralIntMultiplicativeGroupOps(val lhs: Int) extends AnyVal {
  def /[A](rhs:A)(implicit ev: Field[A]): A = ev.div(ev.fromInt(lhs), rhs)
}

final class LiteralLongMultiplicativeGroupOps(val lhs: Long) extends AnyVal {
  def /[A](rhs:A)(implicit ev: Field[A], c:ConvertableTo[A]): A = ev.div(c.fromLong(lhs), rhs)
}

final class LiteralDoubleMultiplicativeGroupOps(val lhs: Double) extends AnyVal {
  def /[A](rhs:A)(implicit ev: Field[A]): A = ev.div(ev.fromDouble(lhs), rhs)
}

final class SemiringOps[A](lhs:A)(implicit ev:Semiring[A]) {
  def pow(rhs:Int): A = macro Ops.binop[Int, A]
  def **(rhs:Int): A = macro Ops.binop[Int, A]
}

final class EuclideanRingOps[A](lhs:A)(implicit ev:EuclideanRing[A]) {
  def /~(rhs:A): A = macro Ops.binop[A, A]
  def %(rhs:A): A = macro Ops.binop[A, A]
  def /%(rhs:A): (A, A) = macro Ops.binop[A, (A, A)]

  def gcd(rhs:A): A = macro Ops.binop[A, A]
  def lcm(rhs:A): A = macro Ops.binop[A, A]

  // TODO: This is a bit
  def /~(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def %(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def /%(rhs:Int): (A, A) = macro Ops.binopWithSelfLift[Int, Ring[A], (A, A)]

  def /~(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def %(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def /%(rhs:Double)(implicit ev1:Field[A]): (A, A) = macro Ops.binopWithLift[Double, Field[A], (A, A)]

  def /~(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) /~ rhs
  def %(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) % rhs
  def /%(rhs:Number)(implicit c:ConvertableFrom[A]): (Number, Number) = c.toNumber(lhs) /% rhs
}

final class LiteralIntEuclideanRingOps(val lhs: Int) extends AnyVal {
  def /~[A](rhs:A)(implicit ev: EuclideanRing[A]): A = ev.quot(ev.fromInt(lhs), rhs)
  def %[A](rhs:A)(implicit ev: EuclideanRing[A]): A = ev.mod(ev.fromInt(lhs), rhs)
  def /%[A](rhs:A)(implicit ev: EuclideanRing[A]): (A, A) = ev.quotmod(ev.fromInt(lhs), rhs)
}

final class LiteralLongEuclideanRingOps(val lhs: Long) extends AnyVal {
  def /~[A](rhs:A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]): A = ev.quot(c.fromLong(lhs), rhs)
  def %[A](rhs:A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]): A = ev.mod(c.fromLong(lhs), rhs)
  def /%[A](rhs:A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]): (A, A) = ev.quotmod(c.fromLong(lhs), rhs)
}

final class LiteralDoubleEuclideanRingOps(val lhs: Double) extends AnyVal {
  def /~[A](rhs:A)(implicit ev: Field[A]): A = ev.quot(ev.fromDouble(lhs), rhs)
  def %[A](rhs:A)(implicit ev: Field[A]): A = ev.mod(ev.fromDouble(lhs), rhs)
  def /%[A](rhs:A)(implicit ev: Field[A]): (A, A) = ev.quotmod(ev.fromDouble(lhs), rhs)
}

final class IsRealOps[A](lhs:A)(implicit ev:IsReal[A]) {
  def isWhole(): Boolean = macro Ops.unop[Boolean]
  def ceil(): A = macro Ops.unop[A]
  def floor(): A = macro Ops.unop[A]
  def round(): A = macro Ops.unop[A]
  //def toDouble(): Double = macro Ops.unop[Double]
}

final class NRootOps[A](lhs: A)(implicit ev: NRoot[A]) {
  def nroot(rhs: Int): A = macro Ops.binop[Int, A]
  def sqrt(): A = macro Ops.unop[A]
  def fpow(rhs: A): A = macro Ops.binop[A, A]

  // TODO: should be macros
  def pow(rhs: Double)(implicit c: Field[A]): A = ev.fpow(lhs, c.fromDouble(rhs))
  def **(rhs: Double)(implicit c: Field[A]): A = ev.fpow(lhs, c.fromDouble(rhs))

  def pow(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) pow rhs
  def **(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) ** rhs
}

final class LiteralIntNRootOps(val lhs: Int) extends AnyVal {
  def **[A](rhs:A)(implicit ev: NRoot[A], c:ConvertableTo[A]): A = ev.fpow(c.fromLong(lhs), rhs)
}

final class LiteralLongNRootOps(val lhs: Long) extends AnyVal {
  def **[A](rhs:A)(implicit ev: NRoot[A], c:ConvertableTo[A]): A = ev.fpow(c.fromLong(lhs), rhs)
}

final class LiteralDoubleNRootOps(val lhs: Double) extends AnyVal {
  def **[A](rhs:A)(implicit ev: NRoot[A], c:ConvertableTo[A]): A = ev.fpow(c.fromDouble(lhs), rhs)
}

final class TrigOps[A](lhs: A)(implicit ev: Trig[A]) {
  def exp(): A = macro Ops.unop[A]
  def log(): A = macro Ops.unop[A]

  def log(base: Int)(implicit f: Field[A]): A =
    f.div(ev.log(lhs), ev.log(f.fromInt(base)))
}

final class HeytingOps[A: Heyting](lhs:A) {
  def unary_~(): A = macro Ops.unop[A]
  def &(rhs: A): A = macro Ops.binop[A, A]
  def |(rhs: A): A = macro Ops.binop[A, A]
  def imp(rhs: A): A = macro Ops.binop[A, A]

  def &(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def |(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]

  def &(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) & rhs
  def |(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) | rhs
}

final class BoolOps[A: Bool](lhs:A) {
  def ^(rhs: A): A = macro Ops.binop[A, A]
  def nand(rhs: A): A = macro Ops.binop[A, A]
  def nor(rhs: A): A = macro Ops.binop[A, A]
  def nxor(rhs: A): A = macro Ops.binop[A, A]

  def ^(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]

  def ^(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) ^ rhs
}

final class ModuleOps[V](x: V) {
  def *:[F](lhs:F)(implicit ev: Module[V, F]): V = macro Ops.rbinopWithEv[F, Module[V, F], V]
  def :*[F](rhs:F)(implicit ev: Module[V, F]): V = macro Ops.binopWithEv[F, Module[V, F], V]

  // TODO: Are macros worth it here?
  def *:[F](lhs:Int)(implicit ev: Module[V, F], F: Ring[F]): V = ev.timesl(F.fromInt(lhs), x)
  def :*[F](rhs:Int)(implicit ev: Module[V, F], F: Ring[F]): V = ev.timesr(x, F.fromInt(rhs))
}

final class ModuleUnboundOps[F](lhs: F)(implicit ev: Module[_, F]) {
  def +(rhs: F): F = macro Ops.binopWithScalar[F, F]
  def -(rhs: F): F = macro Ops.binopWithScalar[F, F]
  def unary_-(): F = macro Ops.unopWithScalar[F]
  
  def *(rhs: F): F = macro Ops.binopWithScalar[F, F]
  
  def pow(rhs: Int): F = macro Ops.binopWithScalar[Int, F]
  def **(rhs: Int): F = macro Ops.binopWithScalar[Int, F]
}

final class VectorSpaceOps[V](x: V) {
  def :/[F](rhs:F)(implicit ev: VectorSpace[V, F]): V = macro Ops.binopWithEv[F, VectorSpace[V, F], V]

  //def *:[F](lhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesl(ev.scalar.fromDouble(lhs), x)
  //def :*[F](rhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesr(x, ev.scalar.fromDouble(rhs))

  def :/[F](rhs:Int)(implicit ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromInt(rhs))
  def :/[F](rhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromDouble(rhs))
}

final class VectorSpaceUnboundOps[F](lhs: F)(implicit ev: VectorSpace[_, F]) {
  def /(rhs: F): F = macro Ops.binopWithScalar[F, F]
  def reciprocal(): F = macro Ops.unopWithScalar[F]
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

final class ConvertableFromOps[A](lhs:A)(implicit ev:ConvertableFrom[A]) {
  override def toString(): String = macro Ops.unop[String]
  def toByte(): Byte = macro Ops.unop[Byte]
  def toShort(): Short = macro Ops.unop[Short]
  def toInt(): Int = macro Ops.unop[Int]
  def toLong(): Long = macro Ops.unop[Long]
  def toFloat(): Float = macro Ops.unop[Float]
  def toDouble(): Double = macro Ops.unop[Double]
  def toBigInt(): BigInt = macro Ops.unop[BigInt]
  def toBigDecimal(): BigDecimal = macro Ops.unop[BigDecimal]
  def toRational(): Rational = macro Ops.unop[Rational]
}

final class BitStringOps[A](lhs: A)(implicit ev: BitString[A]) {
  def <<(rhs: Int): A = macro Ops.binop[Int, A]
  def >>(rhs: Int): A = macro Ops.binop[Int, A]
  def >>>(rhs: Int): A = macro Ops.binop[Int, A]

  def bitCount(): Int = macro Ops.unop[Int]
  def highestOneBit(): A = macro Ops.unop[A]
  def lowestOneBit(): A = macro Ops.unop[A]
  def numberOfLeadingZeros(): Int = macro Ops.unop[Int]
  def numberOfTrailingZeros(): Int = macro Ops.unop[Int]

  def toHexString(): String = macro Ops.unop[String]

  def rotateLeft(rhs: Int): A = macro Ops.binop[Int, A]
  def rotateRight(rhs: Int): A = macro Ops.binop[Int, A]
}

final class GroupActionGroupOps[G](lhs: G) {
  def |+|> [P](rhs: P)(implicit ev: GroupAction[P, G]): P =
    macro Ops.binopWithEv[P, GroupAction[P, G], P]
  def +> [P](rhs: P)(implicit ev: AdditiveGroupAction[P, G]): P =
    macro Ops.binopWithEv[P, AdditiveGroupAction[P, G], P]
  def *> [P](rhs: P)(implicit ev: MultiplicativeGroupAction[P, G]): P =
    macro Ops.binopWithEv[P, MultiplicativeGroupAction[P, G], P]
}

final class GroupActionPointOps[P](lhs: P) {
  def <|+| [G](rhs: G)(implicit ev: GroupAction[P, G]): P =
    macro Ops.binopWithEv[G, GroupAction[P, G], P]
  def <+ [G](rhs: G)(implicit ev: AdditiveGroupAction[P, G]): P =
    macro Ops.binopWithEv[G, AdditiveGroupAction[P, G], P]
  def <* [G](rhs: G)(implicit ev: MultiplicativeGroupAction[P, G]): P =
    macro Ops.binopWithEv[G, MultiplicativeGroupAction[P, G], P]
}

final class GroupActionUnboundOps[G](lhs: G)(implicit ev: GroupAction[_, G]) {
  def |+|(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def |-|(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def inverse(): G = macro Ops.unopWithScalar[G]
}

final class AdditiveGroupActionUnboundOps[G](lhs: G)(implicit ev: AdditiveGroupAction[_, G]) {
  def +(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def -(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def unary_-(): G = macro Ops.unopWithScalar[G]
}

final class MultiplicativeGroupActionUnboundOps[G](lhs: G)(implicit ev: MultiplicativeGroupAction[_, G]) {
  def *(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def /(rhs: G): G = macro Ops.binopWithScalar[G, G]
  def reciprocal(): G = macro Ops.unopWithScalar[G]
}

final class TorsorPointOps[P](lhs: P) {
  def <-> [G](rhs: P)(implicit ev: AdditiveTorsor[P, G]): G =
    macro Ops.binopWithEv[P, AdditiveTorsor[P, G], G]
  def </> [G](rhs: P)(implicit ev: MultiplicativeTorsor[P, G]): G =
    macro Ops.binopWithEv[P, MultiplicativeTorsor[P, G], G]
}
