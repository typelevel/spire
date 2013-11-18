package spire.syntax

import spire.algebra._
import spire.macrosk.Ops
import spire.math.{BitString, ConvertableTo, ConvertableFrom, Rational, Number}

final class EqOps[A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A) = macro Ops.binop[A, Boolean]
  def =!=(rhs:A) = macro Ops.binop[A, Boolean]
}

final class OrderOps[A](lhs: A)(implicit ev: Order[A]) {
  def >(rhs: A) = macro Ops.binop[A, Boolean]
  def >=(rhs: A) = macro Ops.binop[A, Boolean]
  def <(rhs: A) = macro Ops.binop[A, Boolean]
  def <=(rhs: A) = macro Ops.binop[A, Boolean]
  def compare(rhs: A) = macro Ops.binop[A, Int]
  def min(rhs: A) = macro Ops.binop[A, A]
  def max(rhs: A) = macro Ops.binop[A, A]

  def >(rhs: Int)(implicit ev1: Ring[A]) = macro Ops.binopWithLift[Int, Ring[A], A]
  def >=(rhs: Int)(implicit ev1: Ring[A]) = macro Ops.binopWithLift[Int, Ring[A], A]
  def <(rhs: Int)(implicit ev1: Ring[A]) = macro Ops.binopWithLift[Int, Ring[A], A]
  def <=(rhs: Int)(implicit ev1: Ring[A]) = macro Ops.binopWithLift[Int, Ring[A], A]
  def compare(rhs: Int)(implicit ev1: Ring[A]) = macro Ops.binopWithLift[Int, Ring[A], A]
  def min(rhs: Int)(implicit ev1: Ring[A]) = macro Ops.binopWithLift[Int, Ring[A], A]
  def max(rhs: Int)(implicit ev1: Ring[A]) = macro Ops.binopWithLift[Int, Ring[A], A]

  def >(rhs: Double)(implicit ev1: Field[A]) = macro Ops.binopWithLift[Int, Field[A], A]
  def >=(rhs: Double)(implicit ev1: Field[A]) = macro Ops.binopWithLift[Int, Field[A], A]
  def <(rhs: Double)(implicit ev1: Field[A]) = macro Ops.binopWithLift[Int, Field[A], A]
  def <=(rhs: Double)(implicit ev1: Field[A]) = macro Ops.binopWithLift[Int, Field[A], A]
  def compare(rhs: Double)(implicit ev1: Field[A]) = macro Ops.binopWithLift[Int, Field[A], A]
  def min(rhs: Double)(implicit ev1: Field[A]) = macro Ops.binopWithLift[Int, Field[A], A]
  def max(rhs: Double)(implicit ev1: Field[A]) = macro Ops.binopWithLift[Int, Field[A], A]

  def >(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) > rhs
  def >=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) >= rhs
  def <(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) < rhs
  def <=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) <= rhs
  def compare(rhs:Number)(implicit c:ConvertableFrom[A]): Int = c.toNumber(lhs) compare rhs
  def min(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) min rhs
  def max(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) max rhs
}

final class LiteralIntOrderOps(val lhs: Int) extends AnyVal {
  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lt(c.fromInt(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lteqv(c.fromInt(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gt(c.fromInt(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gteqv(c.fromInt(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.compare(c.fromInt(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.min(c.fromInt(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.max(c.fromInt(lhs), rhs)
}

final class LiteralLongOrderOps(val lhs: Long) extends AnyVal {
  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lt(c.fromLong(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lteqv(c.fromLong(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gt(c.fromLong(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gteqv(c.fromLong(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.compare(c.fromLong(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.min(c.fromLong(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.max(c.fromLong(lhs), rhs)
}

final class LiteralDoubleOrderOps(val lhs: Double) extends AnyVal {
  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lt(c.fromDouble(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lteqv(c.fromDouble(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gt(c.fromDouble(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gteqv(c.fromDouble(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.compare(c.fromDouble(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.min(c.fromDouble(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.max(c.fromDouble(lhs), rhs)
}

final class SignedOps[A:Signed](lhs: A) {
  def abs(): A = macro Ops.unop[A]
  def sign(): Sign = macro Ops.unop[Sign]
  def signum(): Int = macro Ops.unop[Int]
  def isZero(): Boolean = macro Ops.unop[Boolean]
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = macro Ops.binop[A, A]
}

final class GroupOps[A](lhs:A)(implicit ev:Group[A]) {
  def inverse() = macro Ops.unop[A]
  def |-|(rhs:A) = macro Ops.binop[A, A]
}

final class AdditiveSemigroupOps[A](lhs:A)(implicit ev:AdditiveSemigroup[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
  def +(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def +(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def +(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) + rhs
}

final class LiteralIntAdditiveSemigroupOps(val lhs: Int) extends AnyVal {
  def +[A](rhs:A)(implicit ev: Ring[A]) = ev.plus(ev.fromInt(lhs), rhs)
}

final class LiteralLongAdditiveSemigroupOps(val lhs: Long) extends AnyVal {
  def +[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]) = ev.plus(c.fromLong(lhs), rhs)
}

final class LiteralDoubleAdditiveSemigroupOps(val lhs: Double) extends AnyVal {
  def +[A](rhs:A)(implicit ev:Field[A]) = ev.plus(ev.fromDouble(lhs), rhs)
}

final class AdditiveGroupOps[A](lhs:A)(implicit ev:AdditiveGroup[A]) {
  def unary_-() = macro Ops.unop[A]
  def -(rhs:A): A = macro Ops.binop[A, A]
  def -(rhs:Int)(implicit ev1: Ring[A]): A =  macro Ops.binopWithLift[Int, Ring[A], A]
  def -(rhs:Double)(implicit ev1:Field[A]): A =  macro Ops.binopWithLift[Double, Field[A], A]
  def -(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) - rhs
}

final class LiteralIntAdditiveGroupOps(val lhs: Int) extends AnyVal {
  def -[A](rhs:A)(implicit ev: Ring[A]) = ev.minus(ev.fromInt(lhs), rhs)
}

final class LiteralLongAdditiveGroupOps(val lhs: Long) extends AnyVal {
  def -[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]) = ev.minus(c.fromLong(lhs), rhs)
}

final class LiteralDoubleAdditiveGroupOps(val lhs: Double) extends AnyVal {
  def -[A](rhs:A)(implicit ev:Field[A]) = ev.minus(ev.fromDouble(lhs), rhs)
}

final class MultiplicativeSemigroupOps[A](lhs:A)(implicit ev:MultiplicativeSemigroup[A]) {
  def *(rhs:A): A = macro Ops.binop[A, A]
  def *(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def *(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def *(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) * rhs
}

final class LiteralIntMultiplicativeSemigroupOps(val lhs: Int) extends AnyVal {
  def *[A](rhs:A)(implicit ev: Ring[A]) = ev.times(ev.fromInt(lhs), rhs)
}

final class LiteralLongMultiplicativeSemigroupOps(val lhs: Long) extends AnyVal {
  def *[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]) = ev.times(c.fromLong(lhs), rhs)
}

final class LiteralDoubleMultiplicativeSemigroupOps(val lhs: Double) extends AnyVal {
  def *[A](rhs:A)(implicit ev:Field[A]) = ev.times(ev.fromDouble(lhs), rhs)
}

final class MultiplicativeGroupOps[A](lhs:A)(implicit ev:MultiplicativeGroup[A]) {
  def reciprocal() = macro Ops.unop[A]
  def /(rhs:A): A = macro Ops.binop[A, A]
  def /(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def /(rhs:Double)(implicit ev1:Field[A]): A = macro Ops.binopWithLift[Double, Field[A], A]
  def /(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) / rhs
}

final class LiteralIntMultiplicativeGroupOps(val lhs: Int) extends AnyVal {
  def /[A](rhs:A)(implicit ev: Field[A]) = ev.div(ev.fromInt(lhs), rhs)
}

final class LiteralLongMultiplicativeGroupOps(val lhs: Long) extends AnyVal {
  def /[A](rhs:A)(implicit ev: Field[A], c:ConvertableTo[A]) = ev.div(c.fromLong(lhs), rhs)
}

final class LiteralDoubleMultiplicativeGroupOps(val lhs: Double) extends AnyVal {
  def /[A](rhs:A)(implicit ev: Field[A]) = ev.div(ev.fromDouble(lhs), rhs)
}

final class SemiringOps[A](lhs:A)(implicit ev:Semiring[A]) {
  def pow(rhs:Int) = macro Ops.binop[Int, A]
  def **(rhs:Int) = macro Ops.binop[Int, A]
}

final class EuclideanRingOps[A](lhs:A)(implicit ev:EuclideanRing[A]) {
  def /~(rhs:A) = macro Ops.binop[A, A]
  def %(rhs:A) = macro Ops.binop[A, A]
  def /%(rhs:A) = macro Ops.binop[A, (A, A)]

  def gcd(rhs:A) = macro Ops.binop[A, A]
  def lcm(rhs:A) = macro Ops.binop[A, A]

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
  def /~[A](rhs:A)(implicit ev: EuclideanRing[A]) = ev.quot(ev.fromInt(lhs), rhs)
  def %[A](rhs:A)(implicit ev: EuclideanRing[A]) = ev.mod(ev.fromInt(lhs), rhs)
  def /%[A](rhs:A)(implicit ev: EuclideanRing[A]) = ev.quotmod(ev.fromInt(lhs), rhs)
}

final class LiteralLongEuclideanRingOps(val lhs: Long) extends AnyVal {
  def /~[A](rhs:A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]) = ev.quot(c.fromLong(lhs), rhs)
  def %[A](rhs:A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]) = ev.mod(c.fromLong(lhs), rhs)
  def /%[A](rhs:A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]) = ev.quotmod(c.fromLong(lhs), rhs)
}

final class LiteralDoubleEuclideanRingOps(val lhs: Double) extends AnyVal {
  def /~[A](rhs:A)(implicit ev: Field[A]) = ev.quot(ev.fromDouble(lhs), rhs)
  def %[A](rhs:A)(implicit ev: Field[A]) = ev.mod(ev.fromDouble(lhs), rhs)
  def /%[A](rhs:A)(implicit ev: Field[A]) = ev.quotmod(ev.fromDouble(lhs), rhs)
}

final class IsRealOps[A](lhs:A)(implicit ev:IsReal[A]) {
  def isWhole() = macro Ops.unop[Boolean]
  def ceil() = macro Ops.unop[A]
  def floor() = macro Ops.unop[A]
  def round() = macro Ops.unop[A]
  //def toDouble() = macro Ops.unop[Double]
}

final class NRootOps[A](lhs: A)(implicit ev: NRoot[A]) {
  def nroot(rhs: Int): A = macro Ops.binop[Int, A]
  def sqrt(): A = macro Ops.unop[A]
  def fpow(rhs: A): A = macro Ops.binop[A, A]

  // TODO: should be macros
  def pow(rhs: Double)(implicit c: Field[A]) = ev.fpow(lhs, c.fromDouble(rhs))
  def **(rhs: Double)(implicit c: Field[A]) = ev.fpow(lhs, c.fromDouble(rhs))

  def pow(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) pow rhs
  def **(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) ** rhs
}

final class LiteralIntNRootOps(val lhs: Int) extends AnyVal {
  def **[A](rhs:A)(implicit ev: NRoot[A], c:ConvertableTo[A]) = ev.fpow(c.fromLong(lhs), rhs)
}

final class LiteralLongNRootOps(val lhs: Long) extends AnyVal {
  def **[A](rhs:A)(implicit ev: NRoot[A], c:ConvertableTo[A]) = ev.fpow(c.fromLong(lhs), rhs)
}

final class LiteralDoubleNRootOps(val lhs: Double) extends AnyVal {
  def **[A](rhs:A)(implicit ev: NRoot[A], c:ConvertableTo[A]) = ev.fpow(c.fromDouble(lhs), rhs)
}

final class TrigOps[A](lhs: A)(implicit ev: Trig[A]) {
  def exp(): A = macro Ops.unop[A]
  def log(): A = macro Ops.unop[A]
}

final class BooleanAlgebraOps[A](lhs:A)(implicit ev:BooleanAlgebra[A]) {
  def unary_~() = macro Ops.unop[A]
  def &(rhs: A): A = macro Ops.binop[A, A]
  def |(rhs: A): A = macro Ops.binop[A, A]
  def ^(rhs: A): A = macro Ops.binop[A, A]

  def &(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def |(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def ^(rhs: Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]

  def &(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) & rhs
  def |(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) | rhs
  def ^(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) ^ rhs
}

final class ModuleOps[V](x: V) {
  def *:[F](lhs:F)(implicit ev: Module[V, F]): V = macro Ops.rbinopWithEv[F, Module[V, F], V]
  def :*[F](rhs:F)(implicit ev: Module[V, F]): V = macro Ops.binopWithEv[F, Module[V, F], V]

  // TODO: Are macros worth it here?
  def *:[F](lhs:Int)(implicit ev: Module[V, F], F: Ring[F]): V = ev.timesl(F.fromInt(lhs), x)
  def :*[F](rhs:Int)(implicit ev: Module[V, F], F: Ring[F]): V = ev.timesr(x, F.fromInt(rhs))
}

final class VectorSpaceOps[V](x: V) {
  def :/[F](rhs:F)(implicit ev: VectorSpace[V, F]): V = macro Ops.binopWithEv[F, VectorSpace[V, F], V]

  //def *:[F](lhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesl(ev.scalar.fromDouble(lhs), x)
  //def :*[F](rhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.timesr(x, ev.scalar.fromDouble(rhs))

  def :/[F](rhs:Int)(implicit ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromInt(rhs))
  def :/[F](rhs:Double)(implicit ev: VectorSpace[V, F]): V = ev.divr(x, ev.scalar.fromDouble(rhs))
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
