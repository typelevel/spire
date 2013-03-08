package spire.algebra

import spire.macrosk.Ops
import spire.math.{ConvertableTo, ConvertableFrom, Number}

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

  def >(rhs: Int)(implicit c: ConvertableTo[A]) = ev.gt(lhs, c.fromInt(rhs))
  def >=(rhs: Int)(implicit c: ConvertableTo[A]) = ev.gteqv(lhs, c.fromInt(rhs))
  def <(rhs: Int)(implicit c: ConvertableTo[A]) = ev.lt(lhs, c.fromInt(rhs))
  def <=(rhs: Int)(implicit c: ConvertableTo[A]) = ev.lteqv(lhs, c.fromInt(rhs))
  def compare(rhs: Int)(implicit c: ConvertableTo[A]) = ev.compare(lhs, c.fromInt(rhs))
  def min(rhs: Int)(implicit c: ConvertableTo[A]) = ev.min(lhs, c.fromInt(rhs))
  def max(rhs: Int)(implicit c: ConvertableTo[A]) = ev.max(lhs, c.fromInt(rhs))

  def >(rhs: Double)(implicit c: ConvertableTo[A]) = ev.gt(lhs, c.fromDouble(rhs))
  def >=(rhs: Double)(implicit c: ConvertableTo[A]) = ev.gteqv(lhs, c.fromDouble(rhs))
  def <(rhs: Double)(implicit c: ConvertableTo[A]) = ev.lt(lhs, c.fromDouble(rhs))
  def <=(rhs: Double)(implicit c: ConvertableTo[A]) = ev.lteqv(lhs, c.fromDouble(rhs))
  def compare(rhs: Double)(implicit c: ConvertableTo[A]) = ev.compare(lhs, c.fromDouble(rhs))
  def min(rhs: Double)(implicit c: ConvertableTo[A]) = ev.min(lhs, c.fromDouble(rhs))
  def max(rhs: Double)(implicit c: ConvertableTo[A]) = ev.max(lhs, c.fromDouble(rhs))

  def >(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) > rhs
  def >=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) >= rhs
  def <(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) < rhs
  def <=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) <= rhs
  def compare(rhs:Number)(implicit c:ConvertableFrom[A]): Int = c.toNumber(lhs) compare rhs
  def min(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) min rhs
  def max(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) max rhs
}

final class SignedOps[A:Signed](lhs: A) {
  def abs(): A = macro Ops.unop[A]
  def sign(): Sign = macro Ops.unop[Sign]
  def signum(): Int = macro Ops.unop[Int]
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = macro Ops.binop[A, A]
}

final class GroupOps[A](lhs:A)(implicit ev:Group[A]) {
  def inverse() = macro Ops.unop[A]
}

final class AdditiveSemigroupOps[A](lhs:A)(implicit ev:AdditiveSemigroup[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
  def +(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def +(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def +(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) + rhs
}

final class AdditiveGroupOps[A](lhs:A)(implicit ev:AdditiveGroup[A]) {
  def unary_-() = macro Ops.unop[A]
  def -(rhs:A): A = macro Ops.binop[A, A]
  def -(rhs:Int)(implicit ev1: Ring[A]): A =  macro Ops.binopWithLift[Int, Ring[A], A]
  def -(rhs:Double)(implicit ev1:ConvertableTo[A]): A =  macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def -(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) - rhs
}

final class MultiplicativeSemigroupOps[A](lhs:A)(implicit ev:MultiplicativeSemigroup[A]) {
  def *(rhs:A): A = macro Ops.binop[A, A]
  def *(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def *(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def *(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) * rhs
}

final class MultiplicativeGroupOps[A](lhs:A)(implicit ev:MultiplicativeGroup[A]) {
  def reciprocal() = macro Ops.unop[A]
  def /(rhs:A): A = macro Ops.binop[A, A]
  def /(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def /(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def /(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) / rhs
}

final class SemiringOps[A](lhs:A)(implicit ev:Semiring[A]) {
  def pow(rhs:Int) = macro Ops.binop[Int, A]
  def **(rhs:Int) = macro Ops.binop[Int, A]
}

final class EuclideanRingOps[A](lhs:A)(implicit ev:EuclideanRing[A]) {
  def /~(rhs:A) = macro Ops.binop[A, A]
  def %(rhs:A) = macro Ops.binop[A, A]
  def /%(rhs:A) = macro Ops.binop[A, A]

  def gcd(rhs:A) = macro Ops.binop[A, A]
  def lcm(rhs:A) = macro Ops.binop[A, A]

  // TODO: This is a bit
  def /~(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def %(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def /%(rhs:Int): (A, A) = macro Ops.binopWithSelfLift[Int, Ring[A], (A, A)]

  def /~(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def %(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def /%(rhs:Double)(implicit ev1:ConvertableTo[A]): (A, A) = macro Ops.binopWithLift[Double, ConvertableTo[A], (A, A)]

  def /~(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) /~ rhs
  def %(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) % rhs
  def /%(rhs:Number)(implicit c:ConvertableFrom[A]): (Number, Number) = c.toNumber(lhs) /% rhs
}

final class FieldOps[A](lhs:A)(implicit ev:Field[A]) {
  def isWhole() = macro Ops.unop[Boolean]
  def ceil() = macro Ops.unop[A]
  def floor() = macro Ops.unop[A]
  def round() = macro Ops.unop[A]
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

final class TrigOps[A](lhs: A)(implicit ev: Trig[A]) {
  def exp(): A = macro Ops.unop[A]
  def log(): A = macro Ops.unop[A]
}

final class BooleanAlgebraOps[A](lhs:A)(implicit ev:BooleanAlgebra[A]) {
  def unary_~() = macro Ops.unop[A]
  def &(rhs: A): A = macro Ops.binop[A, A]
  def |(rhs: A): A = macro Ops.binop[A, A]
  def ^(rhs: A): A = macro Ops.binop[A, A]

  def &(rhs: Int)(implicit c: ConvertableTo[A]): A = ev.and(lhs, c.fromInt(rhs))
  def |(rhs: Int)(implicit c: ConvertableTo[A]): A = ev.or(lhs, c.fromInt(rhs))
  def ^(rhs: Int)(implicit c: ConvertableTo[A]): A = ev.xor(lhs, c.fromInt(rhs))

  def &(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) & rhs
  def |(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) | rhs
  def ^(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) ^ rhs
}

final class ModuleOps[V, F](rhs: V)(implicit ev: Module[V, F]) {
  def *: (lhs:F): V = macro Ops.rbinop[F, V]
  def :* (rhs:F): V = macro Ops.binop[F, V]
}

final class VectorSpaceOps[V, F](rhs: V)(implicit ev: VectorSpace[V, F]) {
  def :/ (rhs:F): V = macro Ops.binop[F, V]
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

final class NormedVectorSpaceOps[V](lhs: V) {
  def norm[F](implicit ev: NormedVectorSpace[V, F]): F =
    macro Ops.unopWithEv[NormedVectorSpace[V, F], F]

  def normalize[F](implicit ev: NormedVectorSpace[V, F]): V =
    macro Ops.unopWithEv[NormedVectorSpace[V, F], V]
}
