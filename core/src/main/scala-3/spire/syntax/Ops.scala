package spire
package syntax

import spire.algebra._
import spire.algebra.partial._
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

final class LeftModuleOps[V](x: V) extends AnyVal:
  def *:[F](lhs: F)(using ev: LeftModule[V, F]): V = ev.timesl(lhs, x)
  def *:[F](lhs: Int)(using ev: LeftModule[V, F], F: Ring[F]): V = ev.timesl(F.fromInt(lhs), x)

final class RightModuleOps[V](x: V) extends AnyVal:
  def :*[F](rhs: F)(using ev: RightModule[V, F]): V = ev.timesr(x, rhs)
  def :*[F](rhs: Int)(using ev: RightModule[V, F], F: Ring[F]): V = ev.timesr(x, F.fromInt(rhs))
