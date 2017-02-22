package spire
package math

import scala.{Vector => SVector}

import spire.algebra._
import spire.implicits._

import shapeless._
import shapeless.ops.nat._
import scala.reflect.ClassTag

final class Vec[N <: Nat, A](val repr: SVector[A]) extends AnyVal {
  
  import LT._
  import LTEq._

  def apply[NN <: Nat](implicit ev: NN < N, toInt: ToInt[NN]): A = 
    repr(toInt())

  def apply(n: Nat)(implicit ev: n.N < N, toInt: ToInt[n.N]): A = apply[n.N]

  def n = repr.size

  def map[B](f: A => B): Vec[N, B] = new Vec(repr.map(f))

  def ap[B](ff: Vec[N, A => B]): Vec[N, B] = 
    new Vec(repr.zip(ff.repr).map { case (a, f) => f(a) })

  def map2[B, C](that: Vec[N, B])(f: (A, B) => C): Vec[N, C] = 
    that.ap(map(f.curried))

  def updated(i: Nat, a: A)(implicit ev: i.N < N,
                           toInt: ToInt[i.N]): Vec[N, A] =
    updated[i.N](a)

  def updated[NN <: Nat](a: A)(implicit ev: NN < N,
                           toInt: ToInt[NN]): Vec[N, A] =
    new Vec(repr.updated(toInt(), a))


  def exists(f: A => Boolean): Boolean = repr.exists(f)

  def forall(f: A => Boolean): Boolean = repr.forall(f)

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    repr.foldLeft(b)(f)

  def x(implicit ev: nat._1 <= N): A = repr(0)
  def y(implicit ev: nat._2 <= N): A = repr(1)
  def z(implicit ev: nat._3 <= N): A = repr(2)
  def w(implicit ev: nat._4 <= N): A = repr(3)

  def r(implicit ev: nat._1 <= N): A = repr(0)
  def θ(implicit ev: nat._2 <= N): A = repr(1)
  def ϕ(implicit ev: nat._3 <= N): A = repr(2)

  def ===(that: Vec[N, A])(implicit EA: Eq[A]): Boolean =
    repr === that.repr

  def +(that: Vec[N, A])(implicit G: AdditiveSemigroup[A]): Vec[N, A] =
    map2(that)(_ + _)

  def +(a: A)(implicit G: AdditiveSemigroup[A]): Vec[N, A] =
    map(_ + a)

  def -(that: Vec[N, A])(implicit G: AdditiveGroup[A]): Vec[N, A] =
    map2(that)(_ - _)

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): Vec[N, A] =
    map(a * _)

  def -(a: A)(implicit G: AdditiveGroup[A]): Vec[N, A] =
    map(_ - a)

  def ⋅(that: Vec[N, A])(implicit G: Semiring[A]): A =
    map2(that)(_ * _).repr.foldLeft(G.zero)(_ + _)

  def :+(a: A): Vec[Succ[N], A] = new Vec(repr :+ a)


  def pad[D <: Nat](n: Nat, a: A)(
      implicit DD: Diff.Aux[n.N, N, D],
      toIntD: ToInt[D]): Vec[n.N, A] =
    new Vec[n.N, A](repr ++ SVector.fill(toIntD())(a))

  def padZero[D <: Nat](n: Nat)(implicit G: AdditiveMonoid[A],
                                DD: Diff.Aux[n.N, N, D],
                                toIntD: ToInt[D],
                                toIntN: ToInt[n.N]): Vec[n.N, A] =
    pad(n, G.zero)

  def padOne[D <: Nat](n: Nat)(implicit G: Ring[A],
                               DD: Diff.Aux[n.N, N, D],
                               toIntD: ToInt[D],
                               toIntN: ToInt[n.N]): Vec[n.N, A] = pad(n, G.one)


  def unary_-(implicit G: AdditiveGroup[A]): Vec[N, A] = map(-_)


  def dropUntil[D <: Nat](n: Nat)(implicit ev: n.N <= N,
                                  toInt: ToInt[n.N]): Vec[n.N, A] =
    new Vec(repr.take(toInt()))

  def ×(that: Vec[N, A])(implicit ev: N =:= nat._3, G: Rng[A]): Vec[nat._3, A] = {
    val SVector(u1, u2, u3) = repr
    val SVector(v1, v2, v3) = that.repr
    new Vec(SVector((u2 * v3) - (u3 * v2), (u3 * v1) - (u1 * v3), (u1 * v2) - (u2 * v1)))
  }

  def toArray(implicit classTag: ClassTag[A]): Array[A] = repr.toArray

  override def toString: String = repr.toString
}

object Vec extends VecInstances {

  import LT._

  def fill[N <: Nat, A](a: A)(implicit toInt: ToInt[N]): Vec[N, A] = new Vec(SVector.fill(toInt())(a))
  def fill[A](n: Nat)(a: A)(implicit toInt: ToInt[n.N]): Vec[n.N, A] = fill[n.N, A](a)
  def zero[N <: Nat, A](implicit G: AdditiveGroup[A], toInt: ToInt[N]): Vec[N, A] = fill[N, A](G.zero)
  def zero[A](n: Nat)(implicit G: AdditiveGroup[A], toInt: ToInt[n.N]): Vec[n.N, A] = zero[n.N, A]
  def sized[N <: Nat, A](repr: SVector[A])(implicit toInt :ToInt[N]): Vec[N, A] = {
    val N = toInt()
    require(N == repr.size, s"expected size $N is not the same as ${repr.length}")
    new Vec(repr)
  }
  def sized[A](n: Nat)(repr: SVector[A])(implicit toInt: ToInt[n.N]): Vec[n.N, A] = sized[n.N, A](repr)

  def basis[B <: Nat, N <: Nat, A](implicit R: Ring[A], ev: B < N, toIntB: ToInt[B], toIntN: ToInt[N]): Vec[N, A] = 
    zero[N, A].updated[B](R.one)

  def basis[A](b: Nat, n: Nat)(implicit R: Ring[A], ev: b.N < n.N, toIntB: ToInt[b.N], toIntN: ToInt[n.N]): Vec[n.N, A] =
    basis[b.N, n.N, A]
}

private[math] sealed trait VecEq[N <: Nat, A] extends Eq[Vec[N, A]] {
  implicit def EqA: Eq[A]
  def eqv(x: Vec[N, A], y: Vec[N, A]): Boolean = x === y
}

abstract class VecInstances extends VecInstances0 {
  implicit def vecEq[N <: Nat, A](implicit E: Eq[A]): Eq[Vec[N, A]] = new VecEq[N, A] {
    def EqA = E
  }

  implicit lazy val vec2fNormedVectorSpace: NormedVectorSpace[Vec2f, Float] = vec2fInnerProductSpace.normed
  implicit lazy val vec3fNormedVectorSpace: NormedVectorSpace[Vec3f, Float] = vec3fInnerProductSpace.normed
  implicit lazy val vec4fNormedVectorSpace: NormedVectorSpace[Vec4f, Float] = vec4fInnerProductSpace.normed
  implicit lazy val vec2dNormedVectorSpace: NormedVectorSpace[Vec2d, Double] = vec2dInnerProductSpace.normed
  implicit lazy val vec3dNormedVectorSpace: NormedVectorSpace[Vec3d, Double] = vec3dInnerProductSpace.normed
  implicit lazy val vec4dNormedVectorSpace: NormedVectorSpace[Vec4d, Double] = vec4dInnerProductSpace.normed
}

sealed trait VecInstances0 {

  implicit lazy val vec2fInnerProductSpace: InnerProductSpace[Vec2f, Float] = 
    new VecInnerProductSpace[nat._2, Float] {
      def scalar = FloatAlgebra
      def toInt = ToInt[nat._2]
    }

  implicit lazy val vec3fInnerProductSpace: InnerProductSpace[Vec3f, Float] = 
    new VecInnerProductSpace[nat._3, Float] {
      def scalar = FloatAlgebra
      def toInt = ToInt[nat._3]
    }

  implicit lazy val vec4fInnerProductSpace: InnerProductSpace[Vec4f, Float] = 
    new VecInnerProductSpace[nat._4, Float] {
      def scalar = FloatAlgebra
      def toInt = ToInt[nat._4]
    }

  implicit lazy val vec2dInnerProductSpace: InnerProductSpace[Vec2d, Double] = 
    new VecInnerProductSpace[nat._2, Double] {
      def scalar = DoubleAlgebra
      def toInt = ToInt[nat._2]
    }

  implicit lazy val vec3dInnerProductSpace: InnerProductSpace[Vec3d, Double] = 
    new VecInnerProductSpace[nat._3, Double] {
      def scalar = DoubleAlgebra
      def toInt = ToInt[nat._3]
    }

  implicit lazy val vec4dInnerProductSpace: InnerProductSpace[Vec4d, Double] = 
    new VecInnerProductSpace[nat._4, Double] {
      def scalar = DoubleAlgebra
      def toInt = ToInt[nat._4]
    }
}


private[math] sealed trait VecInnerProductSpace[N <: Nat, A] extends InnerProductSpace[Vec[N, A], A] {

  implicit def toInt: ToInt[N]

  def dot(x: Vec[N, A], y: Vec[N, A]): A =  x ⋅ y
  def timesl(l: A, x: Vec[N, A]): Vec[N, A] = l *: x
  def negate(v: Vec[N, A]): Vec[N, A] = -v
  val zero: Vec[N, A] = Vec.zero[N, A]
  def plus(x: Vec[N, A], y: Vec[N, A]): Vec[N, A] = x + y
}
