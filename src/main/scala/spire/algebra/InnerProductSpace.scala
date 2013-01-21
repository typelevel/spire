package spire.algebra

import spire.math.{ Complex, Trig, Fractional }
import spire.macrosk.Ops

import scala.{ specialized => spec }
import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait InnerProductSpace[V, @spec(Int, Long, Float, Double) F] extends NormedVectorSpace[V, F] {
  implicit def nroot: NRoot[F]

  def norm(v: V): F = nroot.sqrt(dot(v, v))
  def dot(v: V, w: V): F
}

object InnerProductSpace extends InnerProductSpace1 {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: InnerProductSpace[V, R]) = V
}

final class InnerProductSpaceOps[V](lhs: V) {
  def dot[F](rhs: V)(implicit ev: InnerProductSpace[V, F]): F =
    macro Ops.binopWithEv[V, InnerProductSpace[V, F], F]
  def ⋅[F](rhs: V)(implicit ev: InnerProductSpace[V, F]): F =
    macro Ops.binopWithEv[V, InnerProductSpace[V, F], F]
}

trait InnerProductSpace0 {
  implicit def seq[A, CC[A] <: SeqLike[A, CC[A]]](implicit field0: Field[A],
      nroot0: NRoot[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]) = new SeqInnerProductSpace[A, CC[A]] {
    val scalar = field0
    val nroot = nroot0
    val cbf = cbf0
  }

  implicit def MapInnerProductSpace[K, V: Field: NRoot] = new MapInnerProductSpace[K, V] {
    val scalar = Field[V]
    val nroot = NRoot[V]
  }
}

trait InnerProductSpace1 extends InnerProductSpace0 {
  implicit def ArrayInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A],
      classTag0: ClassTag[A]): InnerProductSpace[Array[A], A] = new ArrayInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
    val classTag = classTag0
  }

  implicit def ComplexIsInnerProductSpace[@spec(Float, Double) A](implicit
      f0: Fractional[A], t0: Trig[A]) = new ComplexIsInnerProductSpace[A] {
    def scalar = f0
    def nroot = f0
    def f = f0
    def t = t0
  }
}

trait ComplexIsInnerProductSpace[@spec(Float, Double) A] extends ComplexIsField[A]
with InnerProductSpace[Complex[A], A] with RingAlgebra[Complex[A], A] {
  def timesl(r: A, v: Complex[A]): Complex[A] = Complex(r, scalar.zero) * v
  override def norm(x: Complex[A]): A = x.abs
  def dot(x: Complex[A], y: Complex[A]): A =
    scalar.plus(scalar.times(x.real, y.real), scalar.times(x.imag, y.imag))
}

trait ArrayInnerProductSpace[@spec(Int,Long,Float,Double) A] extends ArrayVectorSpace[A]
with InnerProductSpace[Array[A], A] {
  def dot(x: Array[A], y: Array[A]): A = {
    var z = scalar.zero
    var i = 0
    while (i < x.length && i < y.length) {
      z = scalar.plus(z, scalar.times(x(i), y(i)))
      i += 1
    }
    z
  }
}
