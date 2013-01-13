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

object InnerProductSpace extends InnerProductSpace1

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
}

trait InnerProductSpace1 extends InnerProductSpace0 {
  implicit def ArrayInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A],
      classTag0: ClassTag[A]): InnerProductSpace[Array[A], A] = new ArrayInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
    val classTag = classTag0
  }

  implicit def Tuple2IsInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A]) = new Tuple2IsInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
  }

  implicit def Tuple3IsInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A]) = new Tuple3IsInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
  }

  implicit def Tuple4IsInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A]) = new Tuple4IsInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
  }

  implicit def Tuple5IsInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A]) = new Tuple5IsInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
  }

  implicit def Tuple6IsInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A]) = new Tuple6IsInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
  }

  implicit def Tuple7IsInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A]) = new Tuple7IsInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
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

trait SeqInnerProductSpace[A, SA <: SeqLike[A, SA]] extends SeqVectorSpace[A, SA]
with InnerProductSpace[SA, A] {
  def dot(x: SA, y: SA): A = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A], acc: A): A = {
      if (xi.hasNext && yi.hasNext) {
        loop(xi, yi, scalar.plus(acc, scalar.times(xi.next(), yi.next())))
      } else {
        acc
      }
    }

    loop(x.toIterator, y.toIterator, scalar.zero)
  }
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

trait Tuple2IsInnerProductSpace[@spec(Int,Long,Float,Double) A]
extends InnerProductSpace[(A, A), A] with Tuple2IsModule[A] {
  def dot(v: (A, A), w: (A, A)): A =
    scalar.plus(scalar.times(v._1, w._1), scalar.times(v._2, w._2))
}

trait Tuple3IsInnerProductSpace[@spec(Int,Long,Float,Double) A]
extends InnerProductSpace[(A, A, A), A] with Tuple3IsModule[A] {
  def dot(v: (A, A, A), w: (A, A, A)): A =
    scalar.plus(scalar.plus(scalar.times(v._1, w._1), scalar.times(v._2, w._2)), scalar.times(v._3, w._3))
}

trait Tuple4IsInnerProductSpace[@spec(Int,Long,Float,Double) A]
extends InnerProductSpace[(A, A, A, A), A] with Tuple4IsModule[A] {
  def dot(v: (A, A, A, A), w: (A, A, A, A)): A =
    scalar.plus(scalar.plus(scalar.plus(scalar.times(v._1, w._1), scalar.times(v._2, w._2)), scalar.times(v._3, w._3)), scalar.times(v._4, w._4))
}

trait Tuple5IsInnerProductSpace[@spec(Int,Long,Float,Double) A]
extends InnerProductSpace[(A, A, A, A, A), A] with Tuple5IsModule[A] {
  def dot(v: (A, A, A, A, A), w: (A, A, A, A, A)): A =
    scalar.plus(scalar.plus(scalar.plus(scalar.plus(scalar.times(v._1, w._1), scalar.times(v._2, w._2)), scalar.times(v._3, w._3)), scalar.times(v._4, w._4)), scalar.times(v._5, w._5))
}

trait Tuple6IsInnerProductSpace[@spec(Int,Long,Float,Double) A]
extends InnerProductSpace[(A, A, A, A, A, A), A] with Tuple6IsModule[A] {
  def dot(v: (A, A, A, A, A, A), w: (A, A, A, A, A, A)): A =
    scalar.plus(scalar.plus(scalar.plus(scalar.plus(scalar.plus(scalar.times(v._1, w._1), scalar.times(v._2, w._2)), scalar.times(v._3, w._3)), scalar.times(v._4, w._4)), scalar.times(v._5, w._5)), scalar.times(v._6, w._6))
}

trait Tuple7IsInnerProductSpace[@spec(Int,Long,Float,Double) A]
extends InnerProductSpace[(A, A, A, A, A, A, A), A] with Tuple7IsModule[A] {
  def dot(v: (A, A, A, A, A, A, A), w: (A, A, A, A, A, A, A)): A =
    scalar.plus(scalar.plus(scalar.plus(scalar.plus(scalar.plus(scalar.plus(scalar.times(v._1, w._1), scalar.times(v._2, w._2)), scalar.times(v._3, w._3)), scalar.times(v._4, w._4)), scalar.times(v._5, w._5)), scalar.times(v._6, w._6)), scalar.times(v._7, w._7))
}
