package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/**
 * A module generalizes a vector space by requiring its scalar need only form
 * a ring, rather than a field. In Spire, a `Module` is a left R-Module.
 */
trait Module[V, @spec(Int,Long,Float,Double) R] extends AdditiveGroup[V] {
  implicit def scalar: Ring[R] // TODO: Can this be Rng[R] instead?

  def timesl(r: R, v: V): V
}

/**
 * An R-module whose scalar multiplication comes from the right.
 */
trait RightModule[V, @spec(Int,Long,Float,Double) R] extends AdditiveGroup[V] {
  implicit def scalar: Ring[R]

  def timesr(v: V, r: R): V
}

object Module extends Module2

final class ModuleOps[V, F](rhs: V)(implicit ev: Module[V, F]) {
  def *: (lhs:F): V = macro Ops.rbinop[F, V]
}

final class RightModuleOps[V, F](lhs: V)(implicit ev: RightModule[V, F]) {
  def :* (rhs:F): V = macro Ops.binop[F, V]
}

trait Module0 {
  implicit def seq[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      ring0: Ring[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]) = new SeqModule[A, CC[A]] {
    val scalar = ring0
    val cbf = cbf0
  }
}

trait Module1 extends Module0 {
  implicit def IdentityModule[@spec(Int,Long,Float,Double) V](implicit ring: Ring[V]) = {
    new IdentityModule[V] {
      val scalar = ring
    }
  }

  implicit def ArrayModule[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Ring[A], classTag0: ClassTag[A]): Module[Array[A], A] = new ArrayModule[A] {
    val scalar = scalar0
    val classTag = classTag0
  }

  implicit def RingAlgebraIsModule[V,@spec(Int,Long,Float,Double) R](implicit
    alg: RingAlgebra[V, R]): Module[V, R] = alg

  implicit def Tuple2IsModule[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Ring[A]) = new Tuple2IsModule[A] {
    val scalar = scalar0
  }

  implicit def Tuple3IsModule[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Ring[A]) = new Tuple3IsModule[A] {
    val scalar = scalar0
  }

  implicit def Tuple4IsModule[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Ring[A]) = new Tuple4IsModule[A] {
    val scalar = scalar0
  }

  implicit def Tuple5IsModule[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Ring[A]) = new Tuple5IsModule[A] {
    val scalar = scalar0
  }

  implicit def Tuple6IsModule[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Ring[A]) = new Tuple6IsModule[A] {
    val scalar = scalar0
  }

  implicit def Tuple7IsModule[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Ring[A]) = new Tuple7IsModule[A] {
    val scalar = scalar0
  }
}

trait Module2 extends Module1 {
  implicit def VectorSpaceIsModule[V,@spec(Int,Long,Float,Double) R](implicit
    vs: VectorSpace[V, R]): Module[V, R] = vs
}

trait IdentityModule[@spec(Int,Long,Float,Double) V] extends Module[V, V] {
  def zero = scalar.zero
  def negate(v: V) = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}

trait ArrayModule[@spec(Int,Long,Float,Double) A] extends Module[Array[A], A] {
  implicit def classTag: ClassTag[A]

  def zero: Array[A] = new Array[A](0)

  def negate(x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < x.length) {
      y(i) = scalar.negate(x(i))
      i += 1
    }
    x
  }

  def plus(x: Array[A], y: Array[A]): Array[A] = {
    var prefix = math.min(x.length, y.length)
    var z = new Array[A](math.max(x.length, y.length))
    var i = 0
    while (i < prefix) {
      z(i) = scalar.plus(x(i), y(i))
      i += 1
    }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }

  override def minus(x: Array[A], y: Array[A]): Array[A] = {
    var prefix = math.min(x.length, y.length)
    var z = new Array[A](math.max(x.length, y.length))
    var i = 0
    while (i < prefix) {
      z(i) = scalar.minus(x(i), y(i))
      i += 1
    }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = scalar.negate(y(i)); i += 1 }
    z
  }

  def timesl(r: A, x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < y.length) {
      y(i) = scalar.times(r, x(i))
      i += 1
    }
    y
  }
}

trait Tuple2IsModule[@spec(Int,Long,Float,Double) A]
extends Module[(A, A), A] {
  def zero: (A, A) = (scalar.zero, scalar.zero)
  def plus(x: (A, A), y: (A, A)): (A, A) =
    (scalar.plus(x._1, y._1), scalar.plus(x._2, y._2))
  def negate(x: (A, A)): (A, A) =
    (scalar.negate(x._1), scalar.negate(x._2))
  override def minus(x: (A, A), y: (A, A)): (A, A) =
    (scalar.minus(x._1, y._1), scalar.minus(x._2, y._2))
  def timesl(r: A, v: (A, A)): (A, A) = (scalar.times(r, v._1), scalar.times(r, v._2))
}

trait Tuple3IsModule[@spec(Int,Long,Float,Double) A]
extends Module[(A, A, A), A] {
  def zero: (A, A, A) = (scalar.zero, scalar.zero, scalar.zero)
  def plus(x: (A, A, A), y: (A, A, A)): (A, A, A) =
    (scalar.plus(x._1, y._1), scalar.plus(x._2, y._2), scalar.plus(x._3, y._3))
  def negate(x: (A, A, A)): (A, A, A) =
    (scalar.negate(x._1), scalar.negate(x._2), scalar.negate(x._3))
  override def minus(x: (A, A, A), y: (A, A, A)): (A, A, A) =
    (scalar.minus(x._1, y._1), scalar.minus(x._2, y._2), scalar.minus(x._3, y._3))
  def timesl(r: A, v: (A, A, A)): (A, A, A) =
    (scalar.times(r, v._1), scalar.times(r, v._2), scalar.times(r, v._3))
}

trait Tuple4IsModule[@spec(Int,Long,Float,Double) A]
extends Module[(A, A, A, A), A] {
  def zero: (A, A, A, A) = (scalar.zero, scalar.zero, scalar.zero, scalar.zero)
  def plus(x: (A, A, A, A), y: (A, A, A, A)): (A, A, A, A) =
    (scalar.plus(x._1, y._1), scalar.plus(x._2, y._2), scalar.plus(x._3, y._3), scalar.plus(x._4, y._4))
  def negate(x: (A, A, A, A)): (A, A, A, A) =
    (scalar.negate(x._1), scalar.negate(x._2), scalar.negate(x._3), scalar.negate(x._4))
  override def minus(x: (A, A, A, A), y: (A, A, A, A)): (A, A, A, A) =
    (scalar.minus(x._1, y._1), scalar.minus(x._2, y._2), scalar.minus(x._3, y._3), scalar.minus(x._4, y._4))
  def timesl(r: A, v: (A, A, A, A)): (A, A, A, A) =
    (scalar.times(r, v._1), scalar.times(r, v._2), scalar.times(r, v._3), scalar.times(r, v._4))
}

trait Tuple5IsModule[@spec(Int,Long,Float,Double) A]
extends Module[(A, A, A, A, A), A] {
  def zero: (A, A, A, A, A) = (scalar.zero, scalar.zero, scalar.zero, scalar.zero, scalar.zero)
  def plus(x: (A, A, A, A, A), y: (A, A, A, A, A)): (A, A, A, A, A) =
    (scalar.plus(x._1, y._1), scalar.plus(x._2, y._2), scalar.plus(x._3, y._3), scalar.plus(x._4, y._4), scalar.plus(x._5, y._5))
  def negate(x: (A, A, A, A, A)): (A, A, A, A, A) =
    (scalar.negate(x._1), scalar.negate(x._2), scalar.negate(x._3), scalar.negate(x._4), scalar.negate(x._5))
  override def minus(x: (A, A, A, A, A), y: (A, A, A, A, A)): (A, A, A, A, A) =
    (scalar.minus(x._1, y._1), scalar.minus(x._2, y._2), scalar.minus(x._3, y._3), scalar.minus(x._4, y._4), scalar.minus(x._5, y._5))
  def timesl(r: A, v: (A, A, A, A, A)): (A, A, A, A, A) =
    (scalar.times(r, v._1), scalar.times(r, v._2), scalar.times(r, v._3), scalar.times(r, v._4), scalar.times(r, v._5))
}

trait Tuple6IsModule[@spec(Int,Long,Float,Double) A]
extends Module[(A, A, A, A, A, A), A] {
  def zero: (A, A, A, A, A, A) = (scalar.zero, scalar.zero, scalar.zero, scalar.zero, scalar.zero, scalar.zero)
  def plus(x: (A, A, A, A, A, A), y: (A, A, A, A, A, A)): (A, A, A, A, A, A) =
    (scalar.plus(x._1, y._1), scalar.plus(x._2, y._2), scalar.plus(x._3, y._3), scalar.plus(x._4, y._4), scalar.plus(x._4, y._4), scalar.plus(x._4, y._4))
  def negate(x: (A, A, A, A, A, A)): (A, A, A, A, A, A) =
    (scalar.negate(x._1), scalar.negate(x._2), scalar.negate(x._3), scalar.negate(x._4), scalar.negate(x._4), scalar.negate(x._4))
  override def minus(x: (A, A, A, A, A, A), y: (A, A, A, A, A, A)): (A, A, A, A, A, A) =
    (scalar.minus(x._1, y._1), scalar.minus(x._2, y._2), scalar.minus(x._3, y._3), scalar.minus(x._4, y._4), scalar.minus(x._4, y._4), scalar.minus(x._4, y._4))
  def timesl(r: A, v: (A, A, A, A, A, A)): (A, A, A, A, A, A) =
    (scalar.times(r, v._1), scalar.times(r, v._2), scalar.times(r, v._3), scalar.times(r, v._4), scalar.times(r, v._4), scalar.times(r, v._4))
}

trait Tuple7IsModule[@spec(Int,Long,Float,Double) A]
extends Module[(A, A, A, A, A, A, A), A] {
  def zero: (A, A, A, A, A, A, A) = (scalar.zero, scalar.zero, scalar.zero, scalar.zero, scalar.zero, scalar.zero, scalar.zero)
  def plus(x: (A, A, A, A, A, A, A), y: (A, A, A, A, A, A, A)): (A, A, A, A, A, A, A) =
    (scalar.plus(x._1, y._1), scalar.plus(x._2, y._2), scalar.plus(x._3, y._3), scalar.plus(x._4, y._4), scalar.plus(x._5, y._5), scalar.plus(x._6, y._6), scalar.plus(x._7, y._7))
  def negate(x: (A, A, A, A, A, A, A)): (A, A, A, A, A, A, A) =
    (scalar.negate(x._1), scalar.negate(x._2), scalar.negate(x._3), scalar.negate(x._4), scalar.negate(x._5), scalar.negate(x._6), scalar.negate(x._7))
  override def minus(x: (A, A, A, A, A, A, A), y: (A, A, A, A, A, A, A)): (A, A, A, A, A, A, A) =
    (scalar.minus(x._1, y._1), scalar.minus(x._2, y._2), scalar.minus(x._3, y._3), scalar.minus(x._4, y._4), scalar.minus(x._5, y._5), scalar.minus(x._6, y._6), scalar.minus(x._7, y._7))
  def timesl(r: A, v: (A, A, A, A, A, A, A)): (A, A, A, A, A, A, A) =
    (scalar.times(r, v._1), scalar.times(r, v._2), scalar.times(r, v._3), scalar.times(r, v._4), scalar.times(r, v._5), scalar.times(r, v._6), scalar.times(r, v._7))
}
