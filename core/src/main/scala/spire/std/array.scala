package spire.std

import spire.algebra._

import spire.NoImplicit

import scala.{ specialized => spec }
import scala.reflect.ClassTag

final class ArrayMonoid[@spec A: ClassTag] extends Monoid[Array[A]] {
  def id = new Array[A](0)
  def op(x: Array[A], y: Array[A]) = {
    val z = new Array[A](x.length + y.length)
    System.arraycopy(x, 0, z, 0, x.length)
    System.arraycopy(y, 0, z, x.length, y.length)
    z
  }
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
    y
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

trait ArrayVectorSpace[@spec(Int,Long,Float,Double) A] extends ArrayModule[A] with VectorSpace[Array[A], A]

trait ArrayInnerProductSpace[@spec(Int,Long,Float,Double) A] extends ArrayVectorSpace[A]
with InnerProductSpace[Array[A], A] {
  def dot(v: Array[A], w: Array[A]): A = {
    var z = scalar.zero
    var i = 0
    while (i < v.length && i < w.length) {
      z = scalar.plus(z, scalar.times(v(i), w(i)))
      i += 1
    }
    z
  }
}

trait ArrayCoordinateSpace[@spec(Int,Long,Float,Double) A] extends CoordinateSpace[Array[A], A] with ArrayInnerProductSpace[A] {
  def coord(v: Array[A], i: Int): A = v(i)

  override def dot(v: Array[A], w: Array[A]): A = super.dot(v, w)

  def axis(i: Int): Array[A] = {
    val v = new Array[A](dimensions)
    var j = 0
    while (j < v.length) {
      v(j) = if (i == j) scalar.one else scalar.zero
      j += 1
    }
    v
  }
}

trait ArrayEq[@spec(Int,Long,Float,Double) A] extends Eq[Array[A]] {
  implicit def A: Eq[A]
  implicit def classTag: ClassTag[A]

  def eqv(x: Array[A], y: Array[A]): Boolean = {
    if (x.length != y.length) false else {
      var i = 0
      while (i < x.length && i < y.length && A.eqv(x(i), y(i))) {
        i += 1
      }
      i == x.length
    }
  }
}

trait ArrayVectorEq[@spec(Int,Long,Float,Double) A] extends Eq[Array[A]] {
  def scalar: AdditiveMonoid[A]
  def A: Eq[A]

  def eqv(x: Array[A], y: Array[A]): Boolean = {
    var i = 0
    while (i < x.length && i < y.length && A.eqv(x(i), y(i))) {
      i += 1
    }
    while (i < x.length && A.eqv(x(i), scalar.zero)) {
      i += 1
    }
    while (i < y.length && A.eqv(y(i), scalar.zero)) {
      i += 1
    }
    i >= x.length && i >= y.length
  }
}

trait ArrayOrder[@spec(Int,Long,Float,Double) A] extends Order[Array[A]] with ArrayEq[A] {
  implicit def A: Order[A]

  override def eqv(x: Array[A], y: Array[A]): Boolean = super.eqv(x, y)

  def compare(x: Array[A], y: Array[A]): Int = {
    var i = 0
    var cmp = 0
    while (i < x.length && i < y.length && cmp == 0) {
      cmp = A.compare(x(i), y(i))
      i += 1
    }
    if (cmp == 0) x.length - y.length else cmp
  }
}

trait ArrayVectorOrder[@spec(Int,Long,Float,Double) A] extends Order[Array[A]] with ArrayVectorEq[A] {
  implicit def A: Order[A]

  override def eqv(x: Array[A], y: Array[A]): Boolean = super.eqv(x, y)

  def compare(x: Array[A], y: Array[A]): Int = {
    var i = 0
    var cmp = 0
    while (i < x.length && i < y.length && cmp == 0) {
      cmp = A.compare(x(i), y(i))
      i += 1
    }
    if (cmp != 0) return cmp
    while (i < x.length) {
      if (!A.eqv(x(i), scalar.zero))
        return 1
      i += 1
    }
    while (i < y.length) {
      if (!A.eqv(y(i), scalar.zero))
        return -1
      i += 1
    }
    0
  }
}

trait ArrayInstances0 {
  implicit def ArrayModule[@spec(Int,Long,Float,Double) A](implicit
      ev: NoImplicit[VectorSpace[Array[A], A]], classTag0: ClassTag[A],
      scalar0: Ring[A]): Module[Array[A], A] = new ArrayModule[A] {
    val scalar = scalar0
    val classTag = classTag0
  }
}

trait ArrayInstances1 extends ArrayInstances0 {
  implicit def ArrayVectorSpace[@spec(Int,Long,Float,Double) A](implicit
      ev: NoImplicit[NormedVectorSpace[Array[A], A]], classTag0: ClassTag[A],
      scalar0: Field[A]): VectorSpace[Array[A], A] = new ArrayVectorSpace[A] {
    val scalar = scalar0
    val classTag = classTag0
  }

  implicit def ArrayEq[@spec(Int,Long,Float,Double) A](implicit
      A0: Eq[A], ct: ClassTag[A]) = new ArrayEq[A] {
    val A = A0
    val classTag = ct
  }
}

trait ArrayInstances2 extends ArrayInstances1 {
  implicit def ArrayNormedVectorSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A],
      classTag0: ClassTag[A]): NormedVectorSpace[Array[A], A] = ArrayInnerProductSpace[A].normed

  implicit def ArrayInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], classTag0: ClassTag[A]): InnerProductSpace[Array[A], A] =
    new ArrayInnerProductSpace[A] {
      val scalar = scalar0
      val classTag = classTag0
    }

  implicit def ArrayOrder[@spec(Int,Long,Float,Double) A](implicit
      A0: Order[A], ct: ClassTag[A]) = new ArrayOrder[A] {
    val A = A0
    val classTag = ct
  }
}

trait ArrayInstances extends ArrayInstances2 {
  implicit def ArrayMonoid[@spec A: ClassTag] = new ArrayMonoid[A]
}
