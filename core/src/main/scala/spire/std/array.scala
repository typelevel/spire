package spire.std

import spire.algebra._

import spire.NoImplicit

import scala.{ specialized => spec }
import scala.reflect.ClassTag

object ArraySupport {
  import spire.syntax.order._
  import spire.syntax.ring._

  def eqv[@spec A](x: Array[A], y: Array[A])(implicit ev: Eq[A]): Boolean = {
    var i = 0
    if (x.length != y.length) return false
    while (i < x.length && i < y.length && x(i) === y(i)) i += 1
    i == x.length
  }

  def vectorEqv[@spec A](x: Array[A], y: Array[A])(implicit ev: Eq[A], sc: AdditiveMonoid[A]): Boolean = {
    var i = 0
    while (i < x.length && i < y.length && x(i) === y(i)) i += 1
    while (i < x.length && x(i) === sc.zero) i += 1
    while (i < y.length && y(i) === sc.zero) i += 1
    i >= x.length && i >= y.length
  }

  def compare[@spec A](x: Array[A], y: Array[A])(implicit ev: Order[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = x(i) compare y(i)
      if (cmp != 0) return cmp
      i += 1
    }
    x.length - y.length
  }

  def vectorCompare[@spec A](x: Array[A], y: Array[A])(implicit ev: Order[A], sc: Ring[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = x(i) compare y(i)
      if (cmp != 0) return cmp
      i += 1
    }
    while (i < x.length) {
      if (x(i) =!= sc.zero) return 1
      i += 1
    }
    while (i < y.length) {
      if (y(i) =!= sc.zero) return -1
      i += 1
    }
    0
  }

  def concat[@spec A: ClassTag](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](x.length + y.length)
    System.arraycopy(x, 0, z, 0, x.length)
    System.arraycopy(y, 0, z, x.length, y.length)
    z
  }

  def negate[@spec A: ClassTag: Ring](x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < x.length) {
      y(i) = -x(i)
      i += 1
    }
    y
  }

  def plus[@spec A: ClassTag: AdditiveMonoid](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) + y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }

  def minus[@spec A: ClassTag: AdditiveGroup](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) - y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = -y(i); i += 1 }
    z
  }

  def timesl[@spec A: ClassTag: MultiplicativeSemigroup](r: A, x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < y.length) { y(i) = r * x(i); i += 1 }
    y
  }

  def dot[@spec A](x: Array[A], y: Array[A])(implicit sc: Rig[A]): A = {
    var z = sc.zero
    var i = 0
    while (i < x.length && i < y.length) { z += x(i) * y(i); i += 1 }
    z
  }

  def axis[@spec A](dimensions: Int, i: Int)(implicit ct: ClassTag[A], sc: Rig[A]): Array[A] = {
    val v = new Array[A](dimensions)
    var j = 0
    while (j < v.length) { v(j) = sc.zero; j += 1 }
    if (i < dimensions) v(i) = sc.one
    v
  }

}

trait ArrayModule[@spec(Int,Long,Float,Double) A] extends Module[Array[A], A] {
  implicit def classTag: ClassTag[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}

trait ArrayVectorSpace[@spec(Int,Long,Float,Double) A] extends ArrayModule[A] with VectorSpace[Array[A], A]

trait ArrayInnerProductSpace[@spec(Int,Long,Float,Double) A] extends ArrayVectorSpace[A]
with InnerProductSpace[Array[A], A] {
  def dot(v: Array[A], w: Array[A]): A = ArraySupport.dot(v, w)
}

trait ArrayCoordinateSpace[@spec(Int,Long,Float,Double) A] extends CoordinateSpace[Array[A], A] with ArrayInnerProductSpace[A] {
  def coord(v: Array[A], i: Int): A = v(i)
  override def dot(v: Array[A], w: Array[A]): A = ArraySupport.dot(v, w)
  def axis(i: Int): Array[A] = ArraySupport.axis(dimensions, i)
}

trait ArrayInstances0 {
  implicit def ArrayModule[@spec(Int,Long,Float,Double) A](implicit ev: NoImplicit[VectorSpace[Array[A], A]],classTag0: ClassTag[A], scalar0: Ring[A]): Module[Array[A], A] = new ArrayModule[A] {
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

  implicit def ArrayEq[@spec(Int,Long,Float,Double) A: Eq]: Eq[Array[A]] =
    new Eq[Array[A]] {
      def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
    }
}

trait ArrayInstances2 extends ArrayInstances1 {
  implicit def ArrayInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], classTag0: ClassTag[A]): InnerProductSpace[Array[A], A] =
    new ArrayInnerProductSpace[A] {
      val scalar = scalar0
      val classTag = classTag0
    }

  implicit def ArrayOrder[@spec(Int,Long,Float,Double) A](implicit A0: Order[A]) =
    new Order[Array[A]] {
      override def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
      def compare(x: Array[A], y: Array[A]): Int = ArraySupport.compare(x, y)
    }
}

trait ArrayInstances3 extends ArrayInstances2 {
  implicit def ArrayNormedVectorSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A],
      classTag0: ClassTag[A]): NormedVectorSpace[Array[A], A] = ArrayInnerProductSpace[A].normed
}

trait ArrayInstances extends ArrayInstances3 {
  implicit def ArrayMonoid[@spec A: ClassTag] =
    new Monoid[Array[A]] {
      def id = new Array[A](0)
      def op(x: Array[A], y: Array[A]) = ArraySupport.concat(x, y)
    }
}
