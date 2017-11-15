package spire
package std

import spire.algebra._
import spire.NoImplicit

object ArraySupport {
  import spire.syntax.order._
  import spire.syntax.ring._

  def eqv[@sp A: Eq](x: Array[A], y: Array[A]): Boolean = {
    var i = 0
    if (x.length != y.length) return false
    while (i < x.length && i < y.length && x(i) === y(i)) i += 1
    i == x.length
  }

  def vectorEqv[@sp A](x: Array[A], y: Array[A])(implicit ev: Eq[A], sc: AdditiveMonoid[A]): Boolean = {
    var i = 0
    while (i < x.length && i < y.length && x(i) === y(i)) i += 1
    while (i < x.length && x(i) === sc.zero) i += 1
    while (i < y.length && y(i) === sc.zero) i += 1
    i >= x.length && i >= y.length
  }

  def compare[@sp A: Order](x: Array[A], y: Array[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = x(i) compare y(i)
      if (cmp != 0) return cmp
      i += 1
    }
    x.length - y.length
  }

  def vectorCompare[@sp A](x: Array[A], y: Array[A])(implicit ev: Order[A], sc: AdditiveMonoid[A]): Int = {
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

  def concat[@sp A: ClassTag](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](x.length + y.length)
    System.arraycopy(x, 0, z, 0, x.length)
    System.arraycopy(y, 0, z, x.length, y.length)
    z
  }

  def negate[@sp(Int, Long, Float, Double) A: ClassTag: Ring](x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < x.length) {
      y(i) = -x(i)
      i += 1
    }
    y
  }

  def plus[@sp(Int, Long, Float, Double) A: ClassTag: AdditiveMonoid](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) + y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }

  def minus[@sp(Int, Long, Float, Double) A: ClassTag: AdditiveGroup](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) - y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = -y(i); i += 1 }
    z
  }

  def timesl[@sp(Int, Long, Float, Double) A: ClassTag: MultiplicativeSemigroup](r: A, x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < y.length) { y(i) = r * x(i); i += 1 }
    y
  }

  def dot[@sp(Int, Long, Float, Double) A](x: Array[A], y: Array[A])(implicit sc: Rig[A]): A = {
    var z = sc.zero
    var i = 0
    while (i < x.length && i < y.length) { z += x(i) * y(i); i += 1 }
    z
  }

  def axis[@sp(Float, Double) A](dimensions: Int, i: Int)(implicit ct: ClassTag[A], sc: Rig[A]): Array[A] = {
    val v = new Array[A](dimensions)
    var j = 0
    while (j < v.length) { v(j) = sc.zero; j += 1 }
    if (i < dimensions) v(i) = sc.one
    v
  }
}

trait ArrayInstances0 {
  type NI0[A] = NoImplicit[VectorSpace[Array[A], A]]

  implicit def ArrayCModule[@sp(Int,Long,Float,Double) A: NI0: ClassTag: CRing]: CModule[Array[A], A] =
    new ArrayCModule[A]
}

trait ArrayInstances1 extends ArrayInstances0 {

  implicit def ArrayVectorSpace[@sp(Int,Long,Float,Double) A: ClassTag: Field]: VectorSpace[Array[A], A] =
    new ArrayVectorSpace[A]

  implicit def ArrayEq[@sp A: Eq]: Eq[Array[A]] =
    new ArrayEq[A]
}

trait ArrayInstances2 extends ArrayInstances1 {
  implicit def ArrayOrder[@sp A: Order]: Order[Array[A]] =
    new ArrayOrder[A]
}

trait ArrayInstances extends ArrayInstances2 {
  implicit def ArrayMonoid[@sp A: ClassTag]: Monoid[Array[A]] =
    new ArrayMonoid[A]
}

@SerialVersionUID(0L)
private final class ArrayCModule[@sp(Int,Long,Float,Double) A: ClassTag: CRing]
    (implicit nvs: NoImplicit[VectorSpace[Array[A], A]])
    extends CModule[Array[A], A] with Serializable {
  def scalar: CRing[A] = CRing[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}

@SerialVersionUID(0L)
private final class ArrayVectorSpace[@sp(Int,Float,Long,Double) A: ClassTag: Field]
    extends VectorSpace[Array[A], A] with Serializable {
  def scalar: Field[A] = Field[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}

@SerialVersionUID(0L)
private final class ArrayEq[@sp(Int,Float,Long,Double) A: Eq]
    extends Eq[Array[A]] with Serializable {
  def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
}

@SerialVersionUID(0L)
private final class ArrayOrder[@sp(Int,Float,Long,Double) A: Order]
    extends Order[Array[A]] with Serializable {
  override def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
  def compare(x: Array[A], y: Array[A]): Int = ArraySupport.compare(x, y)
}

@SerialVersionUID(0L)
private final class ArrayMonoid[@sp(Int,Float,Long,Double) A: ClassTag]
    extends Monoid[Array[A]] with Serializable {
  def empty: Array[A] = new Array[A](0)
  def combine(x: Array[A], y: Array[A]): Array[A] = ArraySupport.concat(x, y)
}

@SerialVersionUID(0L)
class ArrayVectorEq[@sp(Int,Long,Float,Double) A: Eq: AdditiveMonoid]
extends Eq[Array[A]] with Serializable {
  def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.vectorEqv(x, y)
}

@SerialVersionUID(0L)
class ArrayVectorOrder[@sp(Int,Long,Float,Double) A: Order: AdditiveMonoid]
extends Order[Array[A]] with Serializable {
  override def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.vectorEqv(x, y)

  def compare(x: Array[A], y: Array[A]): Int = ArraySupport.vectorCompare(x, y)
}
