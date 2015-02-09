package spire.std

import scala.{ specialized => spec }
import scala.reflect.ClassTag

import spire.algebra._
import spire.NoImplicit

object ArraySupport {
  import spire.syntax.order._
  import spire.syntax.ring._

  def eqv[@spec A: Eq](x: Array[A], y: Array[A]): Boolean = {
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

  def compare[@spec A: Order](x: Array[A], y: Array[A]): Int = {
    var i = 0
    while (i < x.length && i < y.length) {
      val cmp = x(i) compare y(i)
      if (cmp != 0) return cmp
      i += 1
    }
    x.length - y.length
  }

  def vectorCompare[@spec A](x: Array[A], y: Array[A])(implicit ev: Order[A], sc: AdditiveMonoid[A]): Int = {
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

  def negate[@spec(Int, Long, Float, Double) A: ClassTag: Ring](x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < x.length) {
      y(i) = -x(i)
      i += 1
    }
    y
  }

  def plus[@spec(Int, Long, Float, Double) A: ClassTag: AdditiveMonoid](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) + y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }

  def minus[@spec(Int, Long, Float, Double) A: ClassTag: AdditiveGroup](x: Array[A], y: Array[A]): Array[A] = {
    val z = new Array[A](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) - y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = -y(i); i += 1 }
    z
  }

  def timesl[@spec(Int, Long, Float, Double) A: ClassTag: MultiplicativeSemigroup](r: A, x: Array[A]): Array[A] = {
    val y = new Array[A](x.length)
    var i = 0
    while (i < y.length) { y(i) = r * x(i); i += 1 }
    y
  }

  def dot[@spec(Int, Long, Float, Double) A](x: Array[A], y: Array[A])(implicit sc: Rig[A]): A = {
    var z = sc.zero
    var i = 0
    while (i < x.length && i < y.length) { z += x(i) * y(i); i += 1 }
    z
  }

  def axis[@spec(Float, Double) A](dimensions: Int, i: Int)(implicit ct: ClassTag[A], sc: Rig[A]): Array[A] = {
    val v = new Array[A](dimensions)
    var j = 0
    while (j < v.length) { v(j) = sc.zero; j += 1 }
    if (i < dimensions) v(i) = sc.one
    v
  }
}

trait ArrayInstances0 {
  type NI0[A] = NoImplicit[VectorSpace[Array[A], A]]

  implicit def ArrayModule[@spec(Int,Long,Float,Double) A: NI0: ClassTag: Ring]: Module[Array[A], A] =
    new ArrayModule[A]
}

trait ArrayInstances1 extends ArrayInstances0 {
  type NI1[A] = NoImplicit[NormedVectorSpace[Array[A], A]]

  implicit def ArrayVectorSpace[@spec(Int,Long,Float,Double) A: NI1: ClassTag: Field]: VectorSpace[Array[A], A] =
    new ArrayVectorSpace[A]

  implicit def ArrayEq[@spec A: Eq]: Eq[Array[A]] =
    new ArrayEq[A]
}

trait ArrayInstances2 extends ArrayInstances1 {
  implicit def ArrayInnerProductSpace[@spec(Float, Double) A: Field: ClassTag]: InnerProductSpace[Array[A], A] =
    new ArrayInnerProductSpace[A]

  implicit def ArrayOrder[@spec A: Order]: Order[Array[A]] =
    new ArrayOrder[A]
}

trait ArrayInstances3 extends ArrayInstances2 {
  implicit def ArrayNormedVectorSpace[@spec(Float, Double) A: Field: NRoot: ClassTag]: NormedVectorSpace[Array[A], A] =
    ArrayInnerProductSpace[A].normed
}

trait ArrayInstances extends ArrayInstances3 {
  implicit def ArrayMonoid[@spec A: ClassTag]: Monoid[Array[A]] =
    new ArrayMonoid[A]
}

@SerialVersionUID(0L)
private final class ArrayModule[@spec(Int,Long,Float,Double) A: ClassTag: Ring]
    (implicit nvs: NoImplicit[VectorSpace[Array[A], A]])
    extends Module[Array[A], A] with Serializable {
  def scalar = Ring[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}

@SerialVersionUID(0L)
private final class ArrayVectorSpace[@spec(Int,Float,Long,Double) A: ClassTag: Field]
    (implicit nnvs: NoImplicit[NormedVectorSpace[Array[A], A]])
    extends VectorSpace[Array[A], A] with Serializable {
  def scalar = Field[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
}

@SerialVersionUID(0L)
private final class ArrayEq[@spec(Int,Float,Long,Double) A: Eq]
    extends Eq[Array[A]] with Serializable {
  def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
}

@SerialVersionUID(0L)
private final class ArrayInnerProductSpace[@spec(Int,Float,Long,Double) A: ClassTag: Field]
    extends InnerProductSpace[Array[A], A] with Serializable {
  def scalar = Field[A]
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
  def dot(x: Array[A], y: Array[A]): A = ArraySupport.dot(x, y)
}

@SerialVersionUID(0L)
private final class ArrayOrder[@spec(Int,Float,Long,Double) A: Order]
    extends Order[Array[A]] with Serializable {
  override def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.eqv(x, y)
  def compare(x: Array[A], y: Array[A]): Int = ArraySupport.compare(x, y)
}

@SerialVersionUID(0L)
private final class ArrayMonoid[@spec(Int,Float,Long,Double) A: ClassTag]
    extends Monoid[Array[A]] with Serializable {
  def id = new Array[A](0)
  def op(x: Array[A], y: Array[A]) = ArraySupport.concat(x, y)
}

@SerialVersionUID(0L)
class ArrayCoordinateSpace[@spec(Int,Long,Float,Double) A: ClassTag](final val dimensions: Int)(implicit val scalar: Field[A])
extends CoordinateSpace[Array[A], A] with Serializable {
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
  override def dot(x: Array[A], y: Array[A]): A = ArraySupport.dot(x, y)
  def coord(v: Array[A], i: Int): A = v(i)
  def axis(i: Int): Array[A] = ArraySupport.axis(dimensions, i)
}

@SerialVersionUID(0L)
class EuclideanArrayCoordinateSpace[@spec(Int,Long,Float,Double) A: ClassTag](implicit val scalar: Field[A])
extends EuclideanCoordinateSpace[Array[A], A] with Serializable {

  def cross(v: Array[A], w: Array[A]) : Array[A] =
    Array( 
      // x(1) * y(2) - x(2) * y(1),
      scalar.minus(scalar.times(v(1), w(2)), scalar.times(v(2), w(1))),
      // x(2) * y(0) - x(0) * y(2),
      scalar.minus(scalar.times(v(2), w(0)), scalar.times(v(0), w(2))),            
      // x(0) * y(1) - x(1) * y(0)
      scalar.minus(scalar.times(v(0), w(1)), scalar.times(v(1), w(0))))
      
  def angle(v: Array[A], y: Array[A])(implicit nroot: NRoot[A], trig: Trig[A]): A = {
    // law of cosines
    // c2 = x2 + y2 - 2xy cos(angle)
    val x2 = dot(v, v)
    val y2 = dot(y, y)
    val c  = minus(v,y)
    val c2 = dot(c,c)
    val n  = scalar.minus(scalar.plus(x2, y2),c2)
    val d  = scalar.times(scalar.times(NRoot[A].sqrt(x2), NRoot[A].sqrt(y2)), scalar.fromInt(2)) 
    spire.math.acos(scalar.div(n, d))
  }
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
  override def dot(x: Array[A], y: Array[A]): A = ArraySupport.dot(x, y)
  def coord(v: Array[A], i: Int): A = v(i)
  def axis(i: Int): Array[A] = ArraySupport.axis(dimensions, i)
     
}

@SerialVersionUID(0L)
class ArrayVectorEq[@spec(Int,Long,Float,Double) A: Eq: AdditiveMonoid]
extends Eq[Array[A]] with Serializable {
  def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.vectorEqv(x, y)
}

@SerialVersionUID(0L)
class ArrayVectorOrder[@spec(Int,Long,Float,Double) A: Order: AdditiveMonoid]
extends Order[Array[A]] with Serializable {
  override def eqv(x: Array[A], y: Array[A]): Boolean = ArraySupport.vectorEqv(x, y)

  def compare(x: Array[A], y: Array[A]): Int = ArraySupport.vectorCompare(x, y)
}
