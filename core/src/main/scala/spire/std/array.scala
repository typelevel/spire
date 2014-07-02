package spire.std

import java.lang.Integer.highestOneBit

import spire.algebra._

import spire.NoImplicit

import scala.{ specialized => spec }
import scala.reflect.ClassTag

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

  def negate[@spec(Int, Long, Float, Double) A: ClassTag: Rng](x: Array[A]): Array[A] = {
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

  def dot[@spec(Int, Long, Float, Double) A](x: Array[A], y: Array[A])(implicit sc: Semiring[A]): A = {
    var z = sc.zero
    var i = 0
    while (i < x.length && i < y.length) { z += x(i) * y(i); i += 1 }
    z
  }
}

trait ArrayInstances1 {
  type NI1[A] = NoImplicit[NormedVectorSpace[Array[A], A]]

  implicit def ArrayVectorSpace[@spec(Int,Long,Float,Double) A: NI1: Rng: ClassTag]: VectorSpace[Array[A], A] =
    new ArrayVectorSpace[A]

  implicit def ArrayEq[@spec A: Eq]: Eq[Array[A]] =
    new ArrayEq[A]

  implicit def ArrayBasis[@spec(Int,Long,Float,Double) A: AdditiveMonoid: ClassTag]: Frame[Array[A], A] =
    new ArrayBasis[A]
}

trait ArrayInstances2 extends ArrayInstances1 {
  implicit def ArrayInnerProductSpace[@spec(Float, Double) A: Rng: ClassTag]: InnerProductSpace[Array[A], A] =
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
private final class ArrayVectorSpace[@spec(Int,Float,Long,Double) A: ClassTag: Rng]
    extends VectorSpace[Array[A], A] with Serializable {
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
private final class ArrayBasis[@spec(Int,Float,Long,Double) A: ClassTag]
    (implicit A: AdditiveMonoid[A])
    extends Frame[Array[A], A] with Serializable {
  val builder = new VectorBuilder[Array[A], A, Int] {
    final class State(var vector: Array[A], var size: Int)

    def init: State = new State(Array.fill(8)(A.zero), 0)

    def update(s: State, i: Int, k: A): State = {
      if (i > s.vector.size) {
        val sz = spire.math.max(highestOneBit(s.vector.size), highestOneBit(i)) << 1
        val tmp = Array.fill(if (sz < 0) Int.MaxValue else sz)(A.zero)
        System.arraycopy(s.vector, 0, tmp, 0, s.vector.length)
        s.vector = tmp
      }

      s.vector(i) = k
      s.size = spire.math.max(s.size, i + 1)
      s
    }

    def result(s: State): Array[A] =
      if (s.vector.size == s.size) s.vector
      else {
        val v = s.vector
        val w = new Array[A](s.size)
        var i = 0
        while (i < w.length) {
          w(i) = v(i)
          i += 1
        }
        w
      }
  }

  // Not really true, but we're pretending Int.MaxValue == Infinity.
  def hasKnownSize: Boolean = false

  def size: Int = ???

  def coord(v: Array[A], i: Int): A = v(i)

  def foreachWithIndex[U](v: Array[A])(f: (Int, A) => U): Unit = {
    var i = 0
    while (i < v.length) {
      f(i, v(i))
      i += 1
    }
  }

  def zipForeachWithIndex[U](v: Array[A], w: Array[A])(f: (Int, A, A) => U): Unit = {
    var i = 0
    while (i < v.length && i < w.length) {
      f(i, v(i), w(i))
      i += 1
    }
    while (i < v.length) {
      f(i, v(i), A.zero)
      i += 1
    }
    while (i < w.length) {
      f(i, A.zero, w(i))
      i += 1
    }
  }

  override def foreach[U](v: Array[A])(f: A => U): Unit =
    v foreach f

  override def map(v: Array[A])(f: A => A): Array[A] =
    v map f

  override def mapWithIndex(v: Array[A])(f: (Int, A) => A): Array[A] = {
    val w = new Array[A](v.length)
    var i = 0
    while (i < v.length) {
      w(i) = f(i, v(i))
      i += 1
    }
    w
  }

  override def zipForeach[U](v: Array[A], w: Array[A])(f: (A, A) => U): Unit = {
    var i = 0
    while (i < v.length && i < w.length) {
      f(v(i), w(i))
      i += 1
    }
    while (i < v.length) {
      f(v(i), A.zero)
      i += 1
    }
    while (i < w.length) {
      f(A.zero, w(i))
      i += 1
    }
  }

  override def zipMap(v: Array[A], w: Array[A])(f: (A, A) => A): Array[A] = {
    val u = new Array[A](spire.math.max(v.length, w.length))
    var i = 0
    while (i < v.length && i < w.length) {
      u(i) = f(v(i), w(i))
      i += 1
    }
    while (i < v.length) {
      u(i) = f(v(i), A.zero)
      i += 1
    }
    while (i < w.length) {
      u(i) = f(A.zero, w(i))
      i += 1
    }
    u
  }

  override def zipMapWithIndex(v: Array[A], w: Array[A])(f: (Int, A, A) => A): Array[A] = {
    val u = new Array[A](spire.math.max(v.length, w.length))
    zipForeachWithIndex(v, w) { (i, x, y) =>
      u(i) = f(i, x, y)
    }
    u
  }
}

@SerialVersionUID(0L)
private final class ArrayInnerProductSpace[@spec(Int,Float,Long,Double) A: ClassTag: Rng]
    extends InnerProductSpace[Array[A], A] with Serializable {
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
