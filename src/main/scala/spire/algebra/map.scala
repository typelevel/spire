package spire.algebra

import scala.{ specialized => spec }

private object MapSupport {
  @inline final def opf[A](x: A, y: A, flip: Boolean)(implicit A: Semigroup[A]): A = {
    if (flip) A.op(y, x) else A.op(x, y)
  }
}

import MapSupport._

trait MapMonoid[K, V] extends Monoid[Map[K, V]] {
  implicit def scalar: Semigroup[V]

  def id: Map[K, V] = Map.empty

  def op(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    if (x.size < y.size) plus0(y, x, true) else plus0(x, y, false)

  @inline private final def plus0(x: Map[K, V], y: Map[K, V], flip: Boolean): Map[K, V] = {
    y.foldLeft(x) { (z, kv) =>
      z + ((kv._1, (x get kv._1) match {
        case Some(u) => opf(u, kv._2, flip)
        case None => kv._2
      }))
    }
  }
}

trait MapGroup[K, V] extends MapMonoid[K, V] with Group[Map[K, V]] {
  implicit def scalar: Group[V]

  def inverse(x: Map[K, V]): Map[K, V] = x mapValues (scalar.inverse(_))
}

trait MapRng[K, V] extends RingAlgebra[Map[K, V], V] { self =>
  def zero: Map[K, V] = Map.empty

  def plus(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    if (x.size < y.size) plus0(y, x) else plus0(x, y)

  def negate(x: Map[K, V]): Map[K, V] = x mapValues (scalar.negate(_))

  def times(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    if (x.size < y.size) times0(y, x, true) else times0(x, y, false)
  
  def timesl(r: V, v: Map[K, V]): Map[K, V] = v mapValues (scalar.times(r, _))

  // Rng addition is commutative, so we can optimize a bit.
  @inline private final def plus0(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    y.foldLeft(x) { (z, kv) =>
      z + ((kv._1, (x get kv._1) match {
        case Some(u) => scalar.plus(u, kv._2)
        case None => kv._2
      }))
    }
  }

  @inline private final def times0(x: Map[K, V], y: Map[K, V], flip: Boolean): Map[K, V] = {
    y.foldLeft(zero) { (z, kv) =>
      (x get kv._1) match {
        case Some(u) =>
          z + ((kv._1, if (flip) scalar.times(kv._2, u) else scalar.times(u, kv._2)))
        case None =>
          z
      }
    }
  }
}

trait MapVectorSpace[K, V] extends MapRng[K, V] with VectorSpace[Map[K, V], V] {
  override def times(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    if (x.size < y.size) times0(y, x, true) else times0(x, y, false)
  
  // Fields are commutative, so optimize a bit.
  @inline private final def times0(x: Map[K, V], y: Map[K, V], flip: Boolean): Map[K, V] = {
    y.foldLeft(zero) { (z, kv) =>
      (x get kv._1) match {
        case Some(u) => z + ((kv._1, scalar.times(u, kv._2)))
        case None => z
      }
    }
  }
}

trait MapInnerProductSpace[K, V] extends MapVectorSpace[K, V]
with InnerProductSpace[Map[K, V], V] {
  def dot(x: Map[K, V], y: Map[K, V]): V =
    times(x, y).foldLeft(scalar.zero) { (a, b) => scalar.plus(a, b._2) }
}

trait MapAlgebra0 {
  implicit def MapMonoid[K, V: Semigroup] = new MapMonoid[K, V] {
    val scalar = Semigroup[V]
  }

  implicit def MapRng[K, V: Ring] = new MapRng[K, V] {
    val scalar = Ring[V]
  }
}

trait MapAlgebra1 extends MapAlgebra0 {
  implicit def MapGroup[K, V: Group] = new MapGroup[K, V] {
    val scalar = Group[V]
  }

  implicit def MapVectorSpace[K, V: Field] = new MapVectorSpace[K, V] {
    val scalar = Field[V]
  }
}

trait MapAlgebra2 extends MapAlgebra1 {
  implicit def MapInnerProductSpace[K, V: Field: NRoot] = new MapInnerProductSpace[K, V] {
    val scalar = Field[V]
    val nroot = NRoot[V]
  }
}

trait MapAlgebra extends MapAlgebra2

object map extends MapAlgebra
