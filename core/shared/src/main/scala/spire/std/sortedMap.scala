package spire
package std

import spire.algebra._
import scala.collection.SortedMap

@SerialVersionUID(0L)
class SortedMapMonoid[K: Ordering, V](implicit val scalar: Semigroup[V]) extends Monoid[SortedMap[K, V]]
with Serializable {
  def id: SortedMap[K, V] = SortedMap.empty

  def op(x: SortedMap[K, V], y: SortedMap[K, V]): SortedMap[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.op _
    if (x.size < y.size) { xx = y; yy = x; f = (x: V, y: V) => scalar.op(y, x) }
    yy.foldLeft(xx) { (z, kv) =>
      z.updated(kv._1, (xx get kv._1).map(u => f(u, kv._2)).getOrElse(kv._2))
    }
  }
}

@SerialVersionUID(0L)
class SortedMapGroup[K: Ordering, V](implicit override val scalar: Group[V]) extends SortedMapMonoid[K, V]
with Group[SortedMap[K, V]] with Serializable {
  def inverse(x: SortedMap[K, V]): SortedMap[K, V] = x.mapValues(scalar.inverse)
}

@SerialVersionUID(0L)
class SortedMapSemiring[K: Ordering, V](implicit val scalar: Semiring[V]) extends Semiring[SortedMap[K, V]] with Serializable {

  def zero: SortedMap[K, V] = SortedMap.empty

  def plus(x: SortedMap[K, V], y: SortedMap[K, V]): SortedMap[K, V] = {
    var xx = x
    var yy = y
    if (x.size < y.size) { xx = y; yy = x }
    yy.foldLeft(xx) { (z, kv) =>
      z.updated(kv._1, (xx get kv._1).map(u => scalar.plus(u, kv._2)).getOrElse(kv._2))
    }
  }

  def times(x: SortedMap[K, V], y: SortedMap[K, V]): SortedMap[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.times _
    if (x.size < y.size) { xx = y; yy = x; f = (x: V, y: V) => scalar.times(y, x) }
    yy.foldLeft(zero) { (z, kv) =>
      (xx get kv._1).map(u => z.updated(kv._1, f(u, kv._2))).getOrElse(z)
    }
  }
}

@SerialVersionUID(0L)
class SortedMapRng[K: Ordering, V](override implicit val scalar: Rng[V]) extends SortedMapSemiring[K, V] with RingAlgebra[SortedMap[K, V], V] with Serializable { self =>
  def negate(x: SortedMap[K, V]): SortedMap[K, V] = x mapValues (scalar.negate(_))

  def timesl(r: V, v: SortedMap[K, V]): SortedMap[K, V] = v mapValues (scalar.times(r, _))
}

@SerialVersionUID(0L)
class SortedMapVectorSpace[K: Ordering, V](override implicit val scalar: Field[V]) extends SortedMapRng[K, V] with VectorSpace[SortedMap[K, V], V] with Serializable {
  override def times(x: SortedMap[K, V], y: SortedMap[K, V]): SortedMap[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.times _
    if (x.size < y.size) { xx = y; yy = x }
    yy.foldLeft(zero) { (z, kv) =>
      (xx get kv._1).map(u => z.updated(kv._1, scalar.times(u, kv._2))).getOrElse(z)
    }
  }
}

@SerialVersionUID(0L)
class SortedMapInnerProductSpace[K: Ordering, V: Field] extends SortedMapVectorSpace[K, V] with InnerProductSpace[SortedMap[K, V], V] with Serializable {
  def dot(x: SortedMap[K, V], y: SortedMap[K, V]): V =
    times(x, y).foldLeft(scalar.zero) { (a, b) => scalar.plus(a, b._2) }
}

@SerialVersionUID(0L)
class SortedMapEq[K: Ordering, V](implicit V: Eq[V]) extends Eq[SortedMap[K, V]] with Serializable {
  def eqv(x: SortedMap[K, V], y: SortedMap[K, V]): Boolean = {
    if (x.size != y.size) false else {
      x forall { case (k, v) =>
        (y get k) match {
          case Some(e) if V.eqv(e, v) => true
          case _ => false
        }
      }
    }
  }
}

@SerialVersionUID(0L)
class SortedMapVectorEq[K: Ordering, V](implicit V: Eq[V], scalar: AdditiveMonoid[V]) extends Eq[SortedMap[K, V]] with Serializable {
  def eqv(x: SortedMap[K, V], y: SortedMap[K, V]): Boolean = {
    @tailrec
    def loop(acc: SortedMap[K, V], it: Iterator[(K, V)]): Boolean = {
      if (it.hasNext) {
        val (k, v0) = it.next()
        (acc get k) match {
          case Some(v1) if V.eqv(v0, v1) =>
            loop(acc - k, it)
          case None if V.eqv(v0, scalar.zero) =>
            loop(acc - k, it)
          case _ =>
            false
        }
      } else {
        acc forall { case (_, v) => V.eqv(v, scalar.zero) }
      }
    }

    loop(x, y.toIterator)
  }
}

trait SortedMapInstances0 {
  implicit def SortedMapMonoid[K: Ordering, V: Semigroup]: SortedMapMonoid[K, V] = new SortedMapMonoid[K, V]

  implicit def SortedMapSemiring[K: Ordering, V: Semiring]: SortedMapSemiring[K, V] = new SortedMapSemiring[K, V]
}

trait SortedMapInstances1 extends SortedMapInstances0 {
  implicit def SortedMapRng[K: Ordering, V: Rng]: SortedMapRng[K, V] = new SortedMapRng[K, V]
}

trait SortedMapInstances2 extends SortedMapInstances1 {
  implicit def SortedMapGroup[K: Ordering, V: Group]: SortedMapGroup[K, V] = new SortedMapGroup[K, V]

  implicit def SortedMapVectorSpace[K: Ordering, V: Field]: SortedMapVectorSpace[K, V] = new SortedMapVectorSpace[K, V]
}

trait SortedMapInstances3 extends SortedMapInstances2 {
  implicit def SortedMapInnerProductSpace[K: Ordering, V: Field]: SortedMapInnerProductSpace[K, V] = new SortedMapInnerProductSpace[K, V]

  implicit def SortedMapEq[K: Ordering, V](implicit V0: Eq[V]): SortedMapEq[K, V] = new SortedMapEq[K, V]
}

trait SortedMapInstances extends SortedMapInstances3

