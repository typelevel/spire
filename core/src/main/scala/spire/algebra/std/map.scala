package spire.algebra
package std

import scala.{ specialized => spec }
import scala.annotation.tailrec

trait MapMonoid[K, V] extends Monoid[Map[K, V]] {
  implicit def scalar: Semigroup[V]

  def id: Map[K, V] = Map.empty

  def op(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.op _
    if (x.size < y.size) { xx = y; yy = x; f = (x: V, y: V) => scalar.op(y, x) }
    yy.foldLeft(xx) { (z, kv) =>
      z.updated(kv._1, (xx get kv._1).map(u => f(u, kv._2)).getOrElse(kv._2))
    }
  }
}

trait MapGroup[K, V] extends MapMonoid[K, V] with Group[Map[K, V]] {
  implicit def scalar: Group[V]

  def inverse(x: Map[K, V]): Map[K, V] = x.mapValues(scalar.inverse)
}

trait MapRng[K, V] extends RingAlgebra[Map[K, V], V] { self =>
  def zero: Map[K, V] = Map.empty

  def plus(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    if (x.size < y.size) { xx = y; yy = x }
    yy.foldLeft(xx) { (z, kv) =>
      z.updated(kv._1, (xx get kv._1).map(u => scalar.plus(u, kv._2)).getOrElse(kv._2))
    }
  }

  def negate(x: Map[K, V]): Map[K, V] = x mapValues (scalar.negate(_))

  def times(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.times _
    if (x.size < y.size) { xx = y; yy = x; f = (x: V, y: V) => scalar.times(y, x) }
    yy.foldLeft(zero) { (z, kv) =>
      (xx get kv._1).map(u => z.updated(kv._1, f(u, kv._2))).getOrElse(z)
    }
  }
  
  def timesl(r: V, v: Map[K, V]): Map[K, V] = v mapValues (scalar.times(r, _))
}

trait MapVectorSpace[K, V] extends MapRng[K, V] with VectorSpace[Map[K, V], V] {
  override def times(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.times _
    if (x.size < y.size) { xx = y; yy = x }
    yy.foldLeft(zero) { (z, kv) =>
      (xx get kv._1).map(u => z.updated(kv._1, scalar.times(u, kv._2))).getOrElse(z)
    }
  }
}

trait MapInnerProductSpace[K, V] extends MapVectorSpace[K, V]
with InnerProductSpace[Map[K, V], V] {
  def dot(x: Map[K, V], y: Map[K, V]): V =
    times(x, y).foldLeft(scalar.zero) { (a, b) => scalar.plus(a, b._2) }
}

trait MapEq[K, V] extends Eq[Map[K, V]] {
  def V: Eq[V]

  def eqv(x: Map[K, V], y: Map[K, V]): Boolean = {
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

trait MapVectorEq[K, V] extends Eq[Map[K, V]] {
  def scalar: AdditiveMonoid[V]
  def V: Eq[V]

  def eqv(x: Map[K, V], y: Map[K, V]): Boolean = {
    @tailrec
    def loop(acc: Map[K, V], it: Iterator[(K, V)]): Boolean = {
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

trait MapInstances0 {
  implicit def MapMonoid[K, V: Semigroup] = new MapMonoid[K, V] {
    val scalar = Semigroup[V]
  }

  implicit def MapRng[K, V: Ring] = new MapRng[K, V] {
    val scalar = Ring[V]
  }
}

trait MapInstances1 extends MapInstances0 {
  implicit def MapGroup[K, V: Group] = new MapGroup[K, V] {
    val scalar = Group[V]
  }

  implicit def MapVectorSpace[K, V: Field] = new MapVectorSpace[K, V] {
    val scalar = Field[V]
  }
}

trait MapInstances2 extends MapInstances1 {
  implicit def MapInnerProductSpace[K, V: Field: NRoot] = new MapInnerProductSpace[K, V] {
    val scalar = Field[V]
    val nroot = NRoot[V]
  }

  implicit def MapEq[K, V](implicit V0: Eq[V]) = new MapEq[K, V] {
    val V = V0
  }
}

trait MapInstances extends MapInstances2
