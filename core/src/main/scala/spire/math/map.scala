package spire.math

import spire.algebra.AdditiveMonoid

import scala.{ specialized => spec }
import scala.annotation.tailrec

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

