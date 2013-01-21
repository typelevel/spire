package spire.math

import spire.algebra.AdditiveMonoid

import scala.annotation.tailrec
import scala.collection.SeqLike

private object SeqSupport {
  @tailrec
  final def forall[A](x: Iterator[A], y: Iterator[A])(f: (A, A) => Boolean, g: A => Boolean): Boolean = {
    if (x.hasNext && y.hasNext) {
      f(x.next(), y.next()) && forall(x, y)(f, g)
    } else if (x.hasNext) {
      g(x.next()) && forall(x, y)(f, g)
    } else if (y.hasNext) {
      g(y.next()) && forall(x, y)(f, g)
    } else {
      true
    }
  }

  private val falsef: Any => Boolean = _ => false

  @inline final def forall[A, SA <: SeqLike[A, SA]](x: SA, y: SA)(
      f: (A, A) => Boolean, g: A => Boolean = falsef): Boolean = {
    forall(x.toIterator, y.toIterator)(f, g)
  }
}

import SeqSupport._

trait SeqEq[A, SA <: SeqLike[A, SA]] extends Eq[SA] {
  def A: Eq[A]

  def eqv(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.eqv(_, _))
}

trait SeqOrder[A, SA <: SeqLike[A, SA]] extends SeqEq[A, SA] with Order[SA] {
  def A: Order[A]

  def compare(x: SA, y: SA): Int = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A]): Int = {
      if (xi.hasNext && yi.hasNext) {
        val cmp = A.compare(xi.next(), yi.next())
        if (cmp == 0) loop(xi, yi) else cmp
      } else if (xi.hasNext) {
        1
      } else if (yi.hasNext) {
        -1
      } else {
        0
      }
    }

    loop(x.toIterator, y.toIterator)
  }

  override def gt(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.gt(_, _))
  override def lt(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.lt(_, _))
  override def gteqv(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.gteqv(_, _))
  override def lteqv(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.lteqv(_, _))
}

trait SeqVectorEq[A, SA <: SeqLike[A, SA]] extends Eq[SA] {
  def scalar: AdditiveMonoid[A]
  def A: Eq[A]

  def eqv(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.eqv(_, _), A.eqv(_, scalar.zero))
}

trait SeqVectorOrder[A, SA <: SeqLike[A, SA]] extends SeqVectorEq[A, SA] with Order[SA] {
  def A: Order[A]

  def compare(x: SA, y: SA): Int = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A]): Int = {
      if (xi.hasNext && yi.hasNext) {
        val cmp = A.compare(xi.next(), yi.next())
        if (cmp == 0) loop(xi, yi) else cmp
      } else if (xi.hasNext) {
        if (A.eqv(xi.next(), scalar.zero)) loop(xi, yi) else 1
      } else if (yi.hasNext) {
        if (A.eqv(yi.next(), scalar.zero)) loop(xi, yi) else -1
      } else {
        0
      }
    }

    loop(x.toIterator, y.toIterator)
  }

  override def gt(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.gt(_, _))
  override def lt(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.lt(_, _))
  override def gteqv(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.gteqv(_, _), A.eqv(_, scalar.zero))
  override def lteqv(x: SA, y: SA): Boolean = forall[A, SA](x, y)(A.lteqv(_, _), A.eqv(_, scalar.zero))
}
