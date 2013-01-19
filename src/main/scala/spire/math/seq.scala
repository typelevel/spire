package spire.math

import scala.annotation.tailrec
import scala.collection.SeqLike

trait SeqEq[A, SA <: SeqLike[A, SA]] extends Eq[SA] {
  def A: Eq[A]

  protected final def forall(x: SA, y: SA)(f: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A]): Boolean = {
      if (xi.hasNext && yi.hasNext) {
        if (f(xi.next(), yi.next())) loop(xi, yi) else false
      } else if (xi.hasNext || yi.hasNext) {
        false
      } else {
        true
      }
    }

    loop(x.toIterator, y.toIterator)
  }

  def eqv(x: SA, y: SA): Boolean = forall(x, y)(A.eqv(_, _))
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

  override def gt(x: SA, y: SA): Boolean = forall(x, y)(A.gt(_, _))
  override def lt(x: SA, y: SA): Boolean = forall(x, y)(A.lt(_, _))
  override def gteqv(x: SA, y: SA): Boolean = forall(x, y)(A.gteqv(_, _))
  override def lteqv(x: SA, y: SA): Boolean = forall(x, y)(A.lteqv(_, _))
}
