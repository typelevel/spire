package spire.algebra

import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

trait SeqModule[A, SA <: SeqLike[A, SA]] extends Module[SA, A] {
  implicit def cbf: CanBuildFrom[SA, A, SA]

  def zero: SA = cbf().result

  def negate(sa: SA): SA = sa map (scalar.negate(_))

  def plus(x: SA, y: SA): SA = {
    @tailrec
    def add(xi: Iterator[A], yi: Iterator[A], b: Builder[A, SA]): SA = {
      val contxi = xi.hasNext
      val contyi = yi.hasNext

      if (contxi || contyi) {
        b += (if (contxi && contyi) {
          (scalar.plus(xi.next(), yi.next()))
        } else if (contxi) {
          xi.next()
        } else {
          yi.next()
        })
        add(xi, yi, b)
      } else {
        b.result
      }
    }

    add(x.toIterator, y.toIterator, cbf(x))
  }

  override def minus(x: SA, y: SA): SA = {
    @tailrec
    def sub(xi: Iterator[A], yi: Iterator[A], b: Builder[A, SA]): SA = {
      val contxi = xi.hasNext
      val contyi = yi.hasNext

      if (contxi || contyi) {
        b += (if (contxi && contyi) {
          (scalar.minus(xi.next(), yi.next()))
        } else if (contxi) {
          xi.next()
        } else {
          scalar.negate(yi.next())
        })
        sub(xi, yi, b)
      } else {
        b.result
      }
    }

    sub(x.toIterator, y.toIterator, cbf(x))
  }

  def timesl(r: A, sa: SA): SA = sa map (scalar.times(r, _))
}

trait SeqVectorSpace[A, SA <: SeqLike[A, SA]] extends SeqModule[A, SA] with VectorSpace[SA, A]

trait SeqInnerProductSpace[A, SA <: SeqLike[A, SA]] extends SeqVectorSpace[A, SA]
with InnerProductSpace[SA, A] {
  def dot(x: SA, y: SA): A = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A], acc: A): A = {
      if (xi.hasNext && yi.hasNext) {
        loop(xi, yi, scalar.plus(acc, scalar.times(xi.next(), yi.next())))
      } else {
        acc
      }
    }

    loop(x.toIterator, y.toIterator, scalar.zero)
  }
}
