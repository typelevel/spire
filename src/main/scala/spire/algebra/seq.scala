package spire.algebra

import spire.math.Order

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

/**
 * The L_p norm is equal to the `p`-th root of the sum of each element to the
 * power `p`. For instance, if `p = 1` we have the Manhattan distance. If you'd
 * like the Euclidean norm (`p = 2`), then you'd probably be best to use an
 * `InnerProductSpace` instead.
 */
trait SeqLpNormedVectorSpace[A, SA <: SeqLike[A, SA]] extends SeqVectorSpace[A, SA]
with NormedVectorSpace[SA, A] {
  def nroot: NRoot[A]
  def signed: Signed[A]

  def p: Int

  def norm(v: SA): A = {
    @tailrec
    def loop(xi: Iterator[A], acc: A): A = {
      if (xi.hasNext) {
        loop(xi, scalar.plus(acc, signed.abs(scalar.pow(xi.next(), p))))
      } else {
        nroot.nroot(acc, p)
      }
    }

    loop(v.toIterator, scalar.zero)
  }
}

/**
 * The norm here uses the absolute maximum of the coordinates (ie. the L_inf
 * norm).
 */
trait SeqMaxNormedVectorSpace[A, SA <: SeqLike[A, SA]] extends SeqVectorSpace[A, SA]
with NormedVectorSpace[SA, A] {
  def order: Order[A]
  def signed: Signed[A]

  def norm(v: SA): A = {
    @tailrec
    def loop(xi: Iterator[A], acc: A): A = {
      if (xi.hasNext) {
        val x = signed.abs(xi.next())
        loop(xi, if (order.gt(x, acc)) x else acc)
      } else {
        acc
      }
    }

    loop(v.toIterator, scalar.zero)
  }
}
