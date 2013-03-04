package spire.std

import spire.algebra._

import spire.NoImplicit

import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

trait SeqModule[A, SA <: SeqLike[A, SA]] extends Module[SA, A] {
  implicit def cbf: CanBuildFrom[SA, A, SA]

  def zero: SA = cbf().result

  def negate(sa: SA): SA = sa map (scalar.negate)

  def plus(x: SA, y: SA): SA = {
    @tailrec
    def add1(it: Iterator[A], b: Builder[A, SA]): SA = if (it.hasNext) {
      b += it.next()
      add1(it, b)
    } else {
      b.result
    }

    @tailrec
    def add2(xi: Iterator[A], yi: Iterator[A], b: Builder[A, SA]): SA = {
      if (!xi.hasNext) {
        add1(yi, b)
      } else if (!yi.hasNext) {
        add1(xi, b)
      } else {
        b += scalar.plus(xi.next(), yi.next())
        add2(xi, yi, b)
      }
    }

    add2(x.toIterator, y.toIterator, cbf(x))
  }

  override def minus(x: SA, y: SA): SA = {
    @tailrec
    def subl(it: Iterator[A], b: Builder[A, SA]): SA = if (it.hasNext) {
      b += it.next()
      subl(it, b)
    } else {
      b.result
    }

    @tailrec
    def subr(it: Iterator[A], b: Builder[A, SA]): SA = if (it.hasNext) {
      b += scalar.negate(it.next())
      subr(it, b)
    } else {
      b.result
    }

    @tailrec
    def sub2(xi: Iterator[A], yi: Iterator[A], b: Builder[A, SA]): SA = {
      if (!xi.hasNext) {
        subr(yi, b)
      } else if (!yi.hasNext) {
        subl(xi, b)
      } else {
        b += scalar.minus(xi.next(), yi.next())
        sub2(xi, yi, b)
      }
    }

    sub2(x.toIterator, y.toIterator, cbf(x))
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

trait SeqCoordinateSpace[A, SA <: SeqLike[A, SA]] extends SeqInnerProductSpace[A, SA]
with CoordinateSpace[SA, A] {
  def coord(v: SA, i: Int): A = v(i)

  override def dot(v: SA, w: SA): A = super[SeqInnerProductSpace].dot(v, w)

  def axis(i: Int): SA = {
    val b = cbf()

    @tailrec def loop(j: Int): SA = if (i < dimensions) {
      b += (if (i == j) scalar.one else scalar.zero)
      loop(j + 1)
    } else b.result

    loop(0)
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

trait SeqOrder[A, SA <: SeqLike[A, SA]] extends Order[SA] with SeqEq[A, SA] {
  def A: Order[A]

  override def eqv(x: SA, y: SA): Boolean = super.eqv(x, y)

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

trait SeqVectorOrder[A, SA <: SeqLike[A, SA]] extends Order[SA] with SeqVectorEq[A, SA] {
  def A: Order[A]

  override def eqv(x: SA, y: SA): Boolean = super.eqv(x, y)

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

trait SeqInstances0 {
  implicit def SeqModule[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      ring0: Ring[A], cbf0: CanBuildFrom[CC[A], A, CC[A]],
      ev: NoImplicit[VectorSpace[CC[A], A]]) = new SeqModule[A, CC[A]] {
    val scalar = ring0
    val cbf = cbf0
  }
}

trait SeqInstances1 extends SeqInstances0 {
  implicit def SeqVectorSpace[A, CC[A] <: SeqLike[A, CC[A]]](implicit field0: Field[A],
      cbf0: CanBuildFrom[CC[A], A, CC[A]],
      ev: NoImplicit[NormedVectorSpace[CC[A], A]]) = new SeqVectorSpace[A, CC[A]] {
    val scalar = field0
    val cbf = cbf0
  }

  implicit def SeqEq[A, CC[A] <: SeqLike[A, CC[A]]](implicit A0: Eq[A]) = {
    new SeqEq[A, CC[A]] {
      val A = A0
    }
  }
}

trait SeqInstances2 extends SeqInstances1 {
  implicit def SeqInnerProductSpace[A, CC[A] <: SeqLike[A, CC[A]]](implicit field0: Field[A],
      nroot0: NRoot[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]) = new SeqInnerProductSpace[A, CC[A]] {
    val scalar = field0
    val nroot = nroot0
    val cbf = cbf0
  }

  implicit def SeqOrder[A, CC[A] <: SeqLike[A, CC[A]]](implicit A0: Order[A]) = {
    new SeqOrder[A, CC[A]] {
      val A = A0
    }
  }
}

trait SeqInstances extends SeqInstances2
