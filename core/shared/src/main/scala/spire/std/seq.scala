package spire
package std

import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

import spire.algebra._
import spire.NoImplicit

@SerialVersionUID(0L)
class SeqCModule[A, SA <: SeqLike[A, SA]](implicit val scalar: CRing[A], cbf: CanBuildFrom[SA,A,SA])
extends CModule[SA, A] with Serializable {
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

@SerialVersionUID(0L)
class SeqVectorSpace[A, SA <: SeqLike[A, SA]](implicit override val scalar: Field[A], cbf: CanBuildFrom[SA,A,SA])
extends SeqCModule[A, SA] with VectorSpace[SA, A] with Serializable

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

@SerialVersionUID(0L)
class SeqEq[A: Eq, SA <: SeqLike[A, SA]] extends Eq[SA] with Serializable {
  def eqv(x: SA, y: SA): Boolean = forall[A, SA](x, y)(Eq[A].eqv(_, _))
}

@SerialVersionUID(0L)
class SeqOrder[A: Order, SA <: SeqLike[A, SA]] extends SeqEq[A, SA] with Order[SA] with Serializable {
  override def eqv(x: SA, y: SA): Boolean = super[SeqEq].eqv(x, y)

  def compare(x: SA, y: SA): Int = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A]): Int = {
      if (xi.hasNext && yi.hasNext) {
        val cmp = Order[A].compare(xi.next(), yi.next())
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
}

@SerialVersionUID(0L)
class SeqVectorEq[A: Eq, SA <: SeqLike[A, SA]](implicit scalar: AdditiveMonoid[A])
extends Eq[SA] with Serializable {
  def eqv(x: SA, y: SA): Boolean =
    forall[A, SA](x, y)(Eq[A].eqv(_, _), Eq[A].eqv(_, scalar.zero))
}

@SerialVersionUID(0L)
class SeqVectorOrder[A: Order, SA <: SeqLike[A, SA]](implicit scalar: AdditiveMonoid[A])
extends SeqVectorEq[A, SA] with Order[SA] with Serializable {
  override def eqv(x: SA, y: SA): Boolean = super[SeqVectorEq].eqv(x, y)

  def compare(x: SA, y: SA): Int = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A]): Int = {
      if (xi.hasNext && yi.hasNext) {
        val cmp = Order[A].compare(xi.next(), yi.next())
        if (cmp == 0) loop(xi, yi) else cmp
      } else if (xi.hasNext) {
        if (Order[A].eqv(xi.next(), scalar.zero)) loop(xi, yi) else 1
      } else if (yi.hasNext) {
        if (Order[A].eqv(yi.next(), scalar.zero)) loop(xi, yi) else -1
      } else {
        0
      }
    }

    loop(x.toIterator, y.toIterator)
  }
}

trait SeqInstances0 {
  implicit def SeqCModule[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      ring0: CRing[A], cbf0: CanBuildFrom[CC[A], A, CC[A]],
      ev: NoImplicit[VectorSpace[CC[A], A]]): SeqCModule[A, CC[A]] = new SeqCModule[A, CC[A]]
}

trait SeqInstances1 extends SeqInstances0 {
  implicit def SeqVectorSpace[A, CC[A] <: SeqLike[A, CC[A]]](implicit field0: Field[A],
      cbf0: CanBuildFrom[CC[A], A, CC[A]]): SeqVectorSpace[A, CC[A]] = new SeqVectorSpace[A, CC[A]]

  implicit def SeqEq[A, CC[A] <: SeqLike[A, CC[A]]](implicit A0: Eq[A]): SeqEq[A, CC[A]] =
    new SeqEq[A, CC[A]]
}

trait SeqInstances2 extends SeqInstances1 {

  implicit def SeqOrder[A, CC[A] <: SeqLike[A, CC[A]]](implicit A0: Order[A]): SeqOrder[A, CC[A]] =
    new SeqOrder[A, CC[A]]
}

trait SeqInstances extends SeqInstances2
