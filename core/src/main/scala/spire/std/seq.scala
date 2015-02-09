package spire.std

import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

import spire.algebra._
import spire.NoImplicit

@SerialVersionUID(0L)
class SeqVectorSpace[A, SA <: SeqLike[A, SA]](implicit val scalar: Rng[A], cbf: CanBuildFrom[SA,A,SA])
extends VectorSpace[SA, A] with Serializable {
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
/**
 * The L_p norm is equal to the `p`-th root of the sum of each element to the
 * power `p`. For instance, if `p = 1` we have the Manhattan distance. If you'd
 * like the Euclidean norm (`p = 2`), then you'd probably be best to use an
 * `InnerProductSpace` instead.
 */
@SerialVersionUID(0L)
class SeqLpNormedVectorSpace[A: Field: NRoot: Signed, SA <: SeqLike[A, SA]](val p: Int)(implicit cbf: CanBuildFrom[SA,A,SA])
extends SeqVectorSpace[A, SA] with NormedVectorSpace[SA, A] with Serializable {
  require(p > 0, "p must be > 0")

  def norm(v: SA): A = {
    @tailrec
    def loop(xi: Iterator[A], acc: A): A = {
      if (xi.hasNext) {
        loop(xi, scalar.plus(acc, Signed[A].abs(scalar.pow(xi.next(), p))))
      } else {
        NRoot[A].nroot(acc, p)
      }
    }

    loop(v.toIterator, scalar.zero)
  }
}

/**
 * The norm here uses the absolute maximum of the coordinates (ie. the L_inf
 * norm).
 */
@SerialVersionUID(0L)
class SeqMaxNormedVectorSpace[A: Rng: Order: Signed, SA <: SeqLike[A, SA]](implicit cbf: CanBuildFrom[SA,A,SA]) 
extends SeqVectorSpace[A, SA] with NormedVectorSpace[SA, A] with Serializable {
  def norm(v: SA): A = {
    @tailrec
    def loop(xi: Iterator[A], acc: A): A = {
      if (xi.hasNext) {
        val x = Signed[A].abs(xi.next())
        loop(xi, if (Order[A].gt(x, acc)) x else acc)
      } else {
        acc
      }
    }

    loop(v.toIterator, Rng[A].zero)
  }
}

@SerialVersionUID(0L)
private final class SeqBasis[A, SA <: SeqLike[A, SA]]
    (implicit A: AdditiveMonoid[A], cbf: CanBuildFrom[SA,A,SA])
    extends Frame[SA, A] with Serializable {
  val builder = new VectorBuilder[SA, A, Int] {
    type State = Map[Int, A]

    def init: State = Map.empty

    def update(s: State, i: Int, k: A): State =
      s + (i -> k)

    def result(s: State): SA =
      if (s.isEmpty) cbf().result()
      else {
        val size = s.maxBy(_._1)._1
        val bldr = cbf()
        var i = 0
        while (i < size) {
          bldr += s.getOrElse(i, A.zero)
          i += 1
        }
        bldr.result()
      }
  }

  // Not really true, but we're pretending Int.MaxValue == Infinity.
  def hasKnownSize: Boolean = false

  def size: Int = ???

  def coord(v: SA, i: Int): A = v(i)

  def foreachWithIndex[U](v: SA)(f: (Int, A) => U): Unit =
    v.foldLeft(0) { (i, x) => f(i, x); i + 1 }

  def zipForeachWithIndex[U](v: SA, w: SA)(f: (Int, A, A) => U): Unit = {
    val vi = v.iterator
    val wi = w.iterator
    var i = 0
    while (vi.hasNext && wi.hasNext) {
      f(i, vi.next(), wi.next())
      i += 1
    }
    while (vi.hasNext) {
      f(i, vi.next(), A.zero)
      i += 1
    }
    while (wi.hasNext) {
      f(i, wi.next(), A.zero)
      i += 1
    }
  }

  override def foreach[U](v: SA)(f: A => U): Unit =
    v foreach f

  override def map(v: SA)(f: A => A): SA =
    v map f

  override def mapWithIndex(v: SA)(f: (Int, A) => A): SA = {
    val it = v.iterator
    var i = 0
    val bldr = cbf()
    while (it.hasNext) {
      bldr += f(i, it.next())
      i += 1
    }
    bldr.result()
  }

  override def zipMapWithIndex(v: SA, w: SA)(f: (Int, A, A) => A): SA = {
    val vi = v.iterator
    val wi = w.iterator
    var i = 0
    val bldr = cbf()
    while (vi.hasNext && wi.hasNext) {
      bldr += f(i, vi.next(), wi.next())
      i += 1
    }
    while (vi.hasNext) {
      bldr += f(i, vi.next(), A.zero)
      i += 1
    }
    while (wi.hasNext) {
      bldr += f(i, wi.next(), A.zero)
      i += 1
    }
    bldr.result()
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
