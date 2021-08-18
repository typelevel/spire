package spire
package example

import spire.implicits._
import spire.math._

import scala.collection.immutable.LazyList
import scala.collection.mutable.Builder

/**
 * Some tools for simplifying decimal expressions, and playing around
 * with numbers.
 *
 * There are three modes:
 *
 * nth: print the nth rational, according to diagonalization
 * all: print the first n rationals, according to diagonalization
 * snap: given y, look for solutions to y = nroot(x, k) / d
 */
object Simplification {
  //
  // def main(args: Array[String]): Unit = {
  //   if (args.isEmpty) {
  //     println("usage: %s [nrat | rats | nprime | primes | snap] [number]")
  //   } else {
  //     args(0) match {
  //       case "nrat" =>
  //         val n = if (args.length == 1) 10 else args(1).toInt
  //         val r: Rational = rationals.drop(n - 1).head
  //         println("rational %d is %s".format(n, r.toString))
  //       case "rats" =>
  //         val n = if (args.length == 1) 10 else args(1).toInt
  //         rationals.take(n).foreach(r => print(r.toString + ", "))
  //         println("...")
  //       case "nprime" =>
  //         val n = if (args.length == 1) 10 else args(1).toInt
  //         val p: Int = primes.drop(n - 1).head
  //         println("rational %d is %s".format(n, p.toString))
  //       case "primes" =>
  //         val n = if (args.length == 1) 10 else args(1).toInt
  //         primes.take(n).foreach(p => print(p.toString + ", "))
  //         println("...")
  //       case "snap" =>
  //         val n = if (args.length == 1) 1.4142135623730951 else args(1).toDouble
  //         val (base, k, div) = snap(n)
  //         println("%s =~ nroot(%s, %s) / %s".format(n, base, k, div))
  //     }
  //   }
  // }
  //
  // /**
  //  * Using Cantor's diagonalization method, create an infinite stream
  //  * of all rational numbers.
  //  *
  //  * This stream will only be able to generate the first
  //  * 42,535,295,865,117,307,928,310,139,910,543,638,528 values, so it
  //  * is not really infinite. Even so, it's unlikely that a user will
  //  * be able to generate this many values.
  //  */
  // val rationals: BigStream[Rational] = {
  //   @tailrec
  //   def next(i: Long, n: Long, d: Long): BigStream[Rational] = {
  //     if (n == 0L) {
  //       next(i + 1L, i, 1L)
  //     } else {
  //       val r = Rational(n, d)
  //       if (n == r.numeratorAsLong) {
  //         new BigCons(r, new BigCons(-r, loop(i, n - 1L, d + 1L)))
  //       } else {
  //         next(i, n - 1L, d + 1L)
  //       }
  //     }
  //   }
  //
  //   def loop(i: Long, n: Long, d: Long): BigStream[Rational] = next(i, n, d)
  //
  //   Rational.zero #:: loop(2L, 1L, 1L)
  // }
  //
  // /**
  //  * Naive prime lazy list. For each odd number, this method tries
  //  * dividing by all previous primes <= sqrt(n).
  //  *
  //  * There are a lot of ways to improve this. For now it's a toy.
  //  * It can generate the millionth prime in ~9s on my computer.
  //  */
  // val primes: LazyList[Int] = {
  //   @tailrec
  //   def next(n: Int, ll: LazyList[Int]): LazyList[Int] =
  //     if (ll.isEmpty || (ll.head ** 2) > n)
  //       n #:: loop(n + 2, primes)
  //     else if (n % ll.head == 0)
  //       next(n + 2, primes)
  //     else
  //       next(n, ll.tail)
  //
  //   def loop(n: Int, ll: LazyList[Int]): LazyList[Int] = next(n, ll)
  //
  //   2 #:: loop(3, primes)
  // }
  //
  // /**
  //  * Given a Double y, look for whole numbers x, k, and d such that:
  //  *
  //  *   y = nroot(x, k) / d
  //  *
  //  * The limit (default: 10) describes the largest root (and divisor)
  //  * that will be checked. The epsilon (default: 0.00000000001)
  //  * describes the maximum distance we can shift the value to find an
  //  * "exact" match.
  //  */
  // def snap(n: Double, limit: Int = 10, epsilon: Double = 0.00000000001): (Double, Int, Int) = {
  //   @tailrec
  //   def loop(i: Int, ex: Int, div: Int): (Double, Int, Int) = {
  //     if (i >= limit) {
  //       (n, 1, 1)
  //     } else if (div < 1) {
  //       loop(i + 1, 1, i + 1)
  //     } else {
  //       val x = math.pow(n * div, ex)
  //       val m = x % 1.0
  //       val d = if (m < 0.5) m else m - 1.0
  //       if (math.abs(d) < epsilon) {
  //         (x - m, ex, div)
  //       } else {
  //         loop(i, ex + 1, div - 1)
  //       }
  //     }
  //   }
  //   if (n < 0.0) {
  //     val (x, k, div) = snap(-n, limit, epsilon)
  //     (x, k, -div)
  //   } else {
  //     loop(1, 1, 1)
  //   }
  // }
}

/**
 * BigStream is a non-memoizing stream.
 *
 * It's similar to Scala's Stream[A] except that it won't exhaust your
 * memory for very large streams. This makes it useful for situations
 * where re-computing the stream is preferrable to trying to store
 * all the results in memory for next time.
 */
object BigStream extends BigStreamCompanionCompat {
  def empty[A]: BigStream[A] = BigNil[A]()

  implicit class Wrapper[A](t: => BigStream[A]) {
    def #::(a: A): BigStream[A] = new BigCons(a, t)
  }

  def newBuilder[A]: Builder[A, BigStream[A]] =
    new Builder[A, BigStream[A]] {
      private var elems: List[A] = Nil
      def addOne(a: A): this.type = {
        elems = a :: elems
        this
      }
      def clear(): Unit = elems = Nil
      override def result(): BigStream[A] =
        elems.foldLeft(BigStream.empty[A])((t, a) => new BigCons(a, t))
    }
}

trait BigStream[A] extends Iterable[A] with BigStreamCompat[A] { self =>

  override def take(n: Int): BigStream[A] =
    if (isEmpty || n < 1) BigNil() else new BigCons(head, tail.take(n - 1))

  override def drop(n: Int): BigStream[A] = {
    @tailrec
    def loop(stream: BigStream[A], i: Int): BigStream[A] =
      if (isEmpty || i < 1) stream else loop(stream.tail, i - 1)
    loop(this, n)
  }

  def iterator: Iterator[A] = new Iterator[A] {
    var stream = self

    def hasNext: Boolean = !stream.isEmpty

    override def next(): A = if (stream.isEmpty) {
      throw new NoSuchElementException
    } else {
      val a = stream.head
      stream = stream.tail
      a
    }
  }

  override def foreach[U](f: A => U): Unit = {
    @tailrec
    def loop(stream: BigStream[A]): Unit = if (!stream.isEmpty) {
      f(stream.head)
      loop(stream.tail)
    }
    loop(this)
  }
}

class BigCons[A](override val head: A, t: => BigStream[A]) extends BigStream[A] {
  override def tail: BigStream[A] = t
  override def isEmpty = false
  override def toString: String = "BigStream(%s, ...)".format(head.toString)
  override def equals(rhs: Any): Boolean = rhs match {
    case s: BigStream[_] => !s.isEmpty && tail == s.tail
    case _               => false
  }
}

case class BigNil[A]() extends BigStream[A] {
  override def head: A = sys.error("head on nil")
  override def tail: BigStream[A] = sys.error("tail on nil")
  override def isEmpty = true
  override def toString: String = "BigStream()"
}
