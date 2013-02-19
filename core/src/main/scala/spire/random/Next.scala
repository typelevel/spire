package spire.random

import spire.algebra._
import spire.math._

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

trait Next[@spec A] { self =>
  def apply(gen: Generator): A

  final def get(implicit gen: Generator): A = apply(gen)

  final def map[B](f: A => B) = new NextFromGen(g => f(apply(g)))

  final def flatMap[B](f: A => Next[B]) = new NextFromGen(g => f(apply(g))(g))

  final def filter(f: A => Boolean): Next[A] = new Next[A] {
    @tailrec final def apply(gen: Generator): A = {
      val a = self(gen)
      if (f(a)) a else apply(gen)
    }
  }

  final def zip[B](that: Next[B]): Next[(A, B)] =
    new NextFromGen(g => (this(g), that(g)))

  final def toIterator(gen: Generator) = new NextIterator(this, gen)

  final def toStream(gen: Generator): Stream[A] = this(gen) #:: toStream(gen)
}

final class NextIterator[A](next: Next[A], gen: Generator) extends Iterator[A] {
  final def hasNext(): Boolean = true
  final def next(): A = next(gen)
}

class NextFromGen[@spec A](f: Generator => A) extends Next[A] {
  def apply(gen: Generator): A = f(gen)
}

trait NextRing[A] extends Ring[Next[A]] {
  def alg: Ring[A]
  def zero: Next[A] = Next.constant(alg.zero)
  def one: Next[A] = Next.constant(alg.one)
  def plus(x: Next[A], y: Next[A]) = new NextFromGen(g => alg.plus(x(g), y(g)))
  def negate(x: Next[A]) = new NextFromGen(g => alg.negate(x(g)))
  def times(x: Next[A], y: Next[A]) = new NextFromGen(g => alg.times(x(g), y(g)))
}

trait NextEuclideanRing[A] extends EuclideanRing[Next[A]] with NextRing[A] {
  def alg: EuclideanRing[A]
  def quot(x: Next[A], y: Next[A]) = new NextFromGen(g => alg.quot(x(g), y(g)))
  def mod(x: Next[A], y: Next[A]) = new NextFromGen(g => alg.mod(x(g), y(g)))
  def gcd(x: Next[A], y: Next[A]) = new NextFromGen(g => alg.gcd(x(g), y(g)))
}

trait NextField[A] extends Field[Next[A]] with NextEuclideanRing[A] {
  def alg: Field[A]
  def div(x: Next[A], y: Next[A]) = new NextFromGen(g => alg.div(x(g), y(g)))
  def ceil(x: Next[A]) = new NextFromGen(g => alg.ceil(x(g)))
  def floor(x: Next[A]) = new NextFromGen(g => alg.floor(x(g)))
  def round(x: Next[A]) = new NextFromGen(g => alg.round(x(g)))
  def isWhole(x: Next[A]) = false
}

object Next {
  @inline final def apply[A](implicit na: Next[A]) = na

  import spire.algebra.Ring

  implicit def ring[A](implicit ev: Ring[A]) = new NextRing[A] { def alg = ev }
  implicit def euclideanRing[A](implicit ev: EuclideanRing[A]) = new NextEuclideanRing[A] { def alg = ev }
  implicit def field[A](implicit ev: Field[A]) = new NextField[A] { def alg = ev }

  final def apply[A, B](f: A => B)(implicit na: Next[A]): Next[B] =
    na.map(f)

  final def apply[A, B, C](f: (A, B) => C)(implicit na: Next[A], nb: Next[B]): Next[C] =
    new NextFromGen(g => f(na(g), nb(g)))

  final def gen[A](f: Generator => A): Next[A] =
    new NextFromGen(g => f(g))

  def reduce[A](ns: Next[A]*)(f: (A, A) => A): Next[A] =
    new NextFromGen(g => ns.map(_(g)).reduceLeft(f))

  def fromBytes[A](n: Int)(f: Array[Byte] => A): Next[A] =
    new NextFromGen(g => f(g.generateBytes(n)))

  def fromInts[A](n: Int)(f: Array[Int] => A): Next[A] =
    new NextFromGen(g => f(g.generateInts(n)))

  def fromLongs[A](n: Int)(f: Array[Long] => A): Next[A] =
    new NextFromGen(g => f(g.generateLongs(n)))

  implicit val unit: Next[Unit] = new NextFromGen[Unit](g => ())
  implicit val boolean: Next[Boolean] = new NextFromGen[Boolean](_.nextBoolean)
  implicit val byte: Next[Byte] = new NextFromGen[Byte](_.nextInt.toByte)
  implicit val short: Next[Short] = new NextFromGen[Short](_.nextInt.toShort)
  implicit val char: Next[Char] = new NextFromGen[Char](_.nextInt.toChar)
  implicit val int: Next[Int] = new NextFromGen[Int](_.nextInt)
  implicit val float: Next[Float] = new NextFromGen[Float](_.nextFloat)
  implicit val long: Next[Long] = new NextFromGen[Long](_.nextLong)
  implicit val double: Next[Double] = new NextFromGen[Double](_.nextDouble)

  implicit val ubyte: Next[UByte] = new NextFromGen[UByte](g => UByte(g.nextInt))
  implicit val ushort: Next[UShort] = new NextFromGen[UShort](g => UShort(g.nextInt))
  implicit val uint: Next[UInt] = new NextFromGen[UInt](g => UInt(g.nextInt))
  implicit val ulong: Next[ULong] = new NextFromGen[ULong](g => ULong(g.nextLong))

  implicit def complex[A: Fractional: Trig: Next]: Next[Complex[A]] =
    Next(Complex(_: A, _: A))

  def intrange(from: Int, to: Int) = {
    val d = to - from + 1
    new NextFromGen(_.nextInt(d) + from)
  }

  def natural(maxDigits: Int) = new Next[Natural] {
    @tailrec
    private def loop(g: Generator, i: Int, size: Int, n: Natural): Natural =
      if (i < size) loop(g, i + 1, size, Natural.Digit(g.next[UInt], n)) else n

    def apply(gen: Generator): Natural =
      loop(gen, 1, gen.nextInt(maxDigits) + 1, Natural.End(gen.next[UInt]))
  }

  def safelong(maxBytes: Int): Next[SafeLong] = if (maxBytes <= 0) {
    throw new IllegalArgumentException("need positive maxBytes, got %s" format maxBytes)
  } else if (maxBytes < 8) {
    val n = (8 - maxBytes) * 8
    new NextFromGen(g => SafeLong(g.nextLong >>> n))
  } else if (maxBytes == 8) {
    new NextFromGen(g => SafeLong(g.nextLong))
  } else  {
    bigint(maxBytes).map(SafeLong(_))
  }

  def bigint(maxBytes: Int) = new Next[BigInt] {
    def apply(gen: Generator): BigInt = BigInt(gen.generateBytes(gen.nextInt(maxBytes) + 1))
  }

  def bigdecimal(maxBytes: Int, maxScale: Int) = new Next[BigDecimal] {
    private val nb = bigint(maxBytes)
    def apply(gen: Generator): BigDecimal = BigDecimal(nb(gen), gen.nextInt(maxScale) + 1)
  }

  implicit def rational(implicit next: Next[BigInt]) =
    Next(Rational(_: BigInt, _: BigInt))(next, next.filter(_ != 0))

  def longrational: Next[Rational] =
    Next(Rational(_: Long, _: Long))(Next[Long], Next[Long].filter(_ != 0))

  def bigrational(maxBytes: Int): Next[Rational] =
    rational(bigint(maxBytes))

  def constant[A](a: A) = new Next[A] {
    def apply(gen: Generator): A = a
  }

  implicit def interval[A](implicit na: Next[A], order: Order[A]) =
    Next((x: A, y: A) => if (order.lt(x, y)) Interval(x, y) else Interval(y, x))

  implicit def option[A](implicit no: Next[Boolean], na: Next[A]) =
    new NextFromGen(g => if (no(g)) Some(na(g)) else None)

  implicit def either[A, B](implicit no: Next[Boolean], na: Next[A], nb: Next[B]) =
    new NextFromGen[Either[A, B]](g => if (no(g)) Right(nb(g)) else Left(na(g)))

  implicit def tuple2[A: Next, B: Next] = Next((_: A, _: B))

  implicit def list[A: Next](minSize: Int, maxSize: Int): Next[List[A]] =
    new Next[List[A]] {
      private val d = maxSize - minSize + 1
      private def loop(g: Generator, n: Int, sofar: List[A]): List[A] =
        if (n > 0) loop(g, n - 1, g.next[A] :: sofar) else sofar

      def apply(gen: Generator): List[A] =
        loop(gen, gen.nextInt(d) + minSize, Nil)
    }

  implicit def set[A: Next](minInputs: Int, maxInputs: Int): Next[Set[A]] =
    list[A](minInputs, maxInputs).map(_.toSet)

  implicit def map[A: Next, B: Next](minInputs: Int, maxInputs: Int): Next[Map[A, B]] =
    list[(A, B)](minInputs, maxInputs).map(_.toMap)

  def oneOf[A: ClassTag](as: A*) = new Next[A] {
    private val arr = as.toArray
    private val len = arr.length
    def apply(gen: Generator): A = arr(gen.nextInt(len))
  }

  def cycleOf[A: ClassTag](as: A*) = new Next[A] {
    private val arr = as.toArray
    private val len = arr.length
    private var i = 0
    def apply(gen: Generator): A = {
      val a = arr(i)
      i = (i + 1) % len
      a
    }
  }
}
