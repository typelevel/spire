package spire.random

import spire.algebra._
import spire.implicits._
import spire.math._

import scala.collection.mutable.ArrayBuffer
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

trait Dist[@spec A] { self =>
  def apply(g: Generator): A

  final def get()(implicit g: Generator): A =
    apply(g)

  def fill(g: Generator, arr: Array[A]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = apply(g)
      i += 1
    }
  }

  final def map[B](f: A => B): Dist[B] =
    new DistFromGen(g => f(apply(g)))

  final def flatMap[B](f: A => Dist[B]): Dist[B] =
    new DistFromGen(g => f(apply(g))(g))

  final def filter(pred: A => Boolean): Dist[A] =
    new Dist[A] {
      @tailrec final def apply(g: Generator): A = {
        val a = self(g)
        if (pred(a)) a else apply(g)
      }
    }

  final def given(pred: A => Boolean): Dist[A] =
    filter(pred)

  def until(pred: A => Boolean): Dist[Seq[A]] = {
    @tailrec def loop(gen: Generator, a: A, buf: ArrayBuffer[A]): Seq[A] = {
      buf.append(a)
      if (pred(a)) buf else loop(gen, self(gen), buf)
    }
    new DistFromGen(g => loop(g, self(g), ArrayBuffer.empty[A]))
  }

  def foldn[B](init: B, n: Int)(f: (B, A) => B): Dist[B] = {
    @tailrec def loop(gen: Generator, i: Int, b: B): B =
      if (i == 0) b else loop(gen, i - 1, f(b, self(gen)))
    new DistFromGen(g => loop(g, n, init))
  }

  def unfold[B](init: B)(f: (B, A) => B)(pred: B => Boolean): Dist[B] = {
    @tailrec def loop(gen: Generator, b: B): B =
      if (pred(b)) b else loop(gen, f(b, self(gen)))
    new DistFromGen(g => loop(g, init))
  }

  def pack(n: Int)(implicit ct: ClassTag[A]): Dist[Array[A]] = new Dist[Array[A]] {
    def apply(gen: Generator): Array[A] = {
      var i = 0
      val arr = new Array[A](n)
      while (i < arr.length) {
        arr(i) = self(gen)
        i += 1
      }
      arr
    }
  }

  def repeat[CC[A] <: Seq[A]](n: Int)(implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): Dist[CC[A]] =
    new Dist[CC[A]] {
      def apply(gen: Generator): CC[A] = {
        val builder = cbf()
        builder.sizeHint(n)
        var i = 0
        while (i < n) {
          builder += self(gen)
          i += 1
        }
        builder.result()
      }
    }

  def iterate(n: Int, f: A => Dist[A]): Dist[A] =
    if (n == 0) this else flatMap(f).iterate(n - 1 ,f)

  def iterateUntil(pred: A => Boolean, f: A => Dist[A]): Dist[A] = new Dist[A] {
    @tailrec def loop(gen: Generator, a: A): A =
      if (pred(a)) a else loop(gen, f(a)(gen))

    def apply(gen: Generator): A = loop(gen, self(gen))
  }

  final def zip[B](that: Dist[B]): Dist[(A, B)] =
    new DistFromGen(g => (this(g), that(g)))

  def zipWith[B, C](that: Dist[B])(f: (A, B) => C): Dist[C] =
    new DistFromGen(g => f(this(g), that(g)))

  final def toIterator(gen: Generator): Iterator[A] =
    new DistIterator(this, gen)

  final def toStream(gen: Generator): Stream[A] =
    this(gen) #:: toStream(gen)

  import scala.collection.generic.CanBuildFrom

  def sample[CC[A] <: Iterable[A]](n: Int)(implicit gen: Generator, cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val b = cbf()
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += self(gen)
      i += 1
    }
    b.result
  }

  final def count(pred: A => Boolean, n: Int)(implicit gen: Generator): Int = {
    @tailrec def loop(num: Int, i: Int): Int =
      if (i == 0) num else loop(num + (if (pred(self(gen))) 1 else 0), i - 1)
    loop(0, n)
  }

  def pr(pred: A => Boolean, n: Int)(implicit gen: Generator): Double =
    1.0 * count(pred, n) / n

  def sum(n: Int)(implicit gen: Generator, alg: Rig[A]): A = {
    @tailrec def loop(total: A, i: Int): A =
      if (i == 0) total else loop(alg.plus(total, self(gen)), i - 1)
    loop(alg.zero, n)
  }

  def ev(n: Int)(implicit gen: Generator, alg: Field[A]): A =
    alg.div(sum(n), alg.fromInt(n))

  def histogram(n: Int)(implicit gen: Generator): Map[A, Double] =
    rawHistogram(n).map { case (k, v) => (k, 1.0 * v / n) }

  def rawHistogram(n: Int)(implicit gen: Generator): Map[A, Int] =
    toStream(gen).take(n).foldLeft(Map.empty[A, Int]) { case (h, a) =>
      h.updated(a, h.getOrElse(a, 0) + 1)
    }
}

final class DistIterator[A](next: Dist[A], gen: Generator) extends Iterator[A] {
  final def hasNext(): Boolean = true
  final def next(): A = next(gen)
}

class DistFromGen[@spec A](f: Generator => A) extends Dist[A] {
  def apply(gen: Generator): A = f(gen)
}

trait DistRing[A] extends Ring[Dist[A]] {
  def alg: Ring[A]
  def zero: Dist[A] = Dist.constant(alg.zero)
  def one: Dist[A] = Dist.constant(alg.one)
  def plus(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => alg.plus(x(g), y(g)))
  def negate(x: Dist[A]): Dist[A] = new DistFromGen(g => alg.negate(x(g)))
  def times(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => alg.times(x(g), y(g)))
}

trait DistEuclideanRing[A] extends EuclideanRing[Dist[A]] with DistRing[A] {
  def alg: EuclideanRing[A]
  def quot(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => alg.quot(x(g), y(g)))
  def mod(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => alg.mod(x(g), y(g)))
  def gcd(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => alg.gcd(x(g), y(g)))
}

trait DistField[A] extends Field[Dist[A]] with DistEuclideanRing[A] {
  def alg: Field[A]
  def div(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => alg.div(x(g), y(g)))
  override def reciprocal(x: Dist[A]): Dist[A] = new DistFromGen(g => alg.reciprocal(x(g)))
}

class Size(val int: Int) extends AnyVal

object Size {
  def apply(n: Int) = new Size(n)
  implicit def sizeToInt(s: Size): Int = s.int
}

object Dist {
  @inline final def apply[A](implicit na: Dist[A]): Dist[A] = na

  import spire.algebra.Ring

  implicit def ring[A](implicit ev: Ring[A]): Ring[Dist[A]] =
    new DistRing[A] { def alg = ev }

  implicit def euclideanRing[A](implicit ev: EuclideanRing[A]): EuclideanRing[Dist[A]] =
    new DistEuclideanRing[A] { def alg = ev }

  implicit def field[A](implicit ev: Field[A]): Field[Dist[A]] =
    new DistField[A] { def alg = ev }

  final def apply[A, B](f: A => B)(implicit na: Dist[A]): Dist[B] =
    na.map(f)

  final def apply[A, B, C](f: (A, B) => C)(implicit na: Dist[A], nb: Dist[B]): Dist[C] =
    new DistFromGen(g => f(na(g), nb(g)))

  final def gen[A](f: Generator => A): Dist[A] =
    new DistFromGen(g => f(g))

  def reduce[A](ns: Dist[A]*)(f: (A, A) => A): Dist[A] =
    new DistFromGen(g => ns.map(_(g)).reduceLeft(f))

  def fromBytes[A](n: Int)(f: Array[Byte] => A): Dist[A] =
    new DistFromGen(g => f(g.generateBytes(n)))

  def fromInts[A](n: Int)(f: Array[Int] => A): Dist[A] =
    new DistFromGen(g => f(g.generateInts(n)))

  def fromLongs[A](n: Int)(f: Array[Long] => A): Dist[A] =
    new DistFromGen(g => f(g.generateLongs(n)))

  def mix[A](ds: Dist[A]*): Dist[A] =
    Dist.oneOf(ds:_*).flatMap(identity)

  def weightedMix[A](tpls: (Double, Dist[A])*): Dist[A] = {
    val ds = new Array[Dist[A]](tpls.length)
    val ws = new Array[Double](tpls.length)
    var i = 0
    var total = 0.0
    tpls.foreach { case (w, d) =>
      total += w
      ws(i) = total
      ds(i) = d
      i += 1
    }

    new DistFromGen({ g =>
      val w = g.nextDouble(total)
      var i = 0
      while (ws(i) < w) i += 1
      ds(i).apply(g)
    })
  }

  implicit val unit: Dist[Unit] = Dist.always(())
  implicit val boolean: Dist[Boolean] = new DistFromGen[Boolean](_.nextBoolean)
  implicit val byte: Dist[Byte] = new DistFromGen[Byte](_.nextInt.toByte)
  implicit val short: Dist[Short] = new DistFromGen[Short](_.nextInt.toShort)
  implicit val char: Dist[Char] = new DistFromGen[Char](_.nextInt.toChar)
  implicit val int: Dist[Int] = new DistFromGen[Int](_.nextInt)
  implicit val float: Dist[Float] = new DistFromGen[Float](_.nextFloat)
  implicit val long: Dist[Long] = new DistFromGen[Long](_.nextLong)
  implicit val double: Dist[Double] = new DistFromGen[Double](_.nextDouble)

  implicit val ubyte: Dist[UByte] = new DistFromGen[UByte](g => UByte(g.nextInt))
  implicit val ushort: Dist[UShort] = new DistFromGen[UShort](g => UShort(g.nextInt))
  implicit val uint: Dist[UInt] = new DistFromGen[UInt](g => UInt(g.nextInt))
  implicit val ulong: Dist[ULong] = new DistFromGen[ULong](g => ULong(g.nextLong))

  implicit def complex[A: Fractional: Trig: IsReal: Dist]: Dist[Complex[A]] =
    Dist(Complex(_: A, _: A))

  implicit def interval[A: Order: Dist]: Dist[Interval[A]] =
    Dist((x: A, y: A) => if (x < y) Interval(x, y) else Interval(y, x))

  implicit def option[A](implicit bool: Dist[Boolean], a: Dist[A]): Dist[Option[A]] =
    new DistFromGen(g => if (bool(g)) Some(a(g)) else None)

  implicit def either[A, B](implicit bool: Dist[Boolean], a: Dist[A], b: Dist[B]): Dist[Either[A, B]] =
    new DistFromGen(g => if (bool(g)) Right(b(g)) else Left(a(g)))

  implicit def tuple2[A: Dist, B: Dist]: Dist[(A, B)] =
    Dist((_: A, _: B))

  implicit def rational(implicit bigint: Dist[BigInt]): Dist[Rational] =
    Dist(Rational(_: BigInt, _: BigInt))(bigint, bigint.filter(_ != 0))

  def constant[A](a: A): Dist[A] = new DistFromGen[A](g => a)

  def always[A](a: A): Dist[A] = new DistFromGen[A](g => a)

  def oneOf[A: ClassTag](as: A*): Dist[A] = new Dist[A] {
    private val arr = as.toArray
    def apply(gen: Generator): A = arr(gen.nextInt(arr.length))
  }

  def cycleOf[A: ClassTag](as: A*): Dist[A] = new Dist[A] {
    private val arr = as.toArray
    private var i = 0
    def apply(gen: Generator): A = {
      val a = arr(i)
      i = (i + 1) % arr.length
      a
    }
  }

  import java.lang.Long.numberOfLeadingZeros

  def binsize(g: Generator): Int = {
    val a = numberOfLeadingZeros(g.nextLong)
    val b = numberOfLeadingZeros(g.nextLong)
    val c = numberOfLeadingZeros(g.nextLong)
    a * b + c
  }

  implicit def size: Dist[Size] =
    new DistFromGen(g => new Size(binsize(g)))

  implicit def defaultNatural(implicit size: Dist[Size]): Dist[Natural] =
    new DistFromGen[Natural]({ g =>
      val init = Natural(g.next[UInt])
      (0 until size(g)).foldLeft(init)((n, _) => Natural.Digit(g.next[UInt], n))
    })

  implicit def defaultBigInt(implicit size: Dist[Size]): Dist[BigInt] =
    new DistFromGen[BigInt](g => BigInt(g.generateBytes(4 * size(g) + 4)))

  implicit def defaultSafeLong(implicit bigint: Dist[BigInt]): Dist[SafeLong] =
    Dist(SafeLong(_: BigInt))

  implicit def defaultBigDecimal(implicit rat: Dist[Rational]): Dist[BigDecimal] =
    Dist((_: Rational).toBigDecimal)

  implicit def array[A](implicit a: Dist[A], size: Dist[Size], ct: ClassTag[A]): Dist[Array[A]] =
    new DistFromGen(g => g.generateArray[A](size(g)))

  implicit def traversableOnce[A, CC[A] <: TraversableOnce[A]]
    (implicit a: Dist[A], size: Dist[Size], cbf: CanBuildFrom[Nothing, A, CC[A]]): Dist[CC[A]] =
    new DistFromGen(g => buildHelper(g, cbf, a, size))

  implicit def map[A, B, MM[A, B] <: scala.collection.Map[A, B]]
    (implicit ab: Dist[(A, B)], size: Dist[Size], cbf: CanBuildFrom[Nothing, (A, B), MM[A, B]]): Dist[MM[A, B]] =
    new DistFromGen(g => buildHelper(g, cbf, ab, size))

  def buildHelper[A, R](g: Generator, cbf: CanBuildFrom[Nothing, A, R], a: Dist[A], size: Dist[Size]): R = {
    val builder = cbf()
    val n = size(g)
    builder.sizeHint(n)
    var i = 0
    while (i < n) {
      builder += a(g)
      i += 1
    }
    builder.result()
  }
}
