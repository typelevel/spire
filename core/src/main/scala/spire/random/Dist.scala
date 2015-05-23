package spire.random

import spire.algebra._
import spire.syntax.all._
import spire.math.{Complex, Interval, Natural, Rational, SafeLong, UByte, UShort, UInt, ULong}

import scala.collection.mutable.ArrayBuffer
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

trait Dist[@spec A] extends Any { self =>

  def apply(gen: Generator): A

  def fill(gen: Generator, arr: Array[A]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = apply(gen)
      i += 1
    }
  }

  final def map[B](f: A => B): Dist[B] =
    new DistFromGen(g => f(apply(g)))

  final def flatMap[B](f: A => Dist[B]): Dist[B] =
    new DistFromGen(g => f(apply(g))(g))

  final def filter(pred: A => Boolean): Dist[A] =
    new Dist[A] {
      @tailrec final def apply(gen: Generator): A = {
        val a = self(gen)
        if (pred(a)) a else apply(gen)
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

  def repeat[CC[X] <: Seq[X]](n: Int)(implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): Dist[CC[A]] =
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

  def sample[CC[X] <: Iterable[X]](n: Int)(implicit gen: Generator, cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
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
  final def hasNext: Boolean = true
  final def next(): A = next(gen)
}

class DistFromGen[@spec A](f: Generator => A) extends Dist[A] {
  def apply(gen: Generator): A = f(gen)
}

trait DistSemiring[A] extends Semiring[Dist[A]] {
  implicit def alg: Semiring[A]
  def zero: Dist[A] = Dist.constant(alg.zero)
  def plus(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => x(g) + y(g))
  def times(x: Dist[A], y: Dist[A]): Dist[A] = new DistFromGen(g => x(g) * y(g))
}

trait DistRng[A] extends DistSemiring[A] with Rng[Dist[A]] {
  implicit def alg: Rng[A]
  def negate(x: Dist[A]): Dist[A] = new DistFromGen(g => alg.negate(x(g)))
}

trait DistRig[A] extends DistSemiring[A] with Rig[Dist[A]] {
  implicit def alg: Rig[A]
  def one: Dist[A] = Dist.constant(alg.one)
}

trait DistRing[A] extends DistRng[A] with Ring[Dist[A]] {
  def alg: Ring[A]
  def one: Dist[A] = Dist.constant(alg.one)
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

trait DistVectorSpace[V, K] extends VectorSpace[Dist[V], Dist[K]] {
  implicit def alg: VectorSpace[V, K]

  def zero: Dist[V] = Dist.constant(alg.zero)
  def plus(x: Dist[V], y: Dist[V]): Dist[V] = new DistFromGen(g => x(g) + y(g))
  def negate(x: Dist[V]): Dist[V] = new DistFromGen(g => -x(g))
  override def minus(x: Dist[V], y: Dist[V]): Dist[V] = new DistFromGen(g => x(g) - y(g))
  def timesl(k: Dist[K], v: Dist[V]): Dist[V] = new DistFromGen(g => k(g) *: v(g))
  def timesr(k: Dist[K], v: Dist[V]): Dist[V] = new DistFromGen(g => v(g) :* k(g))
}

trait DistNormedVectorSpace[V, K] extends DistVectorSpace[V, K]
with NormedVectorSpace[Dist[V], Dist[K]] {
  implicit def alg: NormedVectorSpace[V, K]

  def norm(v: Dist[V]): Dist[K] = v map alg.norm
}

trait DistInnerProductSpace[V, K] extends DistVectorSpace[V, K]
with InnerProductSpace[Dist[V], Dist[K]] {
  implicit def alg: InnerProductSpace[V, K]

  def dot(v: Dist[V], w: Dist[V]): Dist[K] = new DistFromGen(g => v(g) dot w(g))
}

object Dist extends DistInstances8 {
  @inline final def apply[A](implicit na: Dist[A]): Dist[A] = na

  final def apply[A, B](f: A => B)(implicit na: Dist[A]): Dist[B] =
    na.map(f)

  final def apply[A, B, C](f: (A, B) => C)(implicit na: Dist[A], nb: Dist[B]): Dist[C] =
    na.zipWith(nb)(f)

  final def gen[A](f: Generator => A): Dist[A] =
    new DistFromGen(g => f(g))

  def uniform[A: Uniform](low: A, high: A): Dist[A] = Uniform[A].apply(low, high)

  def gaussian[A: Gaussian](mean: A, stdDev: A): Dist[A] = Gaussian[A].apply(mean, stdDev)

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

  implicit val unit: Dist[Unit] = new DistFromGen[Unit](g => ())
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

  implicit def interval[A](implicit na: Dist[A], order: Order[A], r: AdditiveMonoid[A]): Dist[Interval[A]] =
    Dist((x: A, y: A) => if (order.lt(x, y)) Interval(x, y) else Interval(y, x))

  implicit def option[A](implicit no: Dist[Boolean], na: Dist[A]): Dist[Option[A]] =
    new DistFromGen(g => if (no(g)) Some(na(g)) else None)

  implicit def either[A, B](implicit no: Dist[Boolean], na: Dist[A], nb: Dist[B]): Dist[Either[A, B]] =
    new DistFromGen[Either[A, B]](g => if (no(g)) Right(nb(g)) else Left(na(g)))

  implicit def tuple2[A: Dist, B: Dist]: Dist[(A, B)] =
    Dist((_: A, _: B))

  def intrange(from: Int, to: Int): Dist[Int] = {
    val d = to - from + 1
    new DistFromGen(_.nextInt(d) + from)
  }

  def natural(maxDigits: Int): Dist[Natural] = new Dist[Natural] {
    @tailrec
    private def loop(g: Generator, i: Int, size: Int, n: Natural): Natural =
      if (i < size) loop(g, i + 1, size, Natural.Digit(g.next[UInt], n)) else n

    def apply(gen: Generator): Natural =
      loop(gen, 1, gen.nextInt(maxDigits) + 1, Natural.End(gen.next[UInt]))
  }

  def safelong(maxBytes: Int): Dist[SafeLong] = if (maxBytes <= 0) {
    throw new IllegalArgumentException("need positive maxBytes, got %s" format maxBytes)
  } else if (maxBytes < 8) {
    val n = (8 - maxBytes) * 8
    new DistFromGen(g => SafeLong(g.nextLong >>> n))
  } else if (maxBytes == 8) {
    new DistFromGen(g => SafeLong(g.nextLong))
  } else  {
    bigint(maxBytes).map(SafeLong(_))
  }

  def bigint(maxBytes: Int): Dist[BigInt] = new Dist[BigInt] {
    def apply(gen: Generator): BigInt = BigInt(gen.generateBytes(gen.nextInt(maxBytes) + 1))
  }

  def bigdecimal(maxBytes: Int, maxScale: Int): Dist[BigDecimal] = new Dist[BigDecimal] {
    private val nb = bigint(maxBytes)
    def apply(gen: Generator): BigDecimal = BigDecimal(nb(gen), gen.nextInt(maxScale) + 1)
  }

  implicit def rational(implicit next: Dist[BigInt]): Dist[Rational] =
    Dist(Rational(_: BigInt, _: BigInt))(next, next.filter(_ != 0))

  def longrational: Dist[Rational] =
    Dist(Rational(_: Long, _: Long))(Dist[Long], Dist[Long].filter(_ != 0))

  def bigrational(maxBytes: Int): Dist[Rational] =
    rational(bigint(maxBytes))

  def constant[A](a: A): Dist[A] = new DistFromGen[A](g => a)
  def always[A](a: A): Dist[A] = new DistFromGen[A](g => a)

  implicit def array[A: Dist: ClassTag](minSize: Int, maxSize: Int): Dist[Array[A]] =
    new Dist[Array[A]] {
      private val d = maxSize - minSize + 1
      def apply(gen: Generator): Array[A] = gen.generateArray[A](gen.nextInt(d) + minSize)
    }

  implicit def list[A: Dist](minSize: Int, maxSize: Int): Dist[List[A]] =
    new Dist[List[A]] {
      private val d = maxSize - minSize + 1
      private def loop(g: Generator, n: Int, sofar: List[A]): List[A] =
        if (n > 0) loop(g, n - 1, g.next[A] :: sofar) else sofar

      def apply(gen: Generator): List[A] =
        loop(gen, gen.nextInt(d) + minSize, Nil)
    }

  implicit def set[A: Dist](minInputs: Int, maxInputs: Int): Dist[Set[A]] =
    list[A](minInputs, maxInputs).map(_.toSet)

  implicit def map[A: Dist, B: Dist](minInputs: Int, maxInputs: Int): Dist[Map[A, B]] =
    list[(A, B)](minInputs, maxInputs).map(_.toMap)

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

  def gaussianFromDouble[A: Field] = new DistFromGen[A](g => Field[A].fromDouble(g.nextGaussian))
}

trait DistInstances0 {
  implicit def semiring[A](implicit ev: Semiring[A]): Semiring[Dist[A]] =
    new DistSemiring[A] { def alg = ev }
}

trait DistInstances1 extends DistInstances0 {
  implicit def rig[A](implicit ev: Rig[A]): Rig[Dist[A]] =
    new DistRig[A] { def alg = ev }

  implicit def rng[A](implicit ev: Rng[A]): Rng[Dist[A]] =
    new DistRng[A] { def alg = ev }
}

trait DistInstances2 extends DistInstances1 {
  implicit def ring[A](implicit ev: Ring[A]): Ring[Dist[A]] =
    new DistRing[A] { def alg = ev }
}

trait DistInstances3 extends DistInstances2 {
  implicit def euclideanRing[A](implicit ev: EuclideanRing[A]): EuclideanRing[Dist[A]] =
    new DistEuclideanRing[A] { def alg = ev }
}

trait DistInstances4 extends DistInstances3 {
  implicit def field[A](implicit ev: Field[A]): Field[Dist[A]] =
    new DistField[A] { def alg = ev }
}

trait DistInstances5 extends DistInstances4

trait DistInstances6 extends DistInstances5 {
  implicit def vectorSpace[V,K](implicit ev: VectorSpace[V,K]): VectorSpace[Dist[V],Dist[K]] =
    new DistVectorSpace[V,K] { def alg = ev }
}

trait DistInstances7 extends DistInstances6 {
  implicit def NormedVectorSpace[V,K](implicit ev: NormedVectorSpace[V,K]): NormedVectorSpace[Dist[V],Dist[K]] =
    new DistNormedVectorSpace[V,K] { def alg = ev }
}

trait DistInstances8 extends DistInstances7 {
  implicit def InnerProductSpace[V,K](implicit ev: InnerProductSpace[V,K]): InnerProductSpace[Dist[V],Dist[K]] =
    new DistInnerProductSpace[V,K] { def alg = ev }
}
