package spire

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.{specialized => spec}

import spire.algebra._
import spire.math._
import spire.macrosk._

final class LiteralIntOps(val lhs:Int) extends AnyVal {
  @inline private final def q = Rational(lhs, 1)

  def +[A](rhs:A)(implicit ev:Ring[A]) = ev.plus(ev.fromInt(lhs), rhs)
  def -[A](rhs:A)(implicit ev:Ring[A]) = ev.minus(ev.fromInt(lhs), rhs)
  def *[A](rhs:A)(implicit ev:Ring[A]) = ev.times(ev.fromInt(lhs), rhs)
  def /[A](rhs:A)(implicit ev:Field[A]) = ev.div(ev.fromInt(lhs), rhs)

  def /~[A](rhs:A)(implicit ev:EuclideanRing[A]) = ev.quot(ev.fromInt(lhs), rhs)
  def %[A](rhs:A)(implicit ev:EuclideanRing[A]) = ev.mod(ev.fromInt(lhs), rhs)
  def /%[A](rhs:A)(implicit ev:EuclideanRing[A]) = ev.quotmod(ev.fromInt(lhs), rhs)

  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lt(c.fromInt(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lteqv(c.fromInt(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gt(c.fromInt(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gteqv(c.fromInt(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.compare(c.fromInt(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.min(c.fromInt(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.max(c.fromInt(lhs), rhs)

  def +(rhs:Rational) = q + rhs
  def -(rhs:Rational) = q - rhs
  def *(rhs:Rational) = q * rhs
  def /(rhs:Rational) = q / rhs

  def /~(rhs:Rational) = q.quot(rhs)
  def %(rhs:Rational) = q % rhs
  def /%(rhs:Rational) = (q.quot(rhs), q % rhs)

  def **(rhs:Rational)(implicit ev:ApproximationContext[Rational]) = q.pow(rhs)

  def <(rhs:Rational) = q.compare(rhs) < 0
  def <=(rhs:Rational) = q.compare(rhs) <= 0
  def >(rhs:Rational) = q.compare(rhs) > 0
  def >=(rhs:Rational) = q.compare(rhs) >= 0

  @inline private def c[A](implicit f:Fractional[A], t:Trig[A]) =
    Complex(f.fromInt(lhs), f.zero)

  def +[A:Fractional:Trig](rhs:Complex[A]) = c[A] + rhs
  def -[A:Fractional:Trig](rhs:Complex[A]) = c[A] - rhs
  def *[A:Fractional:Trig](rhs:Complex[A]) = c[A] * rhs
  def /[A:Fractional:Trig](rhs:Complex[A]) = c[A] / rhs

  def /~[A:Fractional:Trig](rhs:Complex[A]) = c[A] /~ rhs
  def %[A:Fractional:Trig](rhs:Complex[A]) = c[A] % rhs
  def /%[A:Fractional:Trig](rhs:Complex[A]) = c[A] /% rhs

  def **[A:Fractional:Trig](rhs:Complex[A]) = c[A] ** rhs

  def +(rhs:Real) = Real(lhs) + rhs

  def /~(rhs:Int) = EuclideanRing[Int].quot(lhs, rhs)
  def /%(rhs:Int) = EuclideanRing[Int].quotmod(lhs, rhs)
  def pow(rhs:Int) = Ring[Int].pow(lhs, rhs)
  def **(rhs:Int) = Ring[Int].pow(lhs, rhs)
}

final class LiteralDoubleOps(val lhs:Double) extends AnyVal {
  def pow(rhs:Int) = Ring[Double].pow(lhs, rhs)
  def **(rhs:Int) = Ring[Double].pow(lhs, rhs)

  @inline private final def c[A:Fractional:ConvertableTo:Trig]:Complex[A] =
    Complex(ConvertableFrom[Double].toType[A](lhs), Fractional[A].zero)

  def +[A:Fractional:Trig](rhs:Complex[A]) = c + rhs
  def *[A:Fractional:Trig](rhs:Complex[A]) = c * rhs
  def -[A:Fractional:Trig](rhs:Complex[A]) = c - rhs
  def /[A:Fractional:Trig](rhs:Complex[A]) = c / rhs
  def /~[A:Fractional:Trig](rhs:Complex[A]) = c /~ rhs
  def %[A:Fractional:Trig](rhs:Complex[A]) = c % rhs
  def /%[A:Fractional:Trig](rhs:Complex[A]) = c /% rhs
  def pow[A:Fractional:Trig](rhs:Complex[A]) = c pow rhs
  def **[A:Fractional:Trig](rhs:Complex[A]) = c ** rhs
}

final class ArrayOps[@spec A](arr:Array[A]) {
  def qsum(implicit ev:Ring[A]) = {
    @tailrec
    def f(i:Int, n:Int, t:A):A = if (i < n) f(i + 1, n, ev.plus(t, arr(i))) else t
    f(0, arr.length, ev.zero)
  }

  def qproduct(implicit ev:Ring[A]) = {
    @tailrec
    def f(i:Int, n:Int, t:A):A = if (i < n) f(i + 1, n, ev.times(t, arr(i))) else t
    f(0, arr.length, ev.one)
  }

  def qnorm(p:Int)(implicit ev:Field[A], s: Signed[A], nr:NRoot[A]) = {
    @tailrec
    def f(i:Int, n:Int, t:A):A =
      if (i < n) f(i + 1, n, ev.plus(t, ev.pow(s.abs(arr(i)), p))) else t
    nr.nroot(f(0, arr.length, ev.one), p)
  }

  def qmin(implicit ev:Order[A]) = {
    if (arr.length == 0) sys.error("empty array")
    @tailrec
    def f(i:Int, n:Int, t:A):A = if (i < n) f(i + 1, n, ev.min(t, arr(i))) else t
    f(1, arr.length, arr(0))
  }

  def qmax(implicit ev:Order[A]) = {
    if (arr.length == 0) sys.error("empty array")
    @tailrec
    def f(i:Int, n:Int, t:A):A = if (i < n) f(i + 1, n, ev.max(t, arr(i))) else t
    f(1, arr.length, arr(0))
  }

  def qmean(implicit ev:Field[A]): A = {
    if (arr.length == 0) sys.error("empty array")
    var sum = ev.zero
    @tailrec
    def f(i:Int, j: Int, n:Int):A = if (i < n) {
      val t = ev.div(ev.times(sum, ev.fromInt(i)), ev.fromInt(j))
      val z = ev.div(arr(i), ev.fromInt(j))
      sum = ev.plus(t, z)
      f(i + 1, j + 1, n)
    } else {
      sum
    }
    f(0, 1, arr.length)
  }
      
  import spire.math.{Sorting, Selection}

  def qsort(implicit ev:Order[A], ct:ClassTag[A]) {
    Sorting.sort(arr)
  }

  def qsortBy[@spec B](f:A => B)(implicit ev:Order[B], ct:ClassTag[A]) {
    implicit val ord: Order[A] = ev.on(f)
    Sorting.sort(arr)
  }

  def qsortWith(f:(A, A) => Int)(implicit ct:ClassTag[A]) {
    implicit val ord: Order[A] = Order.from(f)
    Sorting.sort(arr)
  }

  def qsorted(implicit ev:Order[A], ct:ClassTag[A]): Array[A] = {
    val arr2 = arr.clone
    Sorting.sort(arr2)
    arr2
  }

  def qsortedBy[@spec B](f:A => B)(implicit ev:Order[B], ct:ClassTag[A]): Array[A] = {
    implicit val ord: Order[A] = ev.on(f)
    val arr2 = arr.clone
    Sorting.sort(arr2)
    arr2
  }

  def qsortedWith(f:(A, A) => Int)(implicit ct:ClassTag[A]): Array[A] = {
    implicit val ord: Order[A] = Order.from(f)
    val arr2 = arr.clone
    Sorting.sort(arr2)
    arr2
  }

  def qselect(k: Int)(implicit ev:Order[A], ct:ClassTag[A]) {
    Selection.select(arr, k)
  }

  def qselected(k: Int)(implicit ev:Order[A], ct:ClassTag[A]): Array[A] = {
    val arr2 = arr.clone
    Selection.select(arr2, k)
    arr2
  }
}

import scala.collection.generic.CanBuildFrom

final class SeqOps[@spec A](as:Seq[A])(implicit cbf:CanBuildFrom[Seq[A], A, Seq[A]]) {
  def qsum(implicit ev:Ring[A]) = {
    var sum = ev.zero
    as.foreach(a => sum = ev.plus(sum, a))
    sum
  }

  def qproduct(implicit ev:Ring[A]) = {
    var prod = ev.one
    as.foreach(a => prod = ev.times(prod, a))
    prod
  }

  def qnorm(p:Int)(implicit ev:Field[A], s: Signed[A], nr:NRoot[A]) = {
    var t = ev.one
    as.foreach(a => t = ev.plus(t, ev.pow(s.abs(a), p)))
    nr.nroot(t, p)
  }

  def qmin(implicit ev:Order[A]) = {
    if (as.isEmpty) sys.error("empty seq")
    var min = as.head
    as.foreach(a => min = ev.min(min, a))
    min
  }

  def qmax(implicit ev:Order[A]) = {
    if (as.isEmpty) sys.error("empty seq")
    var max = as.head
    as.foreach(a => max = ev.max(max, a))
    max
  }

  def qmean(implicit ev:Field[A]): A = {
    if (as.isEmpty) sys.error("empty seq")
    var mean = ev.zero
    var i = 0
    as.foreach { a =>
      val t = ev.div(ev.times(mean, ev.fromInt(i)), ev.fromInt(i + 1))
      val z = ev.div(a, ev.fromInt(i + 1))
      mean = ev.plus(t, z)
    }
    mean
  }

  import spire.math.{Sorting, Selection}

  protected[this] def toSizeAndArray(implicit ct:ClassTag[A]) = {
    val len = as.size
    val arr = new Array[A](len)
    var i = 0
    as.foreach { a =>
      arr(i) = a
      i += 1
    }
    (len, arr)
  }

  protected[this] def fromSizeAndArray(len: Int, arr: Array[A]) = {
    val b = cbf(as)
    b.sizeHint(len)
    var i = 0
    while (i < len) {
      b += arr(i)
      i += 1
    }
    b.result
  }

  def qsorted(implicit ev:Order[A], ct:ClassTag[A]): Seq[A] = {
    val (len, arr) = toSizeAndArray
    Sorting.sort(arr)
    fromSizeAndArray(len, arr)
  }
  
  def qsortedBy[@spec B](f:A => B)(implicit ev:Order[B], ct:ClassTag[A]): Seq[A] = {
    val (len, arr) = toSizeAndArray
    implicit val ord: Order[A] = ev.on(f)
    Sorting.sort(arr)
    fromSizeAndArray(len, arr)
  }
  
  def qsortedWith(f:(A, A) => Int)(implicit ct:ClassTag[A]): Seq[A] = {
    val (len, arr) = toSizeAndArray
    implicit val ord: Order[A] = Order.from(f)
    Sorting.sort(arr)
    fromSizeAndArray(len, arr)
  }
  
  def qselected(k: Int)(implicit ev:Order[A], ct:ClassTag[A]): Seq[A] = {
    val (len, arr) = toSizeAndArray
    Selection.select(arr, k)
    fromSizeAndArray(len, arr)
  }
}


final class ConversionOps[A](a: A) {
  def narrow[B](implicit rhs: NarrowingConversion[A, B]): B =
    macro Ops.flip[NarrowingConversion[A, B], B]

  def widen[B](implicit rhs: WideningConversion[A, B]): B =
    macro Ops.flip[WideningConversion[A, B], B]
}

object implicits {
  implicit def eqOps[A:Eq](a:A) = new EqOps(a)
  implicit def orderOps[A:Order](a:A) = new OrderOps(a)
  implicit def semigroupOps[A:Semigroup](a:A) = new SemigroupOps(a)
  implicit def groupOps[A:Group](a:A) = new GroupOps(a)

  implicit def convertableOps[A:ConvertableFrom](a:A) = new ConvertableFromOps(a)

  implicit def ringOps[A:Ring](a:A) = new RingOps(a)
  implicit def euclideanRingOps[A:EuclideanRing](a:A) = new EuclideanRingOps(a)
  implicit def fieldOps[A:Field](a:A) = new FieldOps(a)

  implicit def integralOps[A:Integral](a:A) = new IntegralOps(a)
  implicit def fractionalOps[A:Fractional](a:A) = new FractionalOps(a)

  implicit def signedOps[A: Signed](a: A) = new SignedOps(a)
  implicit def nrootOps[A: NRoot](a: A) = new NRootOps(a)

  implicit def literalIntOps(lhs:Int) = new LiteralIntOps(lhs)
  implicit def literalDoubleOps(lhs:Double) = new LiteralDoubleOps(lhs)

  implicit def arrayOps[@spec A](lhs:Array[A]) = new ArrayOps(lhs)
  implicit def seqOps[@spec A](lhs:Seq[A]) = new SeqOps[A](lhs)

  implicit def intToA[A](n:Int)(implicit c:ConvertableTo[A]): A = c.fromInt(n)

  implicit def conversionOps[A](a: A) = new ConversionOps(a)
}

object syntax {
  import spire.macrosk._
  def cfor[A](init:A)(test:A => Boolean, next:A => A)(body:A => Unit): Unit =
    macro Syntax.cforMacro[A]

  implicit def literals(s:StringContext) = new Literals(s)

  object radix { implicit def radix(s:StringContext) = new Radix(s) }
  object si { implicit def siLiterals(s:StringContext) = new SiLiterals(s) }
  object us { implicit def usLiterals(s:StringContext) = new UsLiterals(s) }
  object eu { implicit def euLiterals(s:StringContext) = new EuLiterals(s) }
}
