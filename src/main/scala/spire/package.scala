package spire

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.{specialized => spec}

import spire.algebra._
import spire.math._
import spire.macrosk._

import java.lang.Long.numberOfTrailingZeros
import java.lang.Math
import java.math.BigInteger

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
  def pow(rhs:Double) = spire.math.pow(lhs, rhs)
  def **(rhs:Double) = spire.math.pow(lhs, rhs)

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

  def +[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]) = ev.plus(c.fromDouble(lhs), rhs)
  def -[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]) = ev.minus(c.fromDouble(lhs), rhs)
  def *[A](rhs:A)(implicit ev:Ring[A], c:ConvertableTo[A]) = ev.times(c.fromDouble(lhs), rhs)
  def /[A](rhs:A)(implicit ev:Field[A], c:ConvertableTo[A]) = ev.div(c.fromDouble(lhs), rhs)
  def /~[A](rhs:A)(implicit ev:EuclideanRing[A], c:ConvertableTo[A]) = ev.quot(c.fromDouble(lhs), rhs)
  def %[A](rhs:A)(implicit ev:EuclideanRing[A], c:ConvertableTo[A]) = ev.mod(c.fromDouble(lhs), rhs)
  def /%[A](rhs:A)(implicit ev:EuclideanRing[A], c:ConvertableTo[A]) = ev.quotmod(c.fromDouble(lhs), rhs)
  def **[A](rhs:A)(implicit ev:NRoot[A], c:ConvertableTo[A]) = ev.fpow(c.fromDouble(lhs), rhs)

  def <[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lt(c.fromDouble(lhs), rhs)
  def <=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.lteqv(c.fromDouble(lhs), rhs)
  def >[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gt(c.fromDouble(lhs), rhs)
  def >=[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.gteqv(c.fromDouble(lhs), rhs)

  def cmp[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.compare(c.fromDouble(lhs), rhs)
  def min[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.min(c.fromDouble(lhs), rhs)
  def max[A](rhs:A)(implicit ev:Order[A], c:ConvertableTo[A]) = ev.max(c.fromDouble(lhs), rhs)
}

class LiteralBigIntOps(val lhs:BigInt) extends AnyVal {
  def +(rhs: SafeLong) = SafeLong(lhs) + rhs
  def *(rhs: SafeLong) = SafeLong(lhs) * rhs
  def -(rhs: SafeLong) = SafeLong(lhs) - rhs
  def /(rhs: SafeLong) = SafeLong(lhs) / rhs
  def /~(rhs: SafeLong) = SafeLong(lhs) /~ rhs
  def %(rhs: SafeLong) = SafeLong(lhs) % rhs
  def /%(rhs: SafeLong) = SafeLong(lhs) /% rhs

  def +(rhs: Natural) = Natural(lhs) + rhs
  def *(rhs: Natural) = Natural(lhs) * rhs
  def -(rhs: Natural) = Natural(lhs) - rhs
  def /(rhs: Natural) = Natural(lhs) / rhs
  def /~(rhs: Natural) = Natural(lhs) /~ rhs
  def %(rhs: Natural) = Natural(lhs) % rhs
  def /%(rhs: Natural) = Natural(lhs) /% rhs

  def +(rhs: ULong) = lhs + rhs.toBigInt
  def *(rhs: ULong) = lhs * rhs.toBigInt
  def -(rhs: ULong) = lhs - rhs.toBigInt
  def /(rhs: ULong) = lhs / rhs.toBigInt
  def /~(rhs: ULong) = lhs / rhs.toBigInt
  def %(rhs: ULong) = lhs % rhs.toBigInt
  def /%(rhs: ULong) = lhs /% rhs.toBigInt
}

final class ArrayOps[@spec A](arr:Array[A]) {
  def qsum(implicit ev:Rig[A]) = {
    @tailrec
    def f(i:Int, n:Int, t:A):A = if (i < n) f(i + 1, n, ev.plus(t, arr(i))) else t
    f(0, arr.length, ev.zero)
  }

  def qproduct(implicit ev:Rig[A]) = {
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
  def qsum(implicit ev:Rig[A]) = {
    var sum = ev.zero
    as.foreach(a => sum = ev.plus(sum, a))
    sum
  }

  def qproduct(implicit ev:Rig[A]) = {
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

/**
 * `NoImplicit` provides a way to ensure that a particular implicit doesn't
 * exist. It is often useful to work-around annoying ambiguous implicit
 * problems.
 */
final class NoImplicit[A]
object NoImplicit {
  implicit def noImplicit0[A] = new NoImplicit[A]
  implicit def noImplicit1[A](implicit ev: A) = new NoImplicit[A]
}

object implicits {

  implicit def additiveMonoidOps[A:AdditiveSemigroup](a:A) = new AdditiveSemigroupOps(a)
  implicit def additiveGroupOps[A:AdditiveGroup](a:A) = new AdditiveGroupOps(a)
  implicit def multiplicativeSemigroupOps[A:MultiplicativeSemigroup](a:A) = new MultiplicativeSemigroupOps(a)
  implicit def multiplicativeGroupOps[A:MultiplicativeGroup](a:A) = new MultiplicativeGroupOps(a)

  implicit def eqOps[A:Eq](a:A) = new EqOps(a)
  implicit def orderOps[A:Order](a:A) = new OrderOps(a)
  implicit def semigroupOps[A:Semigroup](a:A) = new SemigroupOps(a)
  implicit def groupOps[A:Group](a:A) = new GroupOps(a)

  implicit def booleanAlgebraOps[A:BooleanAlgebra](a: A) = new BooleanAlgebraOps(a)
  //implicit def rigOps[A:Rig](a:A) = new RigOps(a)
  implicit def semiringOps[A:Semiring](a:A) = new SemiringOps(a)
  implicit def euclideanRingOps[A:EuclideanRing](a:A) = new EuclideanRingOps(a)
  implicit def fieldOps[A:Field](a:A) = new FieldOps(a)

  implicit def signedOps[A: Signed](a: A) = new SignedOps(a)
  implicit def nrootOps[A: NRoot](a: A) = new NRootOps(a)

  implicit def moduleOps[V, R](v:V)(implicit m:Module[V,R]) = new ModuleOps[V, R](v)
  implicit def rightModuleOps[V, R](v:V)(implicit m:RightModule[V,R]) = new RightModuleOps[V, R](v)
  implicit def vectorSpaceOps[V, F](v:V)(implicit m:VectorSpace[V,F]) = new VectorSpaceOps[V, F](v)
  implicit def normedVectorSpaceOps[V](v:V) = new NormedVectorSpaceOps[V](v)
  implicit def innerProductSpaceOps[V](v:V) = new InnerProductSpaceOps[V](v)

  implicit def literalIntOps(lhs:Int) = new LiteralIntOps(lhs)
  implicit def literalDoubleOps(lhs:Double) = new LiteralDoubleOps(lhs)
  implicit def literalBigIntOps(b: BigInt) = new LiteralBigIntOps(b)

  implicit def arrayOps[@spec A](lhs:Array[A]) = new ArrayOps(lhs)
  implicit def seqOps[@spec A](lhs:Seq[A]) = new SeqOps[A](lhs)

  implicit def intToA[A](n:Int)(implicit c:ConvertableTo[A]): A = c.fromInt(n)
  implicit def convertableOps[A:ConvertableFrom](a:A) = new ConvertableFromOps(a)
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

package object math {
  // largest possible double as BigDecimal
  private final val maxDouble = BigDecimal(Double.MaxValue)

  // natural log of largest possible double as BigDecimal
  private final val logMaxDouble = BigDecimal(Math.log(Double.MaxValue))

  // e^logMaxDouble as BigDecimal
  private final val expLogMaxDouble = BigDecimal(Math.exp(Math.log(Double.MaxValue)))

  /**
   * log() implementations
   */

  final def log(n:Double):Double = Math.log(n)

  final def log(n:BigDecimal):BigDecimal = {
    if (n < 0) sys.error("invalid argument: %s" format n)
    else _log(n, BigDecimal(0))
  }

  // Since log(n * x) = log(n) + log(x), we can use Math.log to
  // approximate log for BigDecimal.
  @tailrec private final def _log(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
    if (n <= maxDouble) BigDecimal(Math.log(n.toDouble)) + sofar
    else _log(n / maxDouble, logMaxDouble + sofar)
  }

  /**
   * exp() implementations
   */
  final def exp(n:Double):Double = Math.exp(n)

  final def exp(n:BigDecimal):BigDecimal = _exp(n, BigDecimal(1))

  // Since exp(a + b) = exp(a) * exp(b), we can use Math.log to
  // approximate exp for BigDecimal.
  @tailrec private final def _exp(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
    if (n <= logMaxDouble) BigDecimal(Math.exp(n.toDouble)) * sofar
    else _exp(n - logMaxDouble, maxDouble * sofar)
  }

  /**
   * pow() implementations
   */

  // Since a^b = e^(log(a) * b) we can use exp and log to write pow.
  // TODO: doesn't make precision guarantees, but it's better than nothing.
  private val maxIntEx = BigDecimal(999999999)
  private val minIntEx = BigDecimal(-999999999)

  final def pow(base:BigDecimal, ex:BigDecimal) =
    if (ex.isValidInt && ex <= maxIntEx && ex >= minIntEx) base.pow(ex.toInt)
    else exp(log(base) * ex)

  /**
   * Exponentiation function, e.g. x^y
   *
   * If base^ex doesn't fit in a Long, the result will overflow (unlike
   * Math.pow which will return +/- Infinity). 
   */
  final def pow(base:Long, ex:Long):Long = if (ex < 0L) {
    if(base == 0L) sys.error("zero can't be raised to negative power")
    else if (base == 1L) 1L
    else if (base == -1L) if ((ex & 1L) == 0L) -1L else 1L
    else 0L
  } else {
    _pow(1L, base, ex)
  }

  @tailrec final def _pow(t:Long, b:Long, e:Long): Long = {
    if (e == 0L) t
    else if ((e & 1) == 1) _pow(t * b, b * b, e >> 1L)
    else _pow(t, b * b, e >> 1L)
  }

  final def pow(base:Double, exponent:Double) = Math.pow(base, exponent)

  def gcd(_x: Long, _y: Long): Long = {
    if (_x == 0L) return _y
    if (_y == 0L) return _x
  
    var x = Math.abs(_x)
    var xz = numberOfTrailingZeros(x)
    x >>= xz
  
    var y = Math.abs(_y)
    var yz = numberOfTrailingZeros(y)
    y >>= yz
  
    while (x != y) {
      if (x > y) {
        x -= y
        x >>= numberOfTrailingZeros(x)
      } else {
        y -= x
        y >>= numberOfTrailingZeros(y)
      }
    }
  
    if (xz < yz) x << xz else x << yz
  }


  final def gcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  final def gcd(a: BigInteger, b: BigInteger): BigInteger = a.gcd(b)

  def round(a: Float): Float =
    if (Math.abs(a) >= 16777216.0F) a else Math.round(a).toFloat

  def round(a: Double): Double =
    if (Math.abs(a) >= 4503599627370496.0) a else Math.round(a).toDouble

  @inline final def min(x: Int, y: Int): Int = Math.min(x, y)
  @inline final def min(x: Long, y: Long): Long = Math.min(x, y)
  @inline final def min(x: Float, y: Float): Float = Math.min(x, y)
  @inline final def min(x: Double, y: Double): Double = Math.min(x, y)
  final def min[A](x: A, y: A)(implicit ev: Order[A]) = ev.min(x, y)

  @inline final def max(x: Int, y: Int): Int = Math.max(x, y)
  @inline final def max(x: Long, y: Long): Long = Math.max(x, y)
  @inline final def max(x: Float, y: Float): Float = Math.max(x, y)
  @inline final def max(x: Double, y: Double): Double = Math.max(x, y)
  final def max[A](x: A, y: A)(implicit ev: Order[A]) = ev.max(x, y)
}

package optional {
  object totalfloat {
    trait TotalFloatEq extends Eq[Float] {
      def eqv(x:Float, y:Float) = java.lang.Float.compare(x, y) == 0
      override def neqv(x:Float, y:Float) = java.lang.Float.compare(x, y) != 0
    }
    trait TotalFloatOrder extends Order[Float] with TotalFloatEq {
      override def gt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
      override def gteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
      override def lt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
      override def lteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
      override def min(x: Float, y: Float) = if (java.lang.Float.compare(x, y) < 0) x else y
      override def max(x: Float, y: Float) = Math.max(x, y)
      def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
    }
    implicit object TotalFloatOrder extends TotalFloatOrder
  
    trait TotalDoubleEq extends Eq[Double] {
      def eqv(x:Double, y:Double) = java.lang.Double.compare(x, y) == 0
      override def neqv(x:Double, y:Double) = java.lang.Double.compare(x, y) != 0
    }
    trait TotalDoubleOrder extends Order[Double] with TotalDoubleEq {
      override def gt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
      override def gteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
      override def lt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
      override def lteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
      override def min(x: Double, y: Double) = if (java.lang.Double.compare(x, y) < 0) x else y
      override def max(x: Double, y: Double) = Math.max(x, y)
      def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
    }
    implicit object TotalDoubleOrder extends TotalDoubleOrder
  }
}
