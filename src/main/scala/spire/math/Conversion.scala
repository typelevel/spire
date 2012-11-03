package spire.math

import scala.language.experimental.macros

import java.math.MathContext

import scala.{ specialized => spec }

import spire.macrosk._


/**
 * A type class for numeric conversions. All conversions must extend either
 * `NarrowingConversion` for those lose information, and `WideningConversion`
 * for those that are guaranteed not to lose any information.
 */
sealed trait Conversion[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                        @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B] {
  def convert(a: A): B
}

/**
 * A widening conversion is one which does not cause any information lost. For
 * example, an `Int` can always be embedded in a `Long`. On the other hand,
 * A `Long` cannot be embedded into an `Int` and thus is not a widening
 * conversion.
 *
 * It should be noted that `Int` -> `Float` and `Long` -> `Double` are not
 * widening conversions.
 */
trait WideningConversion[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                         @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B] extends Conversion[A, B] {
  def widen(a: A): B = convert(a)
}

/**
 * A narrowing conversion is one that may cause some information loss. For
 * instance, if we convert a `Long` to a `Double`, we may lose some of the
 * lesser bits, since a `Double` can only represent integers up to 53 bits
 * exactly. After that, we start to lose bits.
 *
 * Both `Int` -> `Float` and `Long` -> `Double` are narrowing conversions.
 */
trait NarrowingConversion[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                          @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B] extends Conversion[A, B] {
  def narrow(a: A): B = convert(a)
}


object Conversion {
  type FromInt[A] = WideningConversion[Int, A]
  type ToDouble[A] = NarrowingConversion[A, Double]

  def apply[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit c: Conversion[A, B]) = c
}


trait WideningConversionLow {

  // This diverges... unless we import WideniningConversion._ explicitly.
  // Argh! Need to figure out these priority problems.

  //implicit def transitiveWiden[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
  //                             @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B]
  //    (implicit c1: WideningConversion[A, Int], c2: WideningConversion[Int, B]) = {
  //  new WideningConversion[A, B] {
  //    def convert(a: A): B = c2.convert(c1.convert(a))
  //  }
  //}
}


object WideningConversion extends WideningConversionLow {
  def apply[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit c: WideningConversion[A, B]) = c
  
  def widen[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](f: A => B) = new WideningConversion[A, B] {
    def convert(a: A): B = f(a)
  }

  implicit def identity[@spec A] = new WideningConversion[A, A] {
    def convert(a: A): A = a
  }

  implicit val byte2short = widen[Byte, Short](n => n)
  implicit val byte2int = widen[Byte, Int](n => n)
  implicit val byte2long = widen[Byte, Long](n => n)
  implicit val byte2float = widen[Byte, Float](n => n.toFloat)
  implicit val byte2double = widen[Byte, Double](n => n.toDouble)
  implicit val byte2bigDecimal = widen[Byte, BigDecimal](n => BigDecimal(n.toInt))
  implicit val byte2bigInt = widen[Byte, BigInt](n => BigInt(n.toInt))
  implicit val byte2rational = widen[Byte, Rational](n => Rational(n.toInt))
  implicit val byte2real = widen[Byte, Real](n => Real(n.toInt))

  implicit val short2int = widen[Short, Int](n => n)
  implicit val short2long = widen[Short, Long](n => n)
  implicit val short2float = widen[Short, Float](n => n.toFloat)
  implicit val short2double = widen[Short, Double](n => n.toDouble)
  implicit val short2bigDecimal = widen[Short, BigDecimal](n => BigDecimal(n.toInt))
  implicit val short2bigInt = widen[Short, BigInt](n => BigInt(n.toInt))
  implicit val short2rational = widen[Short, Rational](n => Rational(n.toInt))
  implicit val short2real = widen[Short, Real](n => Real(n.toInt))

  implicit val int2long = widen[Int, Long](_.toLong)
  implicit val int2double = widen[Int, Double](_.toDouble)
  implicit val int2bigDecimal = widen[Int, BigDecimal](BigDecimal(_))
  implicit val int2bigInt = widen[Int, BigInt](BigInt(_))
  implicit val int2rational = widen[Int, Rational](Rational(_))
  implicit val int2real = widen[Int, Real](Real(_))

  implicit val long2bigInt = widen[Long, BigInt](BigInt(_))
  implicit val long2bigDecimal = widen[Long, BigDecimal](BigDecimal(_))
  implicit val long2rational = widen[Long, Rational](Rational(_))
  implicit val long2real = widen[Long, Real](Real(_))

  implicit val bigInt2bigDecimal = widen[BigInt, BigDecimal](BigDecimal(_))
  implicit val bigInt2rational = widen[BigInt, Rational](Rational(_))
  implicit val bigInt2real = widen[BigInt, Real](Real(_))

  implicit val float2double = widen[Float, Double](_.toDouble)
  implicit val float2bigDecimal = widen[Float, BigDecimal](BigDecimal(_))
  implicit val float2rational = widen[Float, Rational](Rational(_))
  implicit val float2real = widen[Float, Real](Real(_))
  
  implicit val double2bigDecimal = widen[Double, BigDecimal](BigDecimal(_))
  implicit val double2rational = widen[Double, Rational](Rational(_))
  implicit val double2real = widen[Double, Real](Real(_))

  implicit val bigDecimal2rational = widen[BigDecimal, Rational](Rational(_))
  implicit val bigDecimal2real = widen[BigDecimal, Real](Real(_))

  implicit val rational2real = widen[Rational, Real](Real(_))

  implicit def complex2complex[A, B]
  (implicit c: WideningConversion[A, B], f: Fractional[B], t: Trig[B]) =
    widen[Complex[A], Complex[B]](x => Complex(c.convert(x.real), c.convert(x.imag)))

  implicit def realish2complex[A, B]
  (implicit c: WideningConversion[A, B], f: Fractional[B], t: Trig[B]) =
    widen[A, Complex[B]](x => Complex(c.convert(x), Fractional[B].zero))
}


trait NarrowingConversionLow0 {
  //implicit def transitiveNarrow1[A, B, C](implicit c1: NarrowingConversion[A, Long, B]
  implicit def transitiveNarrow1[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                                 @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit
      c1: NarrowingConversion[A, Double], c2: NarrowingConversion[Double, B]) = {
    new NarrowingConversion[A, B] {
      def convert(a: A): B = c2.convert(c1.convert(a))
    }
  }
}

trait NarrowingConversionLow1 extends NarrowingConversionLow0 {
  implicit def transitiveNarrow2[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                                 @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit
      c1: NarrowingConversion[A, Int], c2: NarrowingConversion[Int, B]) = {
    new NarrowingConversion[A, B] {
      def convert(a: A): B = c2.convert(c1.convert(a))
    }
  }
}

trait Not[A]
object Not {
  implicit def not[A]: Not[A] = new Not[A] { }
  implicit def notAmbig[A](implicit a: A): Not[A] = new Not[A] { }
}

trait NarrowingConversionLow2 /*extends NarrowingConversionLow1*/ {
  implicit def widen[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                     @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit
      c: WideningConversion[A, B], ev: Not[(A =:= B)]) = {
    new NarrowingConversion[A, B] {
      def convert(a: A): B = c.convert(a)
    }
  }
}

object NarrowingConversion extends NarrowingConversionLow2 {
  def apply[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit c: NarrowingConversion[A, B]) = c

  def narrow[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
             @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](f: A => B) = new NarrowingConversion[A, B] {
    def convert(a: A): B = f(a)
  }

  private def clip[@spec A, @spec B](x: B, _low: A, _high: A)(implicit
      o: Order[B], c: WideningConversion[A, B]) = {
    import spire.implicits._

    val low = c.convert(_low)
    val high = c.convert(_high)
    if (o.lt(x, low)) low else if (o.gt(x, high)) high else x
  }

  implicit val short2byte = narrow[Short, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)

  implicit val int2byte = narrow[Int, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val int2short = narrow[Int, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val int2float = narrow[Int, Float](_.toFloat)

  implicit val long2byte = narrow[Long, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val long2short = narrow[Long, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val long2int = narrow[Long, Int](clip(_, Int.MinValue, Int.MaxValue).toInt)
  implicit val long2float = narrow[Long, Float](_.toFloat)
  implicit val long2double = narrow[Long, Double](_.toDouble)

  implicit val bigInt2byte = narrow[BigInt, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val bigInt2short = narrow[BigInt, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val bigInt2int =
    narrow[BigInt, Int](clip(_, Int.MinValue, Int.MaxValue).toInt)
  implicit val bigInt2long =
    narrow[BigInt, Long](clip(_, Long.MinValue, Long.MaxValue).toLong)
  implicit val bigInt2float = narrow[BigInt, Float](_.toFloat)
  implicit val bigInt2double = narrow[BigInt, Double](_.toDouble)

  implicit val float2byte = narrow[Float, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val float2short = narrow[Float, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val float2int = narrow[Float, Int](_.toInt)
  implicit val float2long = narrow[Float, Long](_.toLong)

  implicit val double2byte = narrow[Double, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val double2short = narrow[Double, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val double2int = narrow[Double, Int](_.toInt)
  implicit val double2long = narrow[Double, Long](_.toLong)
  implicit val double2float = narrow[Double, Float](_.toFloat)

  implicit val bigDecimal2byte =
    narrow[BigDecimal, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val bigDecimal2short =
    narrow[BigDecimal, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val bigDecimal2int = narrow[BigDecimal, Int](_.toInt)
  implicit val bigDecimal2long = narrow[BigDecimal, Long](_.toLong)
  implicit val bigDecimal2bigInt = narrow[BigDecimal, BigInt](_.toBigInt)
  implicit val bigDecimal2float = narrow[BigDecimal, Float](_.toFloat)
  implicit val bigDecimal2double = narrow[BigDecimal, Double](_.toDouble)

  implicit val rational2byte =
    narrow[Rational, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val rational2short =
    narrow[Rational, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val rational2int =
    narrow[Rational, Int](clip(_, Int.MinValue, Int.MaxValue).toInt)
  implicit val rational2long =
    narrow[Rational, Long](clip(_, Long.MinValue, Long.MaxValue).toLong)
  implicit val rational2bigInt = narrow[Rational, BigInt](_.toBigInt)
  implicit val rational2float = narrow[Rational, Float](_.toFloat)
  implicit val rational2double = narrow[Rational, Double](_.toDouble)
  implicit def rational2bigDecimal(implicit mc: MathContext) =
    narrow[Rational, BigDecimal](_.toBigDecimal(mc))

  implicit val real2byte = narrow[Real, Byte](clip(_, Byte.MinValue, Byte.MaxValue).toByte)
  implicit val real2short = narrow[Real, Short](clip(_, Short.MinValue, Short.MaxValue).toShort)
  implicit val real2int =
    narrow[Real, Int](clip(_, Int.MinValue, Int.MaxValue).toInt)
  implicit val real2long =
    narrow[Real, Long](clip(_, Long.MinValue, Long.MaxValue).toLong)
  implicit val real2bigInt = narrow[Real, BigInt](_.toBigInt)
  implicit val real2float = narrow[Real, Float](_.toFloat)
  implicit val real2double = narrow[Real, Double](_.toDouble)
  implicit def real2bigDecimal(implicit mc: MathContext) =
    narrow[Real, BigDecimal](_.toBigDecimal(mc))
  implicit val real2rational = narrow[Real, Rational](_.toRational)

  implicit def complex2complex[A, B]
  (implicit c: NarrowingConversion[A, B], f: Fractional[B], t: Trig[B]) =
    narrow[Complex[A], Complex[B]](x => Complex(c.convert(x.real), c.convert(x.imag)))

  implicit def realish2complex[A, B]
  (implicit c: NarrowingConversion[A, B], f: Fractional[B], t: Trig[B]) =
    narrow[A, Complex[B]](x => Complex(c.convert(x), Fractional[B].zero))
}

