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


trait WideningConversionLow {
  implicit def transitiveWiden[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                               @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B,
                               @spec(Byte, Short, Int, Long, Float, Double, AnyRef) C]
      (implicit c1: WideningConversion[A, B], c2: WideningConversion[B, C]) = {
    new WideningConversion[A, C] {
      def convert(a: A): C = c2.convert(c1.convert(a))
    }
  }
}


object Conversion {
  type FromInt[A] = WideningConversion[Int, A]
  type ToDouble[A] = NarrowingConversion[A, Double]

  def apply[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit c: Conversion[A, B]) = c
}

object WideningConversion extends WideningConversionLow {
  def apply[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit c: WideningConversion[A, B]) = c
  
  def widen[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](f: A => B) = new WideningConversion[A, B] {
    def convert(a: A): B = f(a)
  }

  implicit val byte2int = widen[Byte, Int](n => n)
  implicit val short2int = widen[Short, Int](n => n)

  implicit val int2int = widen[Int, Int](n => n)
  implicit val int2long = widen[Int, Long](_.toLong)
  implicit val int2double = widen[Int, Double](_.toDouble)
  implicit val int2bigDecimal = widen[Int, BigDecimal](BigDecimal(_))
  implicit val int2bigInt = widen[Int, BigInt](BigInt(_))
  implicit val int2rational = widen[Int, Rational](Rational(_))
  implicit val int2real = widen[Int, Real](Real(_))

  implicit val long2long = widen[Long, Long](n => n)
  implicit val long2bigInt = widen[Long, BigInt](BigInt(_))
  implicit val long2bigDecimal = widen[Long, BigDecimal](BigDecimal(_))
  implicit val long2rational = widen[Long, Rational](Rational(_))
  implicit val long2real = widen[Long, Real](Real(_))

  implicit val bigInt2bigInt = widen[BigInt, BigInt](n => n)
  implicit val bigInt2bigDecimal = widen[BigInt, BigDecimal](BigDecimal(_))
  implicit val bigInt2rational = widen[BigInt, Rational](Rational(_))
  implicit val bigInt2real = widen[BigInt, Real](Real(_))

  implicit val float2float = widen[Float, Float](n => n)
  implicit val float2double = widen[Float, Double](_.toDouble)
  implicit val float2bigDecimal = widen[Float, BigDecimal](BigDecimal(_))
  implicit val float2rational = widen[Float, Rational](Rational(_))
  implicit val float2real = widen[Float, Real](Real(_))
  
  implicit val double2double = widen[Double, Double](n => n)
  implicit val double2bigDecimal = widen[Double, BigDecimal](BigDecimal(_))
  implicit val double2rational = widen[Double, Rational](Rational(_))
  implicit val double2real = widen[Double, Real](Real(_))

  implicit val bigDecimal2rational = widen[BigDecimal, Rational](Rational(_))
  implicit val bigDecimal2real = widen[BigDecimal, Real](Real(_))

  implicit val rational2rational = widen[Rational, Rational](n => n)
  implicit val rational2real = widen[Rational, Real](Real(_))

  implicit val real2real = widen[Real, Real](n => n)

  implicit def complex2complex[A, B]
  (implicit c: WideningConversion[A, B], f: Fractional[B], t: Trig[B]) =
    widen[Complex[A], Complex[B]](x => Complex(c.convert(x.real), c.convert(x.imag)))

  implicit def realish2complex[A, B]
  (implicit c: WideningConversion[A, B], f: Fractional[B], t: Trig[B]) =
    widen[A, Complex[B]](x => Complex(c.convert(x), Fractional[B].zero))
}


trait NarrowingConversionLow0 {
  implicit def transitiveNarrow[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                                @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B,
                                @spec(Byte, Short, Int, Long, Float, Double, AnyRef) C](implicit
      c1: NarrowingConversion[A, B], c2: NarrowingConversion[B, C]) = {
    new NarrowingConversion[A, C] {
      def convert(a: A): C = c2.convert(c1.convert(a))
    }
  }
}

trait NarrowingConversionLow1 extends NarrowingConversionLow0 {
  implicit def widen[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
                     @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit c: WideningConversion[A, B]) = {
    new NarrowingConversion[A, B] {
      def convert(a: A): B = c.convert(a)
    }
  }
}

object NarrowingConversion extends NarrowingConversionLow1 {
  def apply[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
            @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](implicit c: NarrowingConversion[A, B]) = c

  def narrow[@spec(Byte, Short, Int, Long, Float, Double, AnyRef) A,
             @spec(Byte, Short, Int, Long, Float, Double, AnyRef) B](f: A => B) = new NarrowingConversion[A, B] {
    def convert(a: A): B = f(a)
  }

  implicit val int2float = narrow[Int, Float](_.toFloat)

  implicit val long2int = narrow[Long, Int](_.toInt)
  implicit val long2float = narrow[Long, Float](_.toFloat)
  implicit val long2double = narrow[Long, Double](_.toDouble)

  implicit val bigInt2int = narrow[BigInt, Int](_.toInt)
  implicit val bigInt2long = narrow[BigInt, Long](_.toLong)
  implicit val bigInt2float = narrow[BigInt, Float](_.toFloat)
  implicit val bigInt2double = narrow[BigInt, Double](_.toDouble)

  implicit val float2int = narrow[Float, Int](_.toInt)
  implicit val float2long = narrow[Float, Long](_.toLong)
  // float2bigInt = float -> bigDecimal o bigDecimal -> BigInt

  implicit val double2int = narrow[Double, Int](_.toInt)
  implicit val double2long = narrow[Double, Long](_.toLong)
  implicit val double2float = narrow[Double, Float](_.toFloat)

  implicit val bigDecimal2int = narrow[BigDecimal, Int](_.toInt)
  implicit val bigDecimal2long = narrow[BigDecimal, Long](_.toLong)
  implicit val bigDecimal2bigInt = narrow[BigDecimal, BigInt](_.toBigInt)
  implicit val bigDecimal2float = narrow[BigDecimal, Float](_.toFloat)
  implicit val bigDecimal2double = narrow[BigDecimal, Double](_.toDouble)

  implicit val rational2int = narrow[Rational, Int](_.toInt)
  implicit val rational2long = narrow[Rational, Long](_.toLong)
  implicit val rational2bigInt = narrow[Rational, BigInt](_.toBigInt)
  implicit val rational2float = narrow[Rational, Float](_.toFloat)
  implicit val rational2double = narrow[Rational, Double](_.toDouble)
  implicit def rational2bigDecimal(implicit mc: MathContext) =
    narrow[Rational, BigDecimal](_.toBigDecimal(mc))

  implicit val real2int = narrow[Real, Int](_.toInt)
  implicit val real2long = narrow[Real, Long](_.toLong)
  implicit val real2bigInt = narrow[Real, BigInt](_.toBigInt)
  implicit val real2float = narrow[Real, Float](_.toFloat)
  implicit val real2double = narrow[Real, Double](_.toDouble)
  implicit def real2bigDecimal(implicit mc: MathContext) =
    narrow[Real, BigDecimal](_.toBigDecimal(mc))

  implicit def complex2complex[A, B]
  (implicit c: NarrowingConversion[A, B], f: Fractional[B], t: Trig[B]) =
    narrow[Complex[A], Complex[B]](x => Complex(c.convert(x.real), c.convert(x.imag)))

  implicit def realish2complex[A, B]
  (implicit c: NarrowingConversion[A, B], f: Fractional[B], t: Trig[B]) =
    narrow[A, Complex[B]](x => Complex(c.convert(x), Fractional[B].zero))
}

