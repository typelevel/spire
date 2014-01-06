package spire.math

import scala.{specialized => sp}

trait UnsafeConversion[@sp(Byte, Short, Int, Long, Float, Double) A, @sp(Byte, Short, Int, Long, Float, Double) B] {
  def convert(a: A): B
}

trait Conversion[@sp(Byte, Short, Int, Long, Float, Double) A, @sp(Byte, Short, Int, Long, Float, Double) B] extends UnsafeConversion[A, B] {
  def convert(a: A): B
}

class ConversionOps[@sp(Byte, Short, Int, Long, Float, Double) A](a: A) {
  def convert[@sp(Byte, Short, Int, Long, Float, Double) B](implicit ev: Conversion[A, B]): B =
    ev.convert(a)
  def unsafeConvert[@sp(Byte, Short, Int, Long, Float, Double) B](implicit ev: UnsafeConversion[A, B]): B =
    ev.convert(a)
}

object Conversion {
  implicit def noop[A] = new Conversion[A, A] { def convert(a: A): A = a }

  import scala.reflect.ClassTag
  import spire.algebra.{Eq, Order, Semiring, AdditiveMonoid}
  import spire.math.poly.Term

  def apply[@sp(Byte, Short, Int, Long, Float, Double) A, @sp(Byte, Short, Int, Long, Float, Double) B](f: A => B): Conversion[A, B] =
    new Conversion[A, B] {
      def convert(a: A): B = f(a)
    }

  // convert to Complex[_]
  implicit def RealToComplex[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    new Conversion[A, Complex[B]] {
      def convert(a: A): Complex[B] = Complex(ev.convert(a))
    }

  implicit def ComplexToComplex[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    new Conversion[Complex[A], Complex[B]] {
      def convert(a: Complex[A]): Complex[B] = Complex(ev.convert(a.real), ev.convert(a.imag))
    }

  // convert to Quaternion[_]
  implicit def RealToQuaternion[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    new Conversion[A, Quaternion[B]] {
      def convert(a: A): Quaternion[B] = Quaternion(ev.convert(a))
    }

  implicit def ComplexToQuaternion[A, B](implicit ev: Conversion[A, Complex[B]], f: Semiring[B]) =
    new Conversion[A, Quaternion[B]] {
      def convert(a: A): Quaternion[B] = Quaternion(ev.convert(a))
    }

  implicit def QuaternionToQuaternion[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    new Conversion[Quaternion[A], Quaternion[B]] {
      def convert(a: Quaternion[A]): Quaternion[B] =
        Quaternion(ev.convert(a.r), ev.convert(a.i), ev.convert(a.j), ev.convert(a.k))
    }

  // convert to Interval[_]
  implicit def RealToInterval[A, B](implicit ev: Conversion[A, B], o: Order[B]) =
    new Conversion[A, Interval[B]] {
      def convert(a: A): Interval[B] = Interval.point(ev.convert(a))
    }

  implicit def IntervalToInterval[A, B](implicit ev: Conversion[A, B], f: AdditiveMonoid[B], o: Order[B]) =
    new Conversion[Interval[A], Interval[B]] {
      def convert(a: Interval[A]): Interval[B] = a.mapBounds(ev.convert)
    }

  // convert to Polynomial[_]
  implicit def RealToPolynomial[A, B](implicit ev: Conversion[A, B], e: Eq[B], f: Semiring[B], ct: ClassTag[B]) =
    new Conversion[A, Polynomial[B]] {
      def convert(a: A): Polynomial[B] = Polynomial.constant(ev.convert(a))
    }

  implicit def PolynomialToPolynomial[A, B](implicit ev: Conversion[A, B], ea: Eq[A], eb: Eq[B], fa: Semiring[A], fb: Semiring[B], ct: ClassTag[B]) =
    new Conversion[Polynomial[A], Polynomial[B]] {
      def convert(a: Polynomial[A]): Polynomial[B] =
        Polynomial(a.terms.map { case Term(c, e) => Term(ev.convert(c), e) })
    }

  // UByte conversions
  implicit val UByteToUShort = new Conversion[UByte, UShort] { def convert(n: UByte): UShort = UShort(n.toInt) }
  implicit val UByteToUInt = new Conversion[UByte, UInt] { def convert(n: UByte): UInt = UInt(n.toInt) }
  implicit val UByteToULong = new Conversion[UByte, ULong] { def convert(n: UByte): ULong = ULong(n.toInt) }
  implicit val UByteToNatural = new Conversion[UByte, Natural] { def convert(n: UByte): Natural = Natural(n.toInt) }
  implicit val UByteToShort = new Conversion[UByte, Short] { def convert(n: UByte): Short = n.toShort }
  implicit val UByteToInt = new Conversion[UByte, Int] { def convert(n: UByte): Int = n.toInt }
  implicit val UByteToLong = new Conversion[UByte, Long] { def convert(n: UByte): Long = n.toLong }
  implicit val UByteToBigInt = new Conversion[UByte, BigInt] { def convert(n: UByte): BigInt = n.toBigInt }
  implicit val UByteToSafeLong = new Conversion[UByte, SafeLong] { def convert(n: UByte): SafeLong = SafeLong(n.toInt) }
  implicit val UByteToFloat = new Conversion[UByte, Float] { def convert(n: UByte): Float = n.toFloat }
  implicit val UByteToDouble = new Conversion[UByte, Double] { def convert(n: UByte): Double = n.toDouble }
  implicit val UByteToBigDecimal = new Conversion[UByte, BigDecimal] { def convert(n: UByte): BigDecimal = BigDecimal(n.toInt) }
  implicit val UByteToRational = new Conversion[UByte, Rational] { def convert(n: UByte): Rational = Rational(n.toInt) }
  implicit val UByteToAlgebraic = new Conversion[UByte, Algebraic] { def convert(n: UByte): Algebraic = Algebraic(n.toInt) }
  implicit val UByteToReal = new Conversion[UByte, Real] { def convert(n: UByte): Real = Real(n.toInt) }

  // UShort conversions
  implicit val UShortToUInt = new Conversion[UShort, UInt] { def convert(n: UShort): UInt = UInt(n.toInt) }
  implicit val UShortToULong = new Conversion[UShort, ULong] { def convert(n: UShort): ULong = ULong(n.toInt) }
  implicit val UShortToNatural = new Conversion[UShort, Natural] { def convert(n: UShort): Natural = Natural(n.toInt) }
  implicit val UShortToInt = new Conversion[UShort, Int] { def convert(n: UShort): Int = n.toInt }
  implicit val UShortToLong = new Conversion[UShort, Long] { def convert(n: UShort): Long = n.toLong }
  implicit val UShortToBigInt = new Conversion[UShort, BigInt] { def convert(n: UShort): BigInt = n.toBigInt }
  implicit val UShortToSafeLong = new Conversion[UShort, SafeLong] { def convert(n: UShort): SafeLong = SafeLong(n.toInt) }
  implicit val UShortToFloat = new Conversion[UShort, Float] { def convert(n: UShort): Float = n.toFloat }
  implicit val UShortToDouble = new Conversion[UShort, Double] { def convert(n: UShort): Double = n.toDouble }
  implicit val UShortToBigDecimal = new Conversion[UShort, BigDecimal] { def convert(n: UShort): BigDecimal = BigDecimal(n.toInt) }
  implicit val UShortToRational = new Conversion[UShort, Rational] { def convert(n: UShort): Rational = Rational(n.toInt) }
  implicit val UShortToAlgebraic = new Conversion[UShort, Algebraic] { def convert(n: UShort): Algebraic = Algebraic(n.toInt) }
  implicit val UShortToReal = new Conversion[UShort, Real] { def convert(n: UShort): Real = Real(n.toInt) }

  // UInt conversions
  implicit val UIntToULong = new Conversion[UInt, ULong] { def convert(n: UInt): ULong = ULong(n.toLong) }
  implicit val UIntToNatural = new Conversion[UInt, Natural] { def convert(n: UInt): Natural = Natural(n.toLong) }
  implicit val UIntToShort = new Conversion[UInt, Short] { def convert(n: UInt): Short = n.toShort }
  implicit val UIntToLong = new Conversion[UInt, Long] { def convert(n: UInt): Long = n.toLong }
  implicit val UIntToBigInt = new Conversion[UInt, BigInt] { def convert(n: UInt): BigInt = n.toBigInt }
  implicit val UIntToSafeLong = new Conversion[UInt, SafeLong] { def convert(n: UInt): SafeLong = SafeLong(n.toLong) }
  implicit val UIntToDouble = new Conversion[UInt, Double] { def convert(n: UInt): Double = n.toDouble }
  implicit val UIntToBigDecimal = new Conversion[UInt, BigDecimal] { def convert(n: UInt): BigDecimal = BigDecimal(n.toLong) }
  implicit val UIntToRational = new Conversion[UInt, Rational] { def convert(n: UInt): Rational = Rational(n.toLong) }
  implicit val UIntToAlgebraic = new Conversion[UInt, Algebraic] { def convert(n: UInt): Algebraic = Algebraic(n.toLong) }
  implicit val UIntToReal = new Conversion[UInt, Real] { def convert(n: UInt): Real = Real(n.toLong) }

  // ULong conversions
  implicit val ULongToNatural = new Conversion[ULong, Natural] { def convert(n: ULong): Natural = n.toNatural }
  implicit val ULongToBigInt = new Conversion[ULong, BigInt] { def convert(n: ULong): BigInt = n.toBigInt }
  implicit val ULongToSafeLong = new Conversion[ULong, SafeLong] { def convert(n: ULong): SafeLong = n.toSafeLong }
  implicit val ULongToBigDecimal = new Conversion[ULong, BigDecimal] { def convert(n: ULong): BigDecimal = BigDecimal(n.toBigInt) }
  implicit val ULongToRational = new Conversion[ULong, Rational] { def convert(n: ULong): Rational = Rational(n.toSafeLong) }
  implicit val ULongToAlgebraic = new Conversion[ULong, Algebraic] { def convert(n: ULong): Algebraic = Algebraic(n.toSafeLong) }
  implicit val ULongToReal = new Conversion[ULong, Real] { def convert(n: ULong): Real = Real(n.toSafeLong) }

  // Natural conversions
  implicit val NaturalToBigInt = new Conversion[Natural, BigInt] { def convert(n: Natural): BigInt = n.toBigInt }
  implicit val NaturalToSafeLong = new Conversion[Natural, SafeLong] { def convert(n: Natural): SafeLong = SafeLong(n.toBigInt) }
  implicit val NaturalToBigDecimal = new Conversion[Natural, BigDecimal] { def convert(n: Natural): BigDecimal = BigDecimal(n.toBigInt) }
  implicit val NaturalToRational = new Conversion[Natural, Rational] { def convert(n: Natural): Rational = Rational(n.toBigInt) }
  implicit val NaturalToAlgebraic = new Conversion[Natural, Algebraic] { def convert(n: Natural): Algebraic = Algebraic(n.toBigInt) }
  implicit val NaturalToReal = new Conversion[Natural, Real] { def convert(n: Natural): Real = Real(n.toBigInt) }

  // Byte conversions
  implicit val ByteToShort = new Conversion[Byte, Short] { def convert(n: Byte): Short = n.toShort }
  implicit val ByteToInt = new Conversion[Byte, Int] { def convert(n: Byte): Int = n.toInt }
  implicit val ByteToLong = new Conversion[Byte, Long] { def convert(n: Byte): Long = n.toLong }
  implicit val ByteToBigInt = new Conversion[Byte, BigInt] { def convert(n: Byte): BigInt = BigInt(n) }
  implicit val ByteToSafeLong = new Conversion[Byte, SafeLong] { def convert(n: Byte): SafeLong = SafeLong(n) }
  implicit val ByteToFloat = new Conversion[Byte, Float] { def convert(n: Byte): Float = n.toFloat }
  implicit val ByteToDouble = new Conversion[Byte, Double] { def convert(n: Byte): Double = n.toDouble }
  implicit val ByteToBigDecimal = new Conversion[Byte, BigDecimal] { def convert(n: Byte): BigDecimal = BigDecimal(n) }
  implicit val ByteToRational = new Conversion[Byte, Rational] { def convert(n: Byte): Rational = Rational(n) }
  implicit val ByteToAlgebraic = new Conversion[Byte, Algebraic] { def convert(n: Byte): Algebraic = Algebraic(n) }
  implicit val ByteToReal = new Conversion[Byte, Real] { def convert(n: Byte): Real = Real(n) }

  // Short conversions
  implicit val ShortToInt = new Conversion[Short, Int] { def convert(n: Short): Int = n.toInt }
  implicit val ShortToLong = new Conversion[Short, Long] { def convert(n: Short): Long = n.toLong }
  implicit val ShortToBigInt = new Conversion[Short, BigInt] { def convert(n: Short): BigInt = BigInt(n) }
  implicit val ShortToSafeLong = new Conversion[Short, SafeLong] { def convert(n: Short): SafeLong = SafeLong(n) }
  implicit val ShortToFloat = new Conversion[Short, Float] { def convert(n: Short): Float = n.toFloat }
  implicit val ShortToDouble = new Conversion[Short, Double] { def convert(n: Short): Double = n.toDouble }
  implicit val ShortToBigDecimal = new Conversion[Short, BigDecimal] { def convert(n: Short): BigDecimal = BigDecimal(n) }
  implicit val ShortToRational = new Conversion[Short, Rational] { def convert(n: Short): Rational = Rational(n) }
  implicit val ShortToAlgebraic = new Conversion[Short, Algebraic] { def convert(n: Short): Algebraic = Algebraic(n) }
  implicit val ShortToReal = new Conversion[Short, Real] { def convert(n: Short): Real = Real(n) }

  // Int conversions
  implicit val IntToLong = new Conversion[Int, Long] { def convert(n: Int): Long = n.toLong }
  implicit val IntToBigInt = new Conversion[Int, BigInt] { def convert(n: Int): BigInt = BigInt(n) }
  implicit val IntToSafeLong = new Conversion[Int, SafeLong] { def convert(n: Int): SafeLong = SafeLong(n) }
  implicit val IntToDouble = new Conversion[Int, Double] { def convert(n: Int): Double = n.toDouble }
  implicit val IntToBigDecimal = new Conversion[Int, BigDecimal] { def convert(n: Int): BigDecimal = BigDecimal(n) }
  implicit val IntToRational = new Conversion[Int, Rational] { def convert(n: Int): Rational = Rational(n) }
  implicit val IntToAlgebraic = new Conversion[Int, Algebraic] { def convert(n: Int): Algebraic = Algebraic(n) }
  implicit val IntToReal = new Conversion[Int, Real] { def convert(n: Int): Real = Real(n) }

  // Long conversions
  implicit val LongToBigInt = new Conversion[Long, BigInt] { def convert(n: Long): BigInt = BigInt(n) }
  implicit val LongToSafeLong = new Conversion[Long, SafeLong] { def convert(n: Long): SafeLong = SafeLong(n) }
  implicit val LongToBigDecimal = new Conversion[Long, BigDecimal] { def convert(n: Long): BigDecimal = BigDecimal(n) }
  implicit val LongToRational = new Conversion[Long, Rational] { def convert(n: Long): Rational = Rational(n) }
  implicit val LongToAlgebraic = new Conversion[Long, Algebraic] { def convert(n: Long): Algebraic = Algebraic(n) }
  implicit val LongToReal = new Conversion[Long, Real] { def convert(n: Long): Real = Real(n) }

  // BigInt conversions
  implicit val BigIntToSafeLong = new Conversion[BigInt, SafeLong] { def convert(n: BigInt): SafeLong = SafeLong(n) }
  implicit val BigIntToBigDecimal = new Conversion[BigInt, BigDecimal] { def convert(n: BigInt): BigDecimal = BigDecimal(n) }
  implicit val BigIntToRational = new Conversion[BigInt, Rational] { def convert(n: BigInt): Rational = Rational(n) }
  implicit val BigIntToAlgebraic = new Conversion[BigInt, Algebraic] { def convert(n: BigInt): Algebraic = Algebraic(n) }
  implicit val BigIntToReal = new Conversion[BigInt, Real] { def convert(n: BigInt): Real = Real(n) }

  // SafeLong conversions
  implicit val SafeLongToBigInt = new Conversion[SafeLong, BigInt] { def convert(n: SafeLong): BigInt = n.toBigInt }
  implicit val SafeLongToBigDecimal = new Conversion[SafeLong, BigDecimal] { def convert(n: SafeLong): BigDecimal = BigDecimal(n.toBigInt) }
  implicit val SafeLongToRational = new Conversion[SafeLong, Rational] { def convert(n: SafeLong): Rational = Rational(n) }
  implicit val SafeLongToAlgebraic = new Conversion[SafeLong, Algebraic] { def convert(n: SafeLong): Algebraic = Algebraic(n) }
  implicit val SafeLongToReal = new Conversion[SafeLong, Real] { def convert(n: SafeLong): Real = Real(n) }

  // Float conversions
  implicit val FloatToDouble = new Conversion[Float, Double] { def convert(n: Float): Double = n.toDouble }
  implicit val FloatToBigDecimal = new Conversion[Float, BigDecimal] { def convert(n: Float): BigDecimal = BigDecimal(n) }
  implicit val FloatToRational = new Conversion[Float, Rational] { def convert(n: Float): Rational = Rational(n) }
  implicit val FloatToAlgebraic = new Conversion[Float, Algebraic] { def convert(n: Float): Algebraic = Algebraic(n) }
  implicit val FloatToReal = new Conversion[Float, Real] { def convert(n: Float): Real = Real(n) }

  // Double conversions
  implicit val DoubleToBigDecimal = new Conversion[Double, BigDecimal] { def convert(n: Double): BigDecimal = BigDecimal(n) }
  implicit val DoubleToRational = new Conversion[Double, Rational] { def convert(n: Double): Rational = Rational(n) }
  implicit val DoubleToAlgebraic = new Conversion[Double, Algebraic] { def convert(n: Double): Algebraic = Algebraic(n) }
  implicit val DoubleToReal = new Conversion[Double, Real] { def convert(n: Double): Real = Real(n) }

  // BigDecimal conversions
  implicit val BigDecimalToRational = new Conversion[BigDecimal, Rational] { def convert(n: BigDecimal): Rational = Rational(n) }
  implicit val BigDecimalToAlgebraic = new Conversion[BigDecimal, Algebraic] { def convert(n: BigDecimal): Algebraic = Algebraic(n) }
  implicit val BigDecimalToReal = new Conversion[BigDecimal, Real] { def convert(n: BigDecimal): Real = Real(n) }

  // Rational conversions
  implicit val RationalToAlgebraic = new Conversion[Rational, Algebraic] { def convert(n: Rational): Algebraic = Algebraic(n) }
  implicit val RationalToReal = new Conversion[Rational, Real] { def convert(n: Rational): Real = Real(n) }
}

object UnsafeConversion {
  def apply[@sp(Byte, Short, Int, Long, Float, Double) A, @sp(Byte, Short, Int, Long, Float, Double) B](f: A => B): UnsafeConversion[A, B] =
    new UnsafeConversion[A, B] { def convert(a: A): B = f(a) }

  // unsafe Int conversions
  implicit val IntToUByte = new UnsafeConversion[Int, UByte] { def convert(n: Int): UByte = UByte(n) }
  implicit val IntToUShort = new UnsafeConversion[Int, UShort] { def convert(n: Int): UShort = UShort(n) } 
  implicit val IntToUInt = new UnsafeConversion[Int, UInt] { def convert(n: Int): UInt = UInt(n) } 
  implicit val IntToULong = new UnsafeConversion[Int, ULong] { def convert(n: Int): ULong = ULong(n) }
  implicit val IntToNatural = new UnsafeConversion[Int, Natural] { def convert(n: Int): Natural = Natural(n) }
  implicit val IntToByte = new UnsafeConversion[Int, Byte] { def convert(n: Int): Byte = n.toByte }
  implicit val IntToShort = new UnsafeConversion[Int, Short] { def convert(n: Int): Short = n.toShort }
  implicit val IntToFloat = new UnsafeConversion[Int, Float] { def convert(n: Int): Float = n.toFloat }

  // unsafe Long conversions
  implicit val LongToUByte = new UnsafeConversion[Long, UByte] { def convert(n: Long): UByte = UByte(n.toInt) }
  implicit val LongToUShort = new UnsafeConversion[Long, UShort] { def convert(n: Long): UShort = UShort(n.toInt) } 
  implicit val LongToUInt = new UnsafeConversion[Long, UInt] { def convert(n: Long): UInt = UInt(n.toInt) }
  implicit val LongToULong = new UnsafeConversion[Long, ULong] { def convert(n: Long): ULong = ULong(n) }
  implicit val LongToNatural = new UnsafeConversion[Long, Natural] { def convert(n: Long): Natural = Natural(n) }
  implicit val LongToByte = new UnsafeConversion[Long, Byte] { def convert(n: Long): Byte = n.toByte }
  implicit val LongToShort = new UnsafeConversion[Long, Short] { def convert(n: Long): Short = n.toShort }
  implicit val LongToFloat = new UnsafeConversion[Long, Float] { def convert(n: Long): Float = n.toFloat }
  implicit val LongToDouble = new UnsafeConversion[Long, Double] { def convert(n: Long): Double = n.toDouble }
}
