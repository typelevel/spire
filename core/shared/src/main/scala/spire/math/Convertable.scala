package spire
package math

import java.math.MathContext

trait ConvertableTo[@sp A] extends Any {
  def fromByte(n: Byte): A
  def fromShort(n: Short): A
  def fromInt(n: Int): A
  def fromLong(n: Long): A
  def fromFloat(n: Float): A
  def fromDouble(n: Double): A
  def fromBigInt(n: BigInt): A
  def fromBigDecimal(n: BigDecimal): A
  def fromRational(n: Rational): A
  def fromAlgebraic(n: Algebraic): A
  def fromReal(n: Real): A

  def fromType[B: ConvertableFrom](b: B): A
}

private[math] trait ConvertableToByte extends ConvertableTo[Byte] {
  def fromByte(a: Byte): Byte = a
  def fromShort(a: Short): Byte = a.toByte
  def fromInt(a: Int): Byte = a.toByte
  def fromLong(a: Long): Byte = a.toByte
  def fromFloat(a: Float): Byte = a.toByte
  def fromDouble(a: Double): Byte = a.toByte
  def fromBigInt(a: BigInt): Byte = a.toByte
  def fromBigDecimal(a: BigDecimal): Byte = a.toByte
  def fromRational(a: Rational): Byte = a.toBigInt.toByte
  def fromAlgebraic(a: Algebraic): Byte = a.toByte
  def fromReal(a: Real): Byte = a.toByte

  def fromType[B: ConvertableFrom](b: B): Byte = ConvertableFrom[B].toByte(b)
}

private[math] trait ConvertableToShort extends ConvertableTo[Short] {
  def fromByte(a: Byte): Short = a.toShort
  def fromShort(a: Short): Short = a
  def fromInt(a: Int): Short = a.toShort
  def fromLong(a: Long): Short = a.toShort
  def fromFloat(a: Float): Short = a.toShort
  def fromDouble(a: Double): Short = a.toShort
  def fromBigInt(a: BigInt): Short = a.toShort
  def fromBigDecimal(a: BigDecimal): Short = a.toShort
  def fromRational(a: Rational): Short = a.toBigInt.toShort
  def fromAlgebraic(a: Algebraic): Short = a.toShort
  def fromReal(a: Real): Short = a.toShort

  def fromType[B: ConvertableFrom](b: B): Short = ConvertableFrom[B].toShort(b)
}

private[math] trait ConvertableToInt extends ConvertableTo[Int] {
  def fromByte(a: Byte): Int = a.toInt
  def fromShort(a: Short): Int = a.toInt
  def fromInt(a: Int): Int = a
  def fromLong(a: Long): Int = a.toInt
  def fromFloat(a: Float): Int = a.toInt
  def fromDouble(a: Double): Int = a.toInt
  def fromBigInt(a: BigInt): Int = a.toInt
  def fromBigDecimal(a: BigDecimal): Int = a.toInt
  def fromRational(a: Rational): Int = a.toBigInt.toInt
  def fromAlgebraic(a: Algebraic): Int = a.toInt
  def fromReal(a: Real): Int = a.toInt

  def fromType[B: ConvertableFrom](b: B): Int = ConvertableFrom[B].toInt(b)
}

private[math] trait ConvertableToLong extends ConvertableTo[Long] {
  def fromByte(a: Byte): Long = a.toLong
  def fromShort(a: Short): Long = a.toLong
  def fromInt(a: Int): Long = a.toLong
  def fromLong(a: Long): Long = a
  def fromFloat(a: Float): Long = a.toLong
  def fromDouble(a: Double): Long = a.toLong
  def fromBigInt(a: BigInt): Long = a.toLong
  def fromBigDecimal(a: BigDecimal): Long = a.toLong
  def fromRational(a: Rational): Long = a.toBigInt.toLong
  def fromAlgebraic(a: Algebraic): Long = a.toLong
  def fromReal(a: Real): Long = a.toLong

  def fromType[B: ConvertableFrom](b: B): Long = ConvertableFrom[B].toLong(b)
}

private[math] trait ConvertableToFloat extends ConvertableTo[Float] {
  def fromByte(a: Byte): Float = a.toFloat
  def fromShort(a: Short): Float = a.toFloat
  def fromInt(a: Int): Float = a.toFloat
  def fromLong(a: Long): Float = a.toFloat
  def fromFloat(a: Float): Float = a
  def fromDouble(a: Double): Float = a.toFloat
  def fromBigInt(a: BigInt): Float = a.toFloat
  def fromBigDecimal(a: BigDecimal): Float = a.toFloat
  def fromRational(a: Rational): Float = a.toBigDecimal(MathContext.DECIMAL64).toFloat
  def fromAlgebraic(a: Algebraic): Float = a.toFloat
  def fromReal(a: Real): Float = a.toFloat

  def fromType[B: ConvertableFrom](b: B): Float = ConvertableFrom[B].toFloat(b)
}

private[math] trait ConvertableToDouble extends ConvertableTo[Double] {
  def fromByte(a: Byte): Double = a.toDouble
  def fromShort(a: Short): Double = a.toDouble
  def fromInt(a: Int): Double = a.toDouble
  def fromLong(a: Long): Double = a.toDouble
  def fromFloat(a: Float): Double = a.toDouble
  def fromDouble(a: Double): Double = a
  def fromBigInt(a: BigInt): Double = a.toDouble
  def fromBigDecimal(a: BigDecimal): Double = a.toDouble
  def fromRational(a: Rational): Double = a.toBigDecimal(MathContext.DECIMAL64).toDouble
  def fromAlgebraic(a: Algebraic): Double = a.toDouble
  def fromReal(a: Real): Double = a.toDouble

  def fromType[B: ConvertableFrom](b: B): Double = ConvertableFrom[B].toDouble(b)
}

private[math] trait ConvertableToBigInt extends ConvertableTo[BigInt] {
  def fromByte(a: Byte): BigInt = BigInt(a)
  def fromShort(a: Short): BigInt = BigInt(a)
  def fromInt(a: Int): BigInt = BigInt(a)
  def fromLong(a: Long): BigInt = BigInt(a)
  def fromFloat(a: Float): BigInt = BigInt(a.toLong)
  def fromDouble(a: Double): BigInt = BigInt(a.toLong)
  def fromBigInt(a: BigInt): BigInt = a
  def fromBigDecimal(a: BigDecimal): BigInt = a.toBigInt
  def fromRational(a: Rational): BigInt = a.toBigInt
  def fromAlgebraic(a: Algebraic): BigInt = a.toBigInt
  def fromReal(a: Real): BigInt = fromRational(a.toRational)

  def fromType[B: ConvertableFrom](b: B): BigInt = ConvertableFrom[B].toBigInt(b)
}

private[math] trait ConvertableToBigDecimal extends ConvertableTo[BigDecimal] {
  def fromByte(a: Byte): BigDecimal = BigDecimal(a)
  def fromShort(a: Short): BigDecimal = BigDecimal(a)
  def fromInt(a: Int): BigDecimal = BigDecimal(a)
  def fromLong(a: Long): BigDecimal = BigDecimal(a)
  def fromFloat(a: Float): BigDecimal = BigDecimal(a.toDouble)
  def fromDouble(a: Double): BigDecimal = BigDecimal(a)
  def fromBigInt(a: BigInt): BigDecimal = BigDecimal(a)
  def fromBigDecimal(a: BigDecimal): BigDecimal = a
  def fromRational(a: Rational): BigDecimal = a.toBigDecimal(MathContext.DECIMAL64)
  def fromAlgebraic(a: Algebraic): BigDecimal = a.toBigDecimal(MathContext.DECIMAL64)
  def fromReal(a: Real): BigDecimal = fromRational(a.toRational)

  def fromType[B: ConvertableFrom](b: B): BigDecimal = ConvertableFrom[B].toBigDecimal(b)
}

private[math] trait ConvertableToRational extends ConvertableTo[Rational] {
  def fromByte(a: Byte): Rational = Rational(a)
  def fromShort(a: Short): Rational = Rational(a)
  def fromInt(a: Int): Rational = Rational(a)
  def fromLong(a: Long): Rational = Rational(a)
  def fromFloat(a: Float): Rational = Rational(a)
  def fromDouble(a: Double): Rational = Rational(a)
  def fromBigInt(a: BigInt): Rational = Rational(a)
  def fromBigDecimal(a: BigDecimal): Rational = Rational(a)
  def fromRational(a: Rational): Rational = a
  def fromAlgebraic(a: Algebraic): Rational =
    a.toRational.getOrElse(Rational(a.toBigDecimal(MathContext.DECIMAL64)))
  def fromReal(a: Real): Rational = a.toRational

  def fromType[B: ConvertableFrom](b: B): Rational = ConvertableFrom[B].toRational(b)
}

private[math] trait ConvertableToAlgebraic extends ConvertableTo[Algebraic] {
  def fromByte(a: Byte): Algebraic = Algebraic(a)
  def fromShort(a: Short): Algebraic = Algebraic(a)
  def fromInt(a: Int): Algebraic = Algebraic(a)
  def fromLong(a: Long): Algebraic = Algebraic(a)
  def fromFloat(a: Float): Algebraic = Algebraic(a)
  def fromDouble(a: Double): Algebraic = Algebraic(a)
  def fromBigInt(a: BigInt): Algebraic = Algebraic(a)
  def fromBigDecimal(a: BigDecimal): Algebraic = Algebraic(a)
  def fromRational(a: Rational): Algebraic = Algebraic(a)
  def fromAlgebraic(a: Algebraic): Algebraic = a
  def fromReal(a: Real): Algebraic = Algebraic(a.toRational) //FIXME

  def fromType[B: ConvertableFrom](b: B): Algebraic = ConvertableFrom[B].toAlgebraic(b)
}

private[math] trait ConvertableToComplex[A] extends ConvertableTo[Complex[A]] {
  implicit def algebra: Integral[A]

  def fromByte(a: Byte): Complex[A] = Complex(algebra.fromByte(a), algebra.zero)
  def fromShort(a: Short): Complex[A] = Complex(algebra.fromShort(a), algebra.zero)
  def fromInt(a: Int): Complex[A] = Complex(algebra.fromInt(a), algebra.zero)
  def fromLong(a: Long): Complex[A] = Complex(algebra.fromLong(a), algebra.zero)
  def fromFloat(a: Float): Complex[A] = Complex(algebra.fromFloat(a), algebra.zero)
  def fromDouble(a: Double): Complex[A] = Complex(algebra.fromDouble(a), algebra.zero)
  def fromBigInt(a: BigInt): Complex[A] = Complex(algebra.fromBigInt(a), algebra.zero)
  def fromBigDecimal(a: BigDecimal): Complex[A] = Complex(algebra.fromBigDecimal(a), algebra.zero)
  def fromRational(a: Rational): Complex[A] = Complex(algebra.fromRational(a), algebra.zero)
  def fromAlgebraic(a: Algebraic): Complex[A] = Complex(algebra.fromAlgebraic(a), algebra.zero)
  def fromReal(a: Real): Complex[A] = Complex(algebra.fromReal(a), algebra.zero)

  def fromType[B: ConvertableFrom](b: B): Complex[A] = Complex(algebra.fromType(b), algebra.zero)
}

private[math] trait ConvertableToSafeLong extends ConvertableTo[SafeLong] {
  def fromByte(a: Byte): SafeLong = SafeLong(a)
  def fromShort(a: Short): SafeLong = SafeLong(a)
  def fromInt(a: Int): SafeLong = SafeLong(a)
  def fromLong(a: Long): SafeLong = SafeLong(a)
  def fromFloat(a: Float): SafeLong = SafeLong(a.toLong)
  def fromDouble(a: Double): SafeLong = SafeLong(a.toLong)
  def fromBigInt(a: BigInt): SafeLong = SafeLong(a)
  def fromBigDecimal(a: BigDecimal): SafeLong = SafeLong(a.toBigInt)
  def fromRational(a: Rational): SafeLong = if (a.isValidInt) SafeLong(a.toInt) else SafeLong(a.toBigInt)
  def fromAlgebraic(a: Algebraic): SafeLong = if (a.isValidInt) SafeLong(a.toInt) else SafeLong(a.toBigInt)
  def fromReal(a: Real): SafeLong = if (a.isValidInt) SafeLong(a.toInt) else fromRational(a.toRational)

  def fromType[B: ConvertableFrom](b: B): SafeLong = SafeLong(ConvertableFrom[B].toBigInt(b))
}

private[math] trait ConvertableToNumber extends ConvertableTo[Number] {
  def fromByte(a: Byte): Number = Number(a)
  def fromShort(a: Short): Number = Number(a)
  def fromInt(a: Int): Number = Number(a)
  def fromLong(a: Long): Number = Number(a)
  def fromFloat(a: Float): Number = Number(a)
  def fromDouble(a: Double): Number = Number(a)
  def fromBigInt(a: BigInt): Number = Number(a)
  def fromBigDecimal(a: BigDecimal): Number = Number(a)
  def fromRational(a: Rational): Number = Number(a)
  def fromAlgebraic(a: Algebraic): Number =
    Number(a.toRational.getOrElse(Rational(a.toBigDecimal(MathContext.DECIMAL64))))
  def fromReal(a: Real): Number = Number(a.toRational)

  def fromType[B: ConvertableFrom](b: B): Number = Number(ConvertableFrom[B].toDouble(b))
}

private[math] trait ConvertableToNatural extends ConvertableTo[Natural] {
  def fromByte(a: Byte): Natural = Natural(a)
  def fromShort(a: Short): Natural = Natural(a)
  def fromInt(a: Int): Natural = Natural(a)
  def fromLong(a: Long): Natural = Natural(a)
  def fromFloat(a: Float): Natural = fromRational(Rational(a))
  def fromDouble(a: Double): Natural = fromRational(Rational(a))
  def fromBigInt(a: BigInt): Natural = Natural(a)
  def fromBigDecimal(a: BigDecimal): Natural = Natural(a.toBigInt)
  def fromRational(a: Rational): Natural = if (a.isValidInt) Natural(a.toInt) else Natural(a.toBigInt)
  def fromAlgebraic(a: Algebraic): Natural = if (a.isValidInt) Natural(a.toInt) else Natural(a.toBigInt)
  def fromReal(a: Real): Natural = if (a.isValidInt) Natural(a.toInt) else fromRational(a.toRational)

  def fromType[B: ConvertableFrom](b: B): Natural = Natural(ConvertableFrom[B].toBigInt(b))
}

object ConvertableTo {
  @inline final def apply[A](implicit ev: ConvertableTo[A]): ConvertableTo[A] = ev

  implicit final val ConvertableToByte = new ConvertableToByte {}
  implicit final val ConvertableToShort = new ConvertableToShort {}
  implicit final val ConvertableToInt = new ConvertableToInt {}
  implicit final val ConvertableToLong = new ConvertableToLong {}
  implicit final val ConvertableToBigInt = new ConvertableToBigInt {}
  implicit final val ConvertableToFloat = new ConvertableToFloat {}
  implicit final val ConvertableToDouble = new ConvertableToDouble {}
  implicit final val ConvertableToBigDecimal = new ConvertableToBigDecimal {}
  implicit final val ConvertableToRational = new ConvertableToRational {}
  implicit final val ConvertableToAlgebraic = new ConvertableToAlgebraic {}
  implicit final val ConvertableToSafeLong = new ConvertableToSafeLong {}
  implicit final val ConvertableToNumber = new ConvertableToNumber {}
  implicit final val ConvertableToNatural = new ConvertableToNatural {}

  implicit def convertableToComplex[A: Integral]: ConvertableToComplex[A] =
    new ConvertableToComplex[A] { val algebra = Integral[A] }
}

trait ConvertableFrom[@sp A] extends Any {
  def toByte(a: A): Byte
  def toShort(a: A): Short
  def toInt(a: A): Int
  def toLong(a: A): Long
  def toFloat(a: A): Float
  def toDouble(a: A): Double
  def toBigInt(a: A): BigInt
  def toBigDecimal(a: A): BigDecimal
  def toRational(a: A): Rational
  def toAlgebraic(a: A): Algebraic
  def toReal(a: A): Real
  def toNumber(a: A): Number

  def toType[B: ConvertableTo](a: A): B
  def toString(a: A): String
}

private[math] trait ConvertableFromByte extends ConvertableFrom[Byte] {
  def toByte(a: Byte): Byte = a
  def toShort(a: Byte): Short = a.toShort
  def toInt(a: Byte): Int = a.toInt
  def toLong(a: Byte): Long = a.toLong
  def toFloat(a: Byte): Float = a.toFloat
  def toDouble(a: Byte): Double = a.toDouble
  def toBigInt(a: Byte): BigInt = BigInt(a)
  def toBigDecimal(a: Byte): BigDecimal = BigDecimal(a)
  def toRational(a: Byte): Rational = Rational(a)
  def toAlgebraic(a: Byte): Algebraic = Algebraic(a)
  def toReal(a: Byte): Real = Real(a)
  def toNumber(a: Byte): Number = Number(a)

  def toType[B: ConvertableTo](a: Byte): B = ConvertableTo[B].fromByte(a)
  def toString(a: Byte): String = a.toString
}

private[math] trait ConvertableFromShort extends ConvertableFrom[Short] {
  def toByte(a: Short): Byte = a.toByte
  def toShort(a: Short): Short = a
  def toInt(a: Short): Int = a.toInt
  def toLong(a: Short): Long = a.toLong
  def toFloat(a: Short): Float = a.toFloat
  def toDouble(a: Short): Double = a.toDouble
  def toBigInt(a: Short): BigInt = BigInt(a)
  def toBigDecimal(a: Short): BigDecimal = BigDecimal(a)
  def toRational(a: Short): Rational = Rational(a)
  def toAlgebraic(a: Short): Algebraic = Algebraic(a)
  def toReal(a: Short): Real = Real(a)
  def toNumber(a: Short): Number = Number(a)

  def toType[B: ConvertableTo](a: Short): B = ConvertableTo[B].fromShort(a)
  def toString(a: Short): String = a.toString
}

private[math] trait ConvertableFromInt extends ConvertableFrom[Int] {
  def toByte(a: Int): Byte = a.toByte
  def toShort(a: Int): Short = a.toShort
  def toInt(a: Int): Int = a
  def toLong(a: Int): Long = a.toLong
  def toFloat(a: Int): Float = a.toFloat
  def toDouble(a: Int): Double = a.toDouble
  def toBigInt(a: Int): BigInt = BigInt(a)
  def toBigDecimal(a: Int): BigDecimal = BigDecimal(a)
  def toRational(a: Int): Rational = Rational(a)
  def toAlgebraic(a: Int): Algebraic = Algebraic(a)
  def toReal(a: Int): Real = Real(a)
  def toNumber(a: Int): Number = Number(a)

  def toType[B: ConvertableTo](a: Int): B = ConvertableTo[B].fromInt(a)
  def toString(a: Int): String = a.toString
}

private[math] trait ConvertableFromLong extends ConvertableFrom[Long] {
  def toByte(a: Long): Byte = a.toByte
  def toShort(a: Long): Short = a.toShort
  def toInt(a: Long): Int = a.toInt
  def toLong(a: Long): Long = a
  def toFloat(a: Long): Float = a.toFloat
  def toDouble(a: Long): Double = a.toDouble
  def toBigInt(a: Long): BigInt = BigInt(a)
  def toBigDecimal(a: Long): BigDecimal = BigDecimal(a)
  def toRational(a: Long): Rational = Rational(a)
  def toAlgebraic(a: Long): Algebraic = Algebraic(a)
  def toReal(a: Long): Real = Real(a)
  def toNumber(a: Long): Number = Number(a)

  def toType[B: ConvertableTo](a: Long): B = ConvertableTo[B].fromLong(a)
  def toString(a: Long): String = a.toString
}

private[math] trait ConvertableFromFloat extends ConvertableFrom[Float] {
  def toByte(a: Float): Byte = a.toByte
  def toShort(a: Float): Short = a.toShort
  def toInt(a: Float): Int = a.toInt
  def toLong(a: Float): Long = a.toLong
  def toFloat(a: Float): Float = a
  def toDouble(a: Float): Double = a.toDouble
  def toBigInt(a: Float): BigInt = BigInt(a.toLong)
  def toBigDecimal(a: Float): BigDecimal = BigDecimal(a.toDouble)
  def toRational(a: Float): Rational = Rational(a)
  def toAlgebraic(a: Float): Algebraic = Algebraic(a)
  def toReal(a: Float): Real = Real(a)
  def toNumber(a: Float): Number = Number(a)

  def toType[B: ConvertableTo](a: Float): B = ConvertableTo[B].fromFloat(a)
  def toString(a: Float): String = a.toString
}

private[math] trait ConvertableFromDouble extends ConvertableFrom[Double] {
  def toByte(a: Double): Byte = a.toByte
  def toShort(a: Double): Short = a.toShort
  def toInt(a: Double): Int = a.toInt
  def toLong(a: Double): Long = a.toLong
  def toFloat(a: Double): Float = a.toFloat
  def toDouble(a: Double): Double = a
  def toBigInt(a: Double): BigInt = BigInt(a.toLong)
  def toBigDecimal(a: Double): BigDecimal = BigDecimal(a)
  def toRational(a: Double): Rational = Rational(a)
  def toAlgebraic(a: Double): Algebraic = Algebraic(a)
  def toReal(a: Double): Real = Real(a)
  def toNumber(a: Double): Number = Number(a)

  def toType[B: ConvertableTo](a: Double): B = ConvertableTo[B].fromDouble(a)
  def toString(a: Double): String = a.toString
}

private[math] trait ConvertableFromBigInt extends ConvertableFrom[BigInt] {
  def toByte(a: BigInt): Byte = a.toByte
  def toShort(a: BigInt): Short = a.toShort
  def toInt(a: BigInt): Int = a.toInt
  def toLong(a: BigInt): Long = a.toLong
  def toFloat(a: BigInt): Float = a.toFloat
  def toDouble(a: BigInt): Double = a.toDouble
  def toBigInt(a: BigInt): BigInt = a
  def toBigDecimal(a: BigInt): BigDecimal = BigDecimal(a)
  def toRational(a: BigInt): Rational = Rational(a)
  def toAlgebraic(a: BigInt): Algebraic = Algebraic(a)
  def toReal(a: BigInt): Real = Real(a)
  def toNumber(a: BigInt): Number = Number(a)

  def toType[B: ConvertableTo](a: BigInt): B = ConvertableTo[B].fromBigInt(a)
  def toString(a: BigInt): String = a.toString
}

private[math] trait ConvertableFromBigDecimal extends ConvertableFrom[BigDecimal] {
  def toByte(a: BigDecimal): Byte = a.toByte
  def toShort(a: BigDecimal): Short = a.toShort
  def toInt(a: BigDecimal): Int = a.toInt
  def toLong(a: BigDecimal): Long = a.toLong
  def toFloat(a: BigDecimal): Float = a.toFloat
  def toDouble(a: BigDecimal): Double = a.toDouble
  def toBigInt(a: BigDecimal): BigInt = a.toBigInt
  def toBigDecimal(a: BigDecimal): BigDecimal = a
  def toRational(a: BigDecimal): Rational = Rational(a)
  def toAlgebraic(a: BigDecimal): Algebraic = Algebraic(a)
  def toReal(a: BigDecimal): Real = Real(a)
  def toNumber(a: BigDecimal): Number = Number(a)

  def toType[B: ConvertableTo](a: BigDecimal): B = ConvertableTo[B].fromBigDecimal(a)
  def toString(a: BigDecimal): String = a.toString
}

private[math] trait ConvertableFromRational extends ConvertableFrom[Rational] {
  def toByte(a: Rational): Byte = a.toBigInt.toByte
  def toShort(a: Rational): Short = a.toBigInt.toShort
  def toInt(a: Rational): Int = a.toBigInt.toInt
  def toLong(a: Rational): Long = a.toBigInt.toLong
  def toFloat(a: Rational): Float = a.toBigDecimal(MathContext.DECIMAL64).toFloat
  def toDouble(a: Rational): Double = a.toBigDecimal(MathContext.DECIMAL64).toDouble
  def toBigInt(a: Rational): BigInt = a.toBigInt
  def toBigDecimal(a: Rational): BigDecimal = a.toBigDecimal(MathContext.DECIMAL64)
  def toRational(a: Rational): Rational = a
  def toAlgebraic(a: Rational): Algebraic = Algebraic(a)
  def toReal(a: Rational): Real = Real(a)
  def toNumber(a: Rational): Number = Number(a)

  def toType[B: ConvertableTo](a: Rational): B = ConvertableTo[B].fromRational(a)
  def toString(a: Rational): String = a.toString
}

private[math] trait ConvertableFromAlgebraic extends ConvertableFrom[Algebraic] {
  def toByte(a: Algebraic): Byte = a.toInt.toByte
  def toShort(a: Algebraic): Short = a.toInt.toShort
  def toInt(a: Algebraic): Int = a.toInt
  def toLong(a: Algebraic): Long = a.toLong
  def toFloat(a: Algebraic): Float = a.toDouble.toFloat
  def toDouble(a: Algebraic): Double = a.toDouble
  def toBigInt(a: Algebraic): BigInt = a.toBigInt
  // TODO: Figure out how to deal with variable approximability.
  def toBigDecimal(a: Algebraic): BigDecimal =
    a.toBigDecimal(java.math.MathContext.DECIMAL128)
  def toRational(a: Algebraic): Rational =
    a.toRational.getOrElse(Rational(a.toBigDecimal(MathContext.DECIMAL64)))
  def toAlgebraic(a: Algebraic): Algebraic = a
  def toReal(a: Algebraic): Real = a.evaluateWith[Real]
  def toNumber(a: Algebraic): Number =
    a.toRational.map(Number(_)).getOrElse(a.evaluateWith[Number])

  def toType[B: ConvertableTo](a: Algebraic): B = ConvertableTo[B].fromAlgebraic(a)
  def toString(a: Algebraic): String = a.toString
}

private[math] trait ConvertableFromComplex[A] extends ConvertableFrom[Complex[A]] {
  def algebra: Integral[A]

  def toByte(a: Complex[A]): Byte = algebra.toByte(a.real)
  def toShort(a: Complex[A]): Short = algebra.toShort(a.real)
  def toInt(a: Complex[A]): Int = algebra.toInt(a.real)
  def toLong(a: Complex[A]): Long = algebra.toLong(a.real)
  def toFloat(a: Complex[A]): Float = algebra.toFloat(a.real)
  def toDouble(a: Complex[A]): Double = algebra.toDouble(a.real)
  def toBigInt(a: Complex[A]): BigInt = algebra.toBigInt(a.real)
  def toBigDecimal(a: Complex[A]): BigDecimal = algebra.toBigDecimal(a.real)
  def toRational(a: Complex[A]): Rational = algebra.toRational(a.real)
  def toAlgebraic(a: Complex[A]): Algebraic = algebra.toAlgebraic(a.real)
  def toReal(a: Complex[A]): Real = algebra.toReal(a.real)
  def toNumber(a: Complex[A]): Number = algebra.toNumber(a.real)

  def toType[B: ConvertableTo](a: Complex[A]): B = sys.error("fixme")
  def toString(a: Complex[A]): String = a.toString
}

private[math] trait ConvertableFromSafeLong extends ConvertableFrom[SafeLong] {
  def toByte(a: SafeLong): Byte = a.toBigInt.toByte
  def toShort(a: SafeLong): Short = a.toBigInt.toShort
  def toInt(a: SafeLong): Int = a.toBigInt.toInt
  def toLong(a: SafeLong): Long = a.toBigInt.toLong
  def toFloat(a: SafeLong): Float = a.toBigInt.toFloat
  def toDouble(a: SafeLong): Double = a.toBigInt.toDouble
  def toBigInt(a: SafeLong): BigInt = a.toBigInt
  def toBigDecimal(a: SafeLong): BigDecimal = BigDecimal(a.toBigInt)
  def toRational(a: SafeLong): Rational = Rational(a.toBigInt)
  def toAlgebraic(a: SafeLong): Algebraic = Algebraic(a)
  def toReal(a: SafeLong): Real = Real(a)
  def toNumber(a: SafeLong): Number = Number(a)

  def toType[B: ConvertableTo](a: SafeLong): B = ConvertableTo[B].fromBigInt(a.toBigInt)
  def toString(a: SafeLong): String = a.toString
}

private[math] trait ConvertableFromNumber extends ConvertableFrom[Number] {
  def toByte(a: Number): Byte = a.toBigInt.toByte
  def toShort(a: Number): Short = a.toBigInt.toShort
  def toInt(a: Number): Int = a.toBigInt.toInt
  def toLong(a: Number): Long = a.toBigInt.toLong
  def toFloat(a: Number): Float = a.toBigInt.toFloat
  def toDouble(a: Number): Double = a.toBigInt.toDouble
  def toBigInt(a: Number): BigInt = a.toBigInt
  def toBigDecimal(a: Number): BigDecimal = BigDecimal(a.toBigInt)
  def toRational(a: Number): Rational = Rational(a.toBigInt)
  def toAlgebraic(a: Number): Algebraic = Algebraic(a.toRational)
  def toReal(a: Number): Real = Real(a.toRational)
  def toNumber(a: Number): Number = a

  def toType[B: ConvertableTo](a: Number): B = ConvertableTo[B].fromBigInt(a.toBigInt)
  def toString(a: Number): String = a.toString
}

private[math] trait ConvertableFromNatural extends ConvertableFrom[Natural] {
  def toByte(a: Natural): Byte = a.toBigInt.toByte
  def toShort(a: Natural): Short = a.toBigInt.toShort
  def toInt(a: Natural): Int = a.toBigInt.toInt
  def toLong(a: Natural): Long = a.toBigInt.toLong
  def toFloat(a: Natural): Float = a.toBigInt.toFloat
  def toDouble(a: Natural): Double = a.toBigInt.toDouble
  def toBigInt(a: Natural): BigInt = a.toBigInt
  def toBigDecimal(a: Natural): BigDecimal = BigDecimal(a.toBigInt)
  def toRational(a: Natural): Rational = Rational(a.toBigInt)
  def toAlgebraic(a: Natural): Algebraic = Algebraic(a.toBigInt)
  def toReal(a: Natural): Real = Real(a.toBigInt)
  def toNumber(a: Natural): Number = Number(a.toBigInt)

  def toType[B: ConvertableTo](a: Natural): B = ConvertableTo[B].fromBigInt(a.toBigInt)
  def toString(a: Natural): String = a.toString
}


object ConvertableFrom {
  @inline final def apply[A](implicit ev: ConvertableFrom[A]): ConvertableFrom[A] = ev

  implicit final val ConvertableFromByte = new ConvertableFromByte {}
  implicit final val ConvertableFromShort = new ConvertableFromShort {}
  implicit final val ConvertableFromInt = new ConvertableFromInt {}
  implicit final val ConvertableFromLong = new ConvertableFromLong {}
  implicit final val ConvertableFromFloat = new ConvertableFromFloat {}
  implicit final val ConvertableFromDouble = new ConvertableFromDouble {}
  implicit final val ConvertableFromBigInt = new ConvertableFromBigInt {}
  implicit final val ConvertableFromBigDecimal = new ConvertableFromBigDecimal {}
  implicit final val ConvertableFromRational = new ConvertableFromRational {}
  implicit final val ConvertableFromAlgebraic = new ConvertableFromAlgebraic {}
  implicit final val ConvertableFromSafeLong = new ConvertableFromSafeLong {}
  implicit final val ConvertableFromNumber = new ConvertableFromNumber {}
  implicit final val ConvertableFromNatural = new ConvertableFromNatural {}

  implicit def convertableFromComplex[A: Integral]: ConvertableFromComplex[A] =
    new ConvertableFromComplex[A] { val algebra = Integral[A] }
}
