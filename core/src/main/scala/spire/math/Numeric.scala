/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.algebra.{AdditiveAbGroup, IsReal, MultiplicativeAbGroup, NRoot, Ring}
import spire.std._

/**
 * TODO 3. LiteralOps? Literal conversions? 4. Review operator symbols? 5. Support for more operators? 6. Start to worry
 * about things like e.g. pow(BigInt, BigInt)
 */

trait Numeric[@sp(Int, Long, Float, Double) A]
    extends Any
    with Ring[A]
    with AdditiveAbGroup[A]
    with MultiplicativeAbGroup[A]
    with NRoot[A]
    with ConvertableFrom[A]
    with ConvertableTo[A]
    with IsReal[A]

object Numeric {

  implicit final val ByteIsNumeric: Numeric[Byte] = new ByteIsNumeric
  implicit final val ShortIsNumeric: Numeric[Short] = new ShortIsNumeric
  implicit final val IntIsNumeric: Numeric[Int] = new IntIsNumeric
  implicit final val LongIsNumeric: Numeric[Long] = new LongIsNumeric
  implicit final val FloatIsNumeric: Numeric[Float] = new FloatIsNumeric
  implicit final val DoubleIsNumeric: Numeric[Double] = new DoubleIsNumeric
  implicit final val BigIntIsNumeric: Numeric[BigInt] = new BigIntIsNumeric
  implicit final val BigDecimalIsNumeric: Numeric[BigDecimal] = new BigDecimalIsNumeric
  implicit final val AlgebraicIsNumeric: Numeric[Algebraic] = new AlgebraicIsNumeric
  implicit final val RealIsNumeric: Numeric[Real] = new RealIsNumeric
  implicit final val RationalIsNumeric: Numeric[Rational] = new RationalIsNumeric

  // TODO implicit def complexIsNumeric[A: Fractional: Trig: IsReal]: ComplexIsNumeric[A] = new ComplexIsNumeric

  @inline final def apply[A](implicit ev: Numeric[A]): Numeric[A] = ev
}

@SerialVersionUID(0L)
private[math] class ByteIsNumeric
    extends Numeric[Byte]
    with ByteIsEuclideanRing
    with ByteIsNRoot
    with ConvertableFromByte
    with ConvertableToByte
    with ByteIsReal
    with Serializable {
  override def fromInt(n: Int): Byte = n.toByte
  override def fromDouble(n: Double): Byte = n.toByte
  override def fromBigInt(n: BigInt): Byte = n.toByte
  override def toDouble(n: Byte): Double = n.toDouble
  override def toRational(n: Byte): Rational = super[ByteIsReal].toRational(n)
  override def toAlgebraic(n: Byte): Algebraic = super[ByteIsReal].toAlgebraic(n)
  override def toReal(n: Byte): Real = super[ByteIsReal].toReal(n)
  override def toBigInt(n: Byte): BigInt = super[ByteIsReal].toBigInt(n)
  def div(a: Byte, b: Byte): Byte = (a / b).toByte
}

@SerialVersionUID(0L)
private[math] class ShortIsNumeric
    extends Numeric[Short]
    with ShortIsEuclideanRing
    with ShortIsNRoot
    with ConvertableFromShort
    with ConvertableToShort
    with ShortIsReal
    with Serializable {
  override def fromInt(n: Int): Short = n.toShort
  override def fromDouble(n: Double): Short = n.toShort
  override def fromBigInt(n: BigInt): Short = n.toShort
  override def toDouble(n: Short): Double = n.toDouble
  override def toRational(n: Short): Rational = super[ShortIsReal].toRational(n)
  override def toAlgebraic(n: Short): Algebraic = super[ShortIsReal].toAlgebraic(n)
  override def toReal(n: Short): Real = super[ShortIsReal].toReal(n)
  override def toBigInt(n: Short): BigInt = super[ShortIsReal].toBigInt(n)
  def div(a: Short, b: Short): Short = (a / b).toShort
}

@SerialVersionUID(0L)
private[math] class IntIsNumeric
    extends Numeric[Int]
    with IntIsEuclideanRing
    with IntIsNRoot
    with ConvertableFromInt
    with ConvertableToInt
    with IntIsReal
    with Serializable {
  override def fromInt(n: Int): Int = n
  override def fromDouble(n: Double): Int = n.toInt
  override def fromBigInt(n: BigInt): Int = n.toInt
  override def toDouble(n: Int): Double = n.toDouble
  override def toRational(n: Int): Rational = super[IntIsReal].toRational(n)
  override def toAlgebraic(n: Int): Algebraic = super[IntIsReal].toAlgebraic(n)
  override def toReal(n: Int): Real = super[IntIsReal].toReal(n)
  override def toBigInt(n: Int): BigInt = super[IntIsReal].toBigInt(n)
  def div(a: Int, b: Int): Int = a / b
}

@SerialVersionUID(0L)
private[math] class LongIsNumeric
    extends Numeric[Long]
    with LongIsEuclideanRing
    with LongIsNRoot
    with ConvertableFromLong
    with ConvertableToLong
    with LongIsReal
    with Serializable {
  override def fromInt(n: Int): Long = n
  override def fromDouble(n: Double): Long = n.toLong
  override def fromBigInt(n: BigInt): Long = n.toLong
  override def toDouble(n: Long): Double = n.toDouble
  override def toRational(n: Long): Rational = super[LongIsReal].toRational(n)
  override def toAlgebraic(n: Long): Algebraic = super[LongIsReal].toAlgebraic(n)
  override def toReal(n: Long): Real = super[LongIsReal].toReal(n)
  override def toBigInt(n: Long): BigInt = super[LongIsReal].toBigInt(n)
  def div(a: Long, b: Long): Long = a / b
}

@SerialVersionUID(0L)
private[math] class BigIntIsNumeric
    extends Numeric[BigInt]
    with BigIntIsEuclideanRing
    with BigIntIsNRoot
    with ConvertableFromBigInt
    with ConvertableToBigInt
    with BigIntIsReal
    with Serializable {
  override def fromInt(n: Int): BigInt = BigInt(n)
  override def fromDouble(n: Double): BigInt = BigDecimal(n).toBigInt
  override def fromBigInt(n: BigInt): BigInt = n
  override def toDouble(n: BigInt): Double = n.toDouble
  override def toRational(n: BigInt): Rational = super[BigIntIsReal].toRational(n)
  override def toAlgebraic(n: BigInt): Algebraic = super[BigIntIsReal].toAlgebraic(n)
  override def toReal(n: BigInt): Real = super[BigIntIsReal].toReal(n)
  override def toBigInt(n: BigInt): BigInt = n
  def div(a: BigInt, b: BigInt): BigInt = a / b
}

@SerialVersionUID(0L)
private[math] class FloatIsNumeric
    extends Numeric[Float]
    with FloatIsField
    with FloatIsNRoot
    with ConvertableFromFloat
    with ConvertableToFloat
    with FloatIsReal
    with Serializable {
  override def fromInt(n: Int): Float = n.toFloat
  override def fromDouble(n: Double): Float = n.toFloat
  override def fromBigInt(n: BigInt): Float = n.toFloat
  override def toDouble(n: Float): Double = n.toDouble
  override def toRational(n: Float): Rational = super[FloatIsReal].toRational(n)
  override def toAlgebraic(n: Float): Algebraic = super[FloatIsReal].toAlgebraic(n)
  override def toReal(n: Float): Real = super[FloatIsReal].toReal(n)
}

@SerialVersionUID(0L)
private[math] class DoubleIsNumeric
    extends Numeric[Double]
    with DoubleIsField
    with DoubleIsNRoot
    with ConvertableFromDouble
    with ConvertableToDouble
    with DoubleIsReal
    with Serializable {
  override def fromInt(n: Int): Double = n.toDouble
  override def fromDouble(n: Double): Double = n
  override def fromBigInt(n: BigInt): Double = n.toDouble
  override def toDouble(n: Double): Double = n.toDouble
  override def toRational(n: Double): Rational = super[DoubleIsReal].toRational(n)
  override def toAlgebraic(n: Double): Algebraic = super[DoubleIsReal].toAlgebraic(n)
  override def toReal(n: Double): Real = super[DoubleIsReal].toReal(n)
}

@SerialVersionUID(0L)
private[math] class BigDecimalIsNumeric
    extends Numeric[BigDecimal]
    with BigDecimalIsField
    with BigDecimalIsNRoot
    with ConvertableFromBigDecimal
    with ConvertableToBigDecimal
    with BigDecimalIsReal
    with Serializable {
  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n)
  override def fromBigInt(n: BigInt): BigDecimal = BigDecimal(n)
  override def toDouble(n: BigDecimal): Double = n.toDouble
  override def toRational(n: BigDecimal): Rational = super[BigDecimalIsReal].toRational(n)
  override def toAlgebraic(n: BigDecimal): Algebraic = super[BigDecimalIsReal].toAlgebraic(n)
  override def toReal(n: BigDecimal): Real = super[BigDecimalIsReal].toReal(n)
}

@SerialVersionUID(0L)
private[math] class RationalIsNumeric
    extends Numeric[Rational]
    with RationalIsField
    with RationalApproximateNRoot
    with ConvertableFromRational
    with ConvertableToRational
    with RationalIsReal
    with Serializable {
  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
  override def fromBigInt(n: BigInt): Rational = Rational(n)
  override def toDouble(n: Rational): Double = n.toDouble
  override def toRational(n: Rational): Rational = n
  override def toAlgebraic(n: Rational): Algebraic = super[RationalIsReal].toAlgebraic(n)
  override def toReal(n: Rational): Real = super[RationalIsReal].toReal(n)
}

@SerialVersionUID(1L)
private[math] class AlgebraicIsNumeric
    extends Numeric[Algebraic]
    with AlgebraicIsField
    with AlgebraicIsNRoot
    with ConvertableFromAlgebraic
    with ConvertableToAlgebraic
    with AlgebraicIsReal
    with Serializable {
  override def fromInt(n: Int): Algebraic = Algebraic(n)
  override def fromDouble(n: Double): Algebraic = Algebraic(n)
  override def fromBigInt(n: BigInt): Algebraic = Algebraic(n)
  override def toDouble(n: Algebraic): Double = n.toDouble
  override def toAlgebraic(n: Algebraic): Algebraic = n
  override def toReal(n: Algebraic): Real = super[AlgebraicIsReal].toReal(n)
}

@SerialVersionUID(0L)
private[math] class RealIsNumeric extends Numeric[Real] with RealIsFractional with Serializable {
  override def fromInt(n: Int): Real = Real(n)
  override def fromDouble(n: Double): Real = Real(n)
  override def fromBigInt(n: BigInt): Real = Real(n)
  override def toDouble(n: Real): Double = n.toDouble
}
