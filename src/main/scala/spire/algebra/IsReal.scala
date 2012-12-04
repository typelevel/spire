package spire.algebra

import spire.math._

import scala.{ specialized => spec }

/**
 * A simple class for numeric types that are a subset of the reals.
 */
trait IsReal[@spec A] {
  def toDouble(a: A): Double
}

object IsReal {
  def apply[@spec A](implicit A: IsReal[A]): IsReal[A] = A

  private def apply[@spec A](f: A => Double) = new IsReal[A] {
    def toDouble(a: A) = f(a)
  }

  val ByteIsReal = IsReal[Byte] { x: Byte => x.toDouble }
  val ShortIsReal = IsReal[Short] { x: Short => x.toDouble }
  val IntIsReal = IsReal[Int] { x: Int => x.toDouble }
  val LongIsReal = IsReal[Long] { x: Long => x.toDouble }
  val BigIntIsReal = IsReal[BigInt] { x: BigInt => x.toDouble }
  val UByteIsReal = IsReal[UByte] { x: UByte => x.toDouble }
  val UShortIsReal = IsReal[UShort] { x: UShort => x.toDouble }
  val UIntIsReal = IsReal[UInt] { x: UInt => x.toDouble }
  val ULongIsReal = IsReal[ULong] { x: ULong => x.toDouble }
  val FloatIsReal = IsReal[Float] { x: Float => x.toDouble }
  val DoubleIsReal = IsReal[Double] { x: Double => x.toDouble }
  val RationalIsReal = IsReal[Rational] { x: Rational => x.toDouble }
  val RealIsReal = IsReal[Real] { x: Real => x.toDouble }
  val BigDecimalIsReal = IsReal[BigDecimal] { x: BigDecimal => x.toDouble }
}

