package spire
package algebra

import java.lang.Double.{ isInfinite, isNaN, doubleToLongBits }
import java.lang.Long.{ numberOfTrailingZeros }

trait Field[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with EuclideanRing[A] with MultiplicativeAbGroup[A] {
  /* On a field, all nonzero elements are invertible, so the remainder of the
   division is always 0. */
  def euclideanFunction(a: A) = BigInt(0)
  def emod(a: A, b: A): A = zero
  def equot(a: A, b: A): A = div(a, b)
  override def equotmod(a: A, b: A): (A, A) = (div(a, b), zero)

  /**
   * This is implemented in terms of basic Field ops. However, this is
   * probably significantly less efficient than can be done with a specific
   * type. So, it is recommended that this method is overriden.
   *
   * This is possible because a Double is a rational number.
   */
  def fromDouble(a: Double): A = if (a == 0.0) {
    fromInt(0)
  } else {
    require(!isInfinite(a) && !isNaN(a),
            "Double must be representable as a fraction.")

    val bits = doubleToLongBits(a)
    val m = bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L
    val zeros = numberOfTrailingZeros(m)
    val value = m >>> zeros
    val exp = ((bits >> 52) & 0x7FF).toInt - 1075 + zeros // 1023 + 52

    val high = times(fromInt((value >>> 30).toInt), fromInt(1 << 30))
    val low = fromInt((value & 0x3FFFFFFF).toInt)
    val num = plus(high, low)
    val unsigned = if (exp > 0) {
      times(num, pow(fromInt(2), exp))
    } else if (exp < 0) {
      div(num, pow(fromInt(2), -exp))
    } else {
      num
    }

    if (a < 0) negate(unsigned) else unsigned
  }

  override def gcd(a: A, b: A)(implicit ev: Eq[A]): A =
    if (isZero(a) && isZero(b)) zero else one

  override def lcm(a: A, b: A)(implicit ev: Eq[A]): A = times(a, b)
}

object FieldAsGCDRing {

  /** Default implementations of gcd/lcm for fields with the integers as a subring.
    * Inspired by the GCD domains of SAGE. 
    */
  trait WithZSubring[A] extends Any with Field[A] {
    def isWhole(a: A): Boolean
    def toBigInt(a: A): BigInt
    override def gcd(x: A, y: A)(implicit ev: Eq[A]): A =
      if (isWhole(x) && isWhole(y)) fromBigInt(toBigInt(x).gcd(toBigInt(y)))
      else if (isZero(x) && isZero(y)) zero
      else one
    override def lcm(x: A, y: A)(implicit ev: Eq[A]): A =
      if (isWhole(x) && isWhole(y)) {
        val bx = toBigInt(x)
        val by = toBigInt(y)
        fromBigInt(bx*by/bx.gcd(by))
      } else times(x, y)
  }

  /** Default implementations of gcd/lcm for fraction fields.
    * Inspired by the GCD domains of SAGE. 
    */
  trait AsFieldOfFractions[A, R] extends Any with Field[A] {
    implicit def ringR: GCDRing[R]
    implicit def eqR: Eq[R]
    def numerator(a: A): R
    def denominator(a: A): R
    def fraction(num: R, den: R): A
    override def gcd(x: A, y: A)(implicit ev: Eq[A]): A = {
      val num = ringR.gcd(numerator(x), numerator(y))
      val den = ringR.lcm(denominator(x), denominator(y))
      fraction(num, den)
    }
    override def lcm(x: A, y: A)(implicit ev: Eq[A]): A = {
      val num = ringR.lcm(numerator(x), numerator(y))
      val den = ringR.gcd(denominator(x), denominator(y))
      fraction(num, den)
    }
  }
}

object Field {
  @inline final def apply[A](implicit f: Field[A]): Field[A] = f
}
