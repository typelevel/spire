package spire
package algebra

/**
 * Field type class. While algebra already provides one, we provide one in Spire that integrates with the commutative
 * ring hierarchy, in particular `GCDRing` and `EuclideanRing`.
 *
 * On a field, all nonzero elements are invertible, so the remainder of the division is always 0. The Euclidean function
 * can take an arbitrary value on nonzero elements (it is undefined for zero); for compatibility with the degree of
 * polynomials, we use the constant 0.
 *
 * The GCD and LCM are defined up to a unit; on a field, it means that either the GCD or LCM can be fixed arbitrarily.
 * Some conventions with consistent defaults are provided in the spire.algebra.Field companion object.
 */
trait Field[@sp(Int, Long, Float, Double) A] extends Any with AlgebraField[A] with EuclideanRing[A] {
  def euclideanFunction(a: A): BigInt = BigInt(0)
  def equot(a: A, b: A): A = div(a, b)
  def emod(a: A, b: A): A = zero
  override def equotmod(a: A, b: A): (A, A) = (div(a, b), zero)
}

object Field extends _root_.algebra.ring.FieldFunctions[Field] with EuclideanRingFunctions[Field] {

  @inline def apply[A](implicit ev: Field[A]): Field[A] = ev

  /**
   * Field with simple default GCD/LCM implementations: gcd(a, b) = 1 (except gcd(0, 0) = 0) while lcm(a, b) = a * b.
   */
  trait WithDefaultGCD[@sp(Int, Long, Float, Double) A] extends Any with Field[A] {
    override def gcd(a: A, b: A)(implicit eqA: Eq[A]): A =
      if (isZero(a) && isZero(b)) zero else one
    override def lcm(a: A, b: A)(implicit eqA: Eq[A]): A = times(a, b)
  }

  /**
   * Field defined as a field of fractions with a default implementation of GCD/LCM such that
   *   - gcd(a/b, c/d) = gcd(a, c) / lcm(b, d)
   *   - lcm(a/b, c/d) = lcm(a, c) / gcd(b, d) which corresponds to the convention of the GCD domains of SageMath; on
   *     rational numbers, it "yields the unique extension of gcd from integers to rationals presuming the natural
   *     extension of the divisibility relation from integers to rationals", see http://math.stackexchange.com/a/151431
   */
  trait FieldOfFractionsGCD[A, R] extends Any with Field[A] {
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
