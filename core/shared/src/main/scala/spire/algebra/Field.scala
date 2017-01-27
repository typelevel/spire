package spire
package algebra

trait Field[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AlgebraField[A] with EuclideanRing[A] {
  /* On a field, all nonzero elements are invertible, so the remainder of the
   division is always 0. */
  def euclideanFunction(a: A) = BigInt(0)
  def emod(a: A, b: A): A = zero
  def equot(a: A, b: A): A = div(a, b)
  override def equotmod(a: A, b: A): (A, A) = (div(a, b), zero)

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

object Field extends _root_.algebra.ring.FieldFunctions[Field] with EuclideanRingFunctions[Field] {
  @inline def apply[A](implicit ev: Field[A]): Field[A] = ev
}
