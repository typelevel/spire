package spire
package algebra

trait Field[@sp(Int, Long, Float, Double) A] extends Any with AlgebraField[A] with EuclideanRing[A] {
  def euclideanFunction(a: A): BigInt = BigInt(0)
  def quot(a: A, b: A): A = div(a, b)
  def mod(a: A, b: A): A = zero
  override def quotmod(a: A, b: A): (A, A) = (div(a, b), zero)
  override def gcd(a: A, b: A)(implicit eqA: Eq[A]): A =
    if (isZero(a) && isZero(b)) zero else one
  override def lcm(a: A, b: A)(implicit eqA: Eq[A]): A = times(a, b)
}

object Field extends _root_.algebra.ring.FieldFunctions[Field] with EuclideanRingFunctions[Field] {
  @inline def apply[A](implicit ev: Field[A]): Field[A] = ev
}
