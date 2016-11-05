package spire
package algebra

trait GCDRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with CRing[A] {
  def gcd(a: A, b: A)(implicit ev: Eq[A]): A
  def lcm(a: A, b: A)(implicit ev: Eq[A]): A
}

object GCDRing {
  @inline final def apply[A](implicit ev: GCDRing[A]): GCDRing[A] = ev
}
